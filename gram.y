%{
  int yylex(void);
  void yyerror(const char *,...);
  int yyparse(void);
  extern int yylineno;
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>

  enum BUILTIN_TYPE
  {
      BYTE,
      SHORTINT,
      INTEGER,
      WORD,
      LONGINT,
      REAL,
      SINGLE,
      DOUBLE,
      EXTENDED,
      COMP,
      CHAR,
      RECORD,
      ARRAY,
      WRONG_TYPE
  };
  typedef enum BUILTIN_TYPE BUILTIN_TYPE;

  enum
  {
      TYPE_ERROR_OUTRANGE,
      TYPE_ERROR_INCOMPATIBLE
  };


  typedef struct Array Array;

  typedef struct List
  {
      int size;
      int count;
      void **elements;
  } List;

  typedef struct Type
  {
      BUILTIN_TYPE type;
      union
      {
          List *record;
          Array *array;
      } u;
  } Type;

  typedef struct VarList
  {
      Type *type;
      List *vars;
  } VarList;

  typedef struct Variable
  {
      char *name;
      char *type;
  } Variable;

  typedef struct SymbolEntry
  {
      char *name;
      Type *type;
  } SymbolEntry;

  typedef struct SymbolTable
  {
      SymbolEntry *entries;
      int size;
      int count;
  } SymbolTable;

  typedef struct VarOpt
  {
      char *len;
      char *precision;
  } VarOpt;

  typedef struct Argument
  {
      char *string;
      char *varname;
      VarOpt *opt;
  } Argument;

  typedef struct ConstExpr
  {
      int type;
      union
      {
          char *string;
          List *record;
      } value;
  } ConstExpr;

  typedef struct ConstField
  {
      char *name;
      ConstExpr *value;
  } ConstField;

  struct Array
  {
      int bounds_type;
      int low;
      int high;
      Type *type;
  };

  /* add functions */
  void add_to_list(List **listptr, void *element);
  void add_var(VarList **varsptr, char *varname);
  void add_global_symbol(char *name, Type *type);
  void add_vars_to_globals(VarList *vars, Type *type);

  /* clear functions */
  void clear_vars(VarList **varsptr);
  void clear_args(List **arglistptr);
  void clear_list(List **listptr);

  /* print functions */
  void print_vars(VarList *vars);
  void print_toupper(char *string);
  void print_const(char *symbol, Type *type, ConstExpr *expr);
  void print_funcall(char *funcname, List *arglist);
  void print_type(Type *type);

  /* convert functions */
  Type *type_parse(char *pascal_type);
  char *get_c_type(BUILTIN_TYPE type);
  char *string_toupper(char *string);
  char *make_expression(char *expr1, char *op, char *expr2);

  /* lookup functions */
  Type *get_var_type(char *name);
  Type *get_field_type(char *name, List *record);

  /* check functions */
  void check_const(Type *type, ConstExpr *expr);
  char *check_record_member(char *record, char *member);
  Type *check_and_print_array_index(char *symbol, List *index_list);
  Type *check_and_print_type_array_index(Type *type, List *index_list, char *symbol);

  /* array declaration helpers */
  Type *set_array_subtype(Type **arrayptr, Type *subtype);
  void set_low_array_bound(Type **arrayptr, char *value);
  void set_high_array_bound(Type **arrayptr, char *value);

  List GlobalSymbolTable;

%}
%union
{
    char *text;
    struct VarList *varlist;
    struct SymbolEntry *symbol;
    struct VarOpt *varopt;
    struct Argument *argument;
    struct List *list;
    struct Type *type;
    struct ConstExpr *const_expr;
    struct ConstField *const_field;
    struct Array *array;
    int ival;
}

%left '*'
%left '+'
%left '-'
%left '/'
%left KWD_SHL
%left KWD_SHR
%left KWD_MOD
%left KWD_DIV

%token KWD_PROGRAM KWD_BEGIN KWD_END KWD_VAR KWD_CONST KWD_SHR KWD_SHL KWD_MOD KWD_DIV KWD_RECORD KWD_ARRAY KWD_OF WRITE_FUNC WRITELN_FUNC READ_FUNC
%token <text> IDENT INT_CONST CHAR_CONST REAL_CONST STRING_LITERAL
%type <text> expression var_expr
%type <argument> arg var_arg
%type <varlist> var_list var_decl field_decl
%type <list> arg_list const_fields record_body array_index
%type <varopt> var_options
%type <type> type_rule array_decl array_bounds array_bounds_decl array_multi_bounds array_low_int_bound array_low_char_bound array_head
%type <const_expr> const_expr
%type <const_field> const_field
%type <symbol> array_element
%%

/* START */

Input : prog_header { printf("#include <stdio.h>\n"); }
        declarations
        KWD_BEGIN { printf("int main()\n{\n"); }
        statements
        KWD_END { printf("  return 0;\n}\n"); } '.'
      ;


/* PROGRAM HEADER */

prog_header :
            | KWD_PROGRAM IDENT { printf("/* program : %s */\n", $2); } prog_args ';'
            ;

ident_list : IDENT
           | ident_list ',' IDENT
           ;

prog_args :
          | '(' ident_list ')'
          ;


/* DECLARATIONS */

declarations :
             | declarations global_vars
             | declarations global_constants
             ;

type_rule : IDENT { $$ = type_parse($1); }
          | KWD_RECORD record_body
            KWD_END {
              $$ = (Type *)malloc(sizeof(Type));
              $$->type = RECORD;
              $$->u.record = $2;
            }
          | array_decl { $$ = $1; }
          ;

/* RECORD TYPE DECLARATION */

record_body : field_decl  ';' {
                   $$ = NULL;
                   add_to_list(&($$), $1);
              }
            | record_body
              field_decl ';' { add_to_list(&($1), $2); $$ = $1; }
            ;

field_decl : var_list ':' IDENT
             {
                 Type *type = type_parse($3);
                 $1->type = type;
                 $$ = $1;
             }
           ;


/* ARRAY TYPE DECLARATION */

array_low_char_bound : CHAR_CONST '.' '.' { $$ = NULL; set_low_array_bound(&($$), $1); }
                      ;

array_low_int_bound : INT_CONST '.' '.' { $$ = NULL; set_low_array_bound(&($$), $1); }
                    | REAL_CONST '.' {
                        if ($1[strlen($1) - 1] != '.') yyerror("syntax error in array declaration");
                        $$ = NULL;
                        set_low_array_bound(&($$), $1);
                      }
                    ;

array_bounds : array_low_int_bound INT_CONST { set_high_array_bound(&($1), $2); $$ = $1; }
             | array_low_char_bound CHAR_CONST { set_high_array_bound(&($1), $2); $$ = $1; }
             ;

array_multi_bounds : { $$ = NULL; }
                   | array_multi_bounds array_bounds ',' {
                       $$ = set_array_subtype(&($1), $2);
                     }
                   ;

array_bounds_decl : '[' array_multi_bounds array_low_int_bound REAL_CONST ')' {
                            set_high_array_bound(&($3), $4);
                            $$ = set_array_subtype(&($2), $3);
                        }
                  | '[' array_multi_bounds array_bounds ']' {
                            $$ = set_array_subtype(&($2), $3);
                        }
                  ;

array_head : KWD_ARRAY array_bounds_decl KWD_OF { $$ = $2; }
           ;

array_decl : array_head IDENT {
                          $$ = set_array_subtype(&($1), type_parse($2));
                        }
           | array_head array_decl { set_array_subtype(&($1), $2); $$ = $1; }
           ;


/* VARIABLES DECLARATION */

global_vars : KWD_VAR var_decl_list
            ;

var_decl_list : var_decl
              | var_decl_list var_decl
              ;

var_decl : var_list ':' type_rule ';'
           {
               add_vars_to_globals($1, $3);
               $1->type = $3;
               print_vars($1);
               clear_vars(&($1));
           }
         ;

var_list : IDENT { $$ = NULL; add_var(&($$), $1);  }
         | var_list ',' IDENT { add_var(&($1), $3); $$ = $1; }
         ;


/* CONSTANTS DECLARATION */

global_constants : KWD_CONST const_decls
                 ;

const_decls : const_decl
            | const_decls const_decl
            ;

const_decl : IDENT ':' type_rule '=' const_expr ';'
             {
                 add_global_symbol($1, $3);
                 check_const($3, $5);
                 print_const($1, $3, $5);
             }
           ;

const_field : IDENT ':' const_expr {
                       ConstField *field;
                       if ($3->type == RECORD)
                           yyerror("nested records are not allowed");
                       field = (ConstField *)malloc(sizeof(ConstField));
                       field->name = $1;
                       field->value = $3;
                       $$ = field;
                   }
            ;

const_fields : const_field { $$ = NULL; add_to_list(&($$), $1); }
             | const_fields ';' const_field { add_to_list(&($1), $3); }
             ;

const_expr : CHAR_CONST {
               $$ = (ConstExpr *)malloc(sizeof(ConstExpr));
               $$->type = CHAR_CONST;
               $$->value.string = $1;
           }
        | INT_CONST {
            $$ = (ConstExpr *)malloc(sizeof(ConstExpr));
            $$->type = INT_CONST;
            $$->value.string = $1;
          }
        | REAL_CONST {
            $$ = (ConstExpr *)malloc(sizeof(ConstExpr));
            $$->type = REAL_CONST;
            $$->value.string = $1;
          }
        | '(' const_fields ')' {
                   $$ = (ConstExpr *)malloc(sizeof(ConstExpr));
                   $$->type = RECORD;
                   $$->value.record = $2;
               }
        ;


/* STATEMENTS DECLARATIONS */

statements :
           | statements { printf("  "); } statement
           ;

statement : func_call
          | var_expr ':' '=' expression ';' {
                    print_toupper($1);
                    printf(" = %s;\n", $4);
                    free($1);
                    free($4);
                 }
          ;


/* EXPRESSION DECLARATIONS */

expression : var_expr { $$ = string_toupper(strdup($1)); }
           | INT_CONST
           | REAL_CONST
           | CHAR_CONST
           | '(' expression ')' {
                     char *s = NULL;
                     asprintf(&s, "(%s)", $2);
                     free($2);
                     $$ = s;
                 }
           | expression '*' expression {
               $$ = make_expression($1, "*", $3);
             }
           | expression '-' expression {
               $$ = make_expression($1, "-", $3);
             }
           | expression '+' expression {
               $$ = make_expression($1, "+", $3);
             }
           | expression '/' expression {
               $$ = make_expression($1, "/", $3);
             }
           | expression KWD_SHL expression {
               $$ = make_expression($1, "<<", $3);
             }
           | expression KWD_SHR expression {
               $$ = make_expression($1, ">>", $3);
             }
           | expression KWD_MOD expression {
               $$ = make_expression($1, "%", $3);
             }
           | expression KWD_DIV expression {
               $$ = make_expression($1, "/", $3);
             }
           ;


/* FUNCTION CALL DECLARATION */

func_call : IDENT '(' arg_list ')' ';'
            {
                print_funcall($1, $3);
                clear_args(&($3));
                free($1);
            }
          ;

arg_list : arg { $$ = NULL; add_to_list(&($$), $1); }
         | arg_list ',' arg { add_to_list(&($1), $3); $$ = $1; }
         ;

arg : STRING_LITERAL {
            $$ = (Argument *)malloc(sizeof(Argument));
            $$->string = $1;
            $$->varname = NULL;
            $$->opt = NULL;
        }
    | var_arg { $$ = $1; }
    ;

var_arg : IDENT var_options
                   {
                       if (! get_var_type($1))
                           yyerror("identifier %s not found", $1);
                       $$ = (Argument *)malloc(sizeof(Argument));
                       $$->varname = $1;
                       $$->string = NULL;
                       $$->opt = $2;
                   }
        ;

var_options : { $$ = NULL; }
            | ':' INT_CONST ':' INT_CONST { $$ = (VarOpt *)malloc(sizeof(VarOpt)); $$->len = $2; $$->precision = $4; }
            ;


/* VARIABLE EXPRESSION DECLARATION */

var_expr : IDENT
    | IDENT '.' IDENT { $$ = check_record_member($1, $3); free($1); free($3); }
    | array_element { $$ = strdup(""); free($1->name); free($1); }
    ;

array_index : INT_CONST { $$ = NULL; add_to_list(&($$), $1); }
            | CHAR_CONST { $$ = NULL; add_to_list(&($$), $1); }
            | array_index ',' INT_CONST { add_to_list(&($1), $3); $$ = $1; }
            | array_index ',' CHAR_CONST { add_to_list(&($1), $3); $$ = $1; }
            ;

array_element : IDENT '[' array_index ']' {
                               $$ = (SymbolEntry *)malloc(sizeof(SymbolEntry));
                               $$->type = check_and_print_array_index($1, $3);
                               $$->name = $1;
                               clear_list(&($3));
                           }
              | array_element '[' array_index ']' {
                  $$ = $1;
                  $$->type = check_and_print_type_array_index($1->type, $3, $1->name);
                  clear_list(&($3));
                }
              ;


/* END OF RULES */

%%

/* Different helper functions */

void create_array_if_null(Type **typeptr)
{
    Type *type;
    if (*typeptr)
        return;
    type = (Type *)malloc(sizeof(Type));
    type->u.array = (Array *)malloc(sizeof(Array));
    type->type = ARRAY;
    type->u.array->type = NULL;
    *typeptr = type;
}

void set_array_bound(Type **arrayptr, int bound, char *value)
{
    Array *array;
    int *boundptr;
    if (! arrayptr)
        return;
    create_array_if_null(arrayptr);
    array = (*arrayptr)->u.array;
    if (bound == 1)
        boundptr = &(array->high);
    else
        boundptr = &(array->low);
    if (*value == '\'')
    {
        array->bounds_type = CHAR_CONST;
        *boundptr = value[1];
    }
    else
    {
        if (! int_in_bounds(value, -2147483648, 2147483647))
            yyerror("array index %s out of range", value);
        array->bounds_type = INT_CONST;
        *boundptr = atoi(value);
    }
}

int int_in_bounds(char *value, long int low, long int high)
{
    long int num = strtol(value, NULL, 10);
    if (errno == ERANGE)
        return 0;
    if (num < low)
        return 0;
    if (num > high)
        return 0;
    return 1;
}

int int_in_32bit_bounds(char *value)
{
    return int_in_bounds(value, -2147483648, 2147483647);
}

int parse_float(char *number, char *significand, long int *exponent)
{
    char *whole;
    char *frac;
    long int e = 0;
    int non_zero_pos, dot_pos, e_pos;
    *significand = '\0';
    if (*number == '+')
        number++;
    else if (*number == '-')
    {
        strcpy(significand, "-");
        number++;
    }
    non_zero_pos = strspn(number, "0");
    dot_pos = strcspn(number, ".");
    e_pos = strcspn(number, "eE");
    number = strdup(number);
    whole = number + non_zero_pos;
    if (! isdigit(*whole))
        whole = NULL;
    frac = number + dot_pos;
    if (! *frac || e_pos == dot_pos + 1)
        frac = NULL;
    else
        frac++;
    if (number[e_pos])
    {
        e = strtol(number + e_pos + 1, NULL, 10);
        if (errno == ERANGE)
        {
            if (number[e_pos + 1] == '-')
            {
                strcpy(significand, "0");
                *exponent = 0;
            }
            else
                return 0;
        }
    }
    number[dot_pos] = '\0';
    number[e_pos] = '\0';

    if (frac)
    {
        non_zero_pos = strspn(frac, "0");
        if (! frac[non_zero_pos])
            frac = NULL;
        else
        {
            int i = strlen(frac) - 1;
            while (frac[i] == '0')
                frac[i--] = '\0';
        }
    }

    if (whole)
    {
        e += strlen(whole);
        strcat(significand, whole);
    }
    else if (frac)
    {
        non_zero_pos = strspn(frac, "0");
        if (non_zero_pos)
        {
            e -= non_zero_pos;
            frac += non_zero_pos;
        }
    }
    if (frac)
        strcat(significand, frac);
    if (! frac && ! whole)
        strcat(significand, "0");
    *exponent = e;
    free(number);
    return 1;
}

int float_cmp(char *n1, char *n2)
{
    char *n1_significand = strdup(n1);
    char *n2_significand = strdup(n2);
    long int n1_exp;
    long int n2_exp;
    int res1 = parse_float(n1, n1_significand, &n1_exp);
    int res2 = parse_float(n2, n2_significand, &n2_exp);
    int result;

    if (! res1 && ! res2)
        result = 0;
    else if (! res1)
        result = 1;
    else if (! res2)
        result = -1;
    else if (n1_exp > n2_exp)
        result = 1;
    else if (n1_exp < n2_exp)
        result = -1;
    else
        result = strcmp(n1_significand, n2_significand);

    if (*n1 == '-' && *n2 != '-')
        result = -1;
    else if (*n1 != '-' && *n2 == '-')
        result = 1;
    else if (*n1 == '-' && *n2 == '-')
        result = -result;
    free(n1_significand);
    free(n2_significand);
    return result;
}

int float_in_bounds(char *number, char *low, char *high)
{
    if (float_cmp(number, low) < 0)
        return 0;
    if (float_cmp(number, high) > 0)
        return 0;
    return 1;
}

void check_real_const(BUILTIN_TYPE type, char *value)
{
    switch (type)
    {
        case CHAR:
            yyerror("incompatible types 'Char' and 'Extended'");
            break;
        case RECORD:
            yyerror("incompatible types 'Record' and 'Extended'");
            break;
        case BYTE: case SHORTINT: case INTEGER: case WORD: case LONGINT: case COMP:
            yyerror("incompatible types 'Integer' and 'Extended'");
            break;
        case REAL:
            if (float_in_bounds(value, "-2.9e39", "1.7e38"))
                return;
            break;
        case SINGLE:
            if (float_in_bounds(value, "-1.5e45", "3.4e38"))
                return;
            break;
        case DOUBLE:
            if (float_in_bounds(value, "-5e324", "1.7e308"))
                return;
            break;
        case EXTENDED:
            if (float_in_bounds(value, "-3.6e4951", "1.1e4932"))
                return;
            break;
    }
    yyerror("constant expression violates subrange bounds");
}

void check_int_const(BUILTIN_TYPE type, char *value)
{
    switch (type)
    {
        case CHAR:
            yyerror("incompatible types 'Char' and 'Integer'");
            break;
        case RECORD:
            yyerror("incompatible types 'Record' and 'Integer'");
            break;
        case BYTE:
            if (int_in_bounds(value, 0, 255))
                return;
            break;
        case SHORTINT:
            if (int_in_bounds(value, -128, 127))
                return;
            break;
        case INTEGER: case LONGINT:
            if (int_in_32bit_bounds(value))
                return;
            break;
        case WORD:
            if (int_in_bounds(value, 0, 65535))
                return;
            break;
        case COMP:
            if (float_in_bounds(value, "-9223372036854775808", "9223372036854775807"))
                return;
            break;
        default:
            check_real_const(type, value);
            return;
    }
    yyerror("constant expression violates subrange bounds");
}

void check_char_const(BUILTIN_TYPE type, char *value)
{
    switch (type)
    {
        case CHAR:
            return;
        case BYTE: case SHORTINT: case INTEGER: case WORD: case LONGINT:
            yyerror("incompatible types 'Integer' and 'Char'");
            break;
        case RECORD:
            yyerror("incompatible types 'Record' and 'Char'");
            break;
        default:
            yyerror("incompatible types 'Extended' and 'Char'");
            break;
    }
}

void check_record_const(Type *type, ConstExpr *expr)
{
    int i;
    List *record_decl;
    List *record_val = expr->value.record;
    if (type->type != RECORD)
        yyerror("given type is not record");
    record_decl = type->u.record;
    for (i = 0; i < record_val->count; i++)
    {
        ConstField *field = (ConstField *)record_val->elements[i];
        if (! get_field_type(field->name, record_decl))
        {
            yyerror("field %s was not found in record", field->name);
        }
    }
}

void print_var_arg(char *name, VarOpt *vo, int if_scanf)
{
    printf("%%");
    if (vo)
    {
        printf("%s.%s", vo->len, vo->precision);
        free(vo->len);
        free(vo->precision);
        free(vo);
    }
    switch (get_var_type(name)->type)
    {
        case CHAR:
            printf("c");
            break;
        case BYTE: case SHORTINT: case INTEGER:
            printf("d");
            break;
        case WRONG_TYPE:
            yyerror("identifier not found");
            break;
        case DOUBLE:
            printf("lf");
            break;
        case EXTENDED:
            printf("Le");
            break;
        default:
            if (if_scanf)
                printf("lf");
            else
                printf("f");
            break;
    }
}





char *check_record_member(char *record, char *member)
{
    char *result;
    Type *type = get_var_type(record);
    if (! type)
        yyerror("identifier %s undeclared", record);
    if (type->type != RECORD)
        yyerror("identifier %s is not of record type", record);
    if (! get_field_type(member, type->u.record))
        yyerror("element %s does not have field %s", record, member);
    asprintf(&result, "%s.%s", record, member);
    return result;
}

Type *check_and_print_type_array_index(Type *type, List *index_list, char *symbol)
{
    int i;
    for (i = 0; i < index_list->count; i++)
    {
        char *index_str = (char *)index_list->elements[i];
        int index;
        Array *array;
        if (type->type != ARRAY)
        {
            if (i == 0)
                yyerror("variable %s is not of array type", symbol);
            else
                yyerror("too many dimensions for array %s", symbol);
        }
        array = type->u.array;
        if (array->bounds_type == CHAR_CONST && index_str[0] != '\'')
            yyerror("array %s is indexed with chars, not integers", symbol);
        if (array->bounds_type == INT_CONST && index_str[0] == '\'')
            yyerror("array %s is indexed with integers, not chars", symbol);
        if (array->bounds_type == INT_CONST)
            index = atoi(index_str);
        else
            index = index_str[1];
        if (! int_in_32bit_bounds(index_str) ||
            index > array->high ||
            index < array->low)
            yyerror("array %s index out of bounds %d %d %d", symbol, index, array->low, array->high);
        if (array->bounds_type == INT_CONST)
            printf("[%d - (%d)]", index, array->low);
        else
            printf("['%c' - ('%c')]", index, array->low);
        type = array->type;
    }
    return type;
}

Type *check_and_print_array_index(char *symbol, List *index)
{
    Type *type;
    type = get_var_type(symbol);
    if (! type)
        yyerror("symbol %s not found", symbol);
    print_toupper(symbol);
    return check_and_print_type_array_index(type, index, symbol);
}

void add_to_list(List **listptr, void *element)
{
    if (! listptr)
        return;
    if (! *listptr)
    {
        *listptr = (List *)malloc(sizeof(List));
        (*listptr)->size = 8;
        (*listptr)->count = 0;
        (*listptr)->elements = (void **)malloc(sizeof(void *) * (*listptr)->size);
    }
    if ((*listptr)->count == (*listptr)->size)
    {
        (*listptr)->size += 8;
        (*listptr)->elements = (void **)realloc((*listptr)->elements,
                                                sizeof(void *) * (*listptr)->size);
    }
    (*listptr)->elements[(*listptr)->count] = element;
    (*listptr)->count++;
}

void clear_list(List **listptr)
{
    int i;
    if (! listptr || ! *listptr)
        return;
    for (i = 0; i < (*listptr)->count; i++)
        free((*listptr)->elements[i]);
    free(*listptr);
    *listptr = NULL;
}

char *string_toupper(char *string)
{
    int i = 0;
    while (string[i])
    {
        string[i] = toupper(string[i]);
        i++;
    }
    return string;
}

void print_toupper(char *string)
{
    char *upper = string_toupper(strdup(string));
    printf("%s", upper);
    free(upper);
}

void check_const(Type *type, ConstExpr *expr)
{
    switch (expr->type)
    {
        case CHAR_CONST:
            check_char_const(type->type, expr->value.string);
            break;
        case INT_CONST:
            check_int_const(type->type, expr->value.string);
            break;
        case REAL_CONST:
            check_real_const(type->type, expr->value.string);
            break;
        case RECORD:
            check_record_const(type, expr);
            break;
    }

}

Type *get_field_type(char *name, List *record)
{
    int i;
    for (i = 0; i < record->count; i++)
    {
        int j;
        VarList *varlist = ((VarList *)record->elements[i]);
        Type *type = varlist->type;
        for (j = 0; j < varlist->vars->count; j++)
            if (strcasecmp(name, ((char *)varlist->vars->elements[j])) == 0)
                return type;
    }
    return NULL;
}


void add_var(VarList **varsptr, char *varname)
{
    if (! varsptr || ! varname)
        return;
    if (! *varsptr)
    {
        *varsptr = (VarList *)malloc(sizeof(VarList));
        (*varsptr)->vars = NULL;
    }
    add_to_list(&((*varsptr)->vars), varname);
}


void print_type(Type *type)
{
    switch (type->type)
    {
        int i;
        case RECORD:
            printf("struct\n{\n");
            for (i = 0; i < type->u.record->count; i++)
            {
                printf("  ");
                print_vars((VarList *)type->u.record->elements[i]);
            }
            printf("}");
            break;
        case ARRAY:
            break;
        default:
            printf("%s", get_c_type(type->type));
            break;
    }
}

void print_const(char *symbol, Type *type, ConstExpr *expr)
{
    print_type(type);
    if (type->type != RECORD)
    {
        printf(" ");
        print_toupper(symbol);
        printf(" = %s;\n", expr->value.string);
    }
    else
    {
        int i;
        int printed = 0;
        List *record = type->u.record;
        List *record_val = expr->value.record;
        printf(" ");
        print_toupper(symbol);
        printf(" = {");
        for (i = 0; i < record->count; i++)
        {
            int k;
            VarList *varlist = (VarList *)record->elements[i];
            for (k = 0; k < varlist->vars->count; k++)
            {
                int j;
                for (j = 0; j < record_val->count; j++)
                {
                    if (strcasecmp(((ConstField *)record_val->elements[j])->name,
                                   (char *)varlist->vars->elements[k]) == 0)
                    {
                        if (printed)
                            putchar(',');
                        printf(" %s", ((ConstField *)record_val->elements[j])->value->value.string);
                        printed++;
                    }
                }
            }
        }
        printf(" };\n");
    }
}

void print_vars(VarList *vars)
{
    int i;
    if (vars->type->type != ARRAY)
    {
        print_type(vars->type);
        putchar(' ');
        for (i = 0; i < vars->vars->count - 1; i++)
        {
            print_toupper((char *)vars->vars->elements[i]);
            printf(", ");
        }
        print_toupper((char *)vars->vars->elements[i]);
        printf(";\n");
    }
    else
    {
        Type *btype;
        Type *type = vars->type;
        btype = type->u.array->type;
        while (btype->type == ARRAY)
        {
            btype = btype->u.array->type;
        }
        print_type(btype);
        putchar(' ');
        for (i = 0; i < vars->vars->count - 1; i++)
        {
            print_toupper((char *)vars->vars->elements[i]);
            btype = type;
            while (btype->type == ARRAY)
            {
                if (btype->u.array->bounds_type == CHAR_CONST)
                    printf("['%c' - ('%c') + 1]", btype->u.array->high, btype->u.array->low);
                else
                    printf("[%d - (%d) + 1]", btype->u.array->high, btype->u.array->low);
                btype = btype->u.array->type;
            }
            printf(", ");
        }
        print_toupper((char *)vars->vars->elements[i]);
        btype = type;
        while (btype->type == ARRAY)
        {
            if (btype->u.array->bounds_type == CHAR_CONST)
                printf("['%c' - ('%c') + 1]", btype->u.array->high, btype->u.array->low);
            else
                    printf("[%d - (%d) + 1]", btype->u.array->high, btype->u.array->low);
            btype = btype->u.array->type;
        }
        printf(";\n");
    }
}

void print_funcall(char *funcname, List *arglist)
{
    int i;
    int function;

    if (strcasecmp(funcname, "write") == 0)
    {
        function = WRITE_FUNC;
        printf("printf(\"");
    }
    else if (strcasecmp(funcname, "writeln") == 0)
    {
        function = WRITELN_FUNC;
        printf("printf(\"");
    }
    else if (strcasecmp(funcname, "read") == 0)
    {
        function = READ_FUNC;
        printf("scanf(\"");
    }

    for (i = 0; i < arglist->count; i++)
        if (((Argument *)arglist->elements[i])->string)
            printf("%s", ((Argument *)arglist->elements[i])->string);
        else if (((Argument *)arglist->elements[i])->varname)
            print_var_arg(((Argument *)arglist->elements[i])->varname,
                          ((Argument *)arglist->elements[i])->opt,
                          function == READ_FUNC);

    if (function == WRITELN_FUNC)
        printf("\\n");
    printf("\"");

    for (i = 0; i < arglist->count; i++)
        if (((Argument *)arglist->elements[i])->varname)
        {
            printf(",%s", function == READ_FUNC ? "&" : "");
            print_toupper(((Argument *)arglist->elements[i])->varname);
        }

    printf(");\n");
}

void clear_vars(VarList **varsptr)
{
    if (! varsptr || ! *varsptr)
        return;
    clear_list(&((*varsptr)->vars));
    free(*varsptr);
    *varsptr = NULL;
}

void clear_args(List **arglistptr)
{
    int i;
    if (! arglistptr || ! *arglistptr)
        return;
    for (i = 0; i < (*arglistptr)->count; i++)
    {
        free(((Argument *)(*arglistptr)->elements[i])->varname);
        free(((Argument *)(*arglistptr)->elements[i])->string);
        free(((Argument *)(*arglistptr)->elements[i])->opt);
    }
    clear_list(arglistptr);
}

void add_global_symbol(char *name, Type *type)
{
    List *globalSymbolTablePtr = &GlobalSymbolTable;
    SymbolEntry *entry;
    Type *type_ = get_var_type(name);
    if (type_ != NULL)
    {
        yyerror("identifier %s redeclared", name);
    }
    entry = (SymbolEntry *)malloc(sizeof(SymbolEntry));
    entry->name = name;
    entry->type = type;
    add_to_list(&globalSymbolTablePtr, entry);
}

void add_vars_to_globals(VarList *vars, Type *type)
{
    int i;
    for (i = 0; i < vars->vars->count; i++)
        add_global_symbol(strdup((char *)vars->vars->elements[i]), type);
}

Type *get_var_type(char *name)
{
    int i;
    for (i = 0; i < GlobalSymbolTable.count; i++)
        if (strcasecmp(name, ((SymbolEntry *)GlobalSymbolTable.elements[i])->name) == 0)
        {
            return ((SymbolEntry *)GlobalSymbolTable.elements[i])->type;
        }
    return NULL;
}

Type *type_parse(char *pascal_type)
{
    Type *type;
    BUILTIN_TYPE btype;
    if (get_var_type(pascal_type) != NULL) /* symbol with name typename was found */
    {
        yyerror("type identifier expected");
    }
    if (strcasecmp("BYTE", pascal_type) == 0)          btype = BYTE;
    else if (strcasecmp("SHORTINT", pascal_type) == 0) btype = SHORTINT;
    else if (strcasecmp("INTEGER", pascal_type) == 0)  btype = INTEGER;
    else if (strcasecmp("WORD", pascal_type) == 0)     btype = WORD;
    else if (strcasecmp("LONGINT", pascal_type) == 0)  btype = LONGINT;
    else if (strcasecmp("REAL", pascal_type) == 0)     btype = REAL;
    else if (strcasecmp("SINGLE", pascal_type) == 0)   btype = SINGLE;
    else if (strcasecmp("DOUBLE", pascal_type) == 0)   btype = DOUBLE;
    else if (strcasecmp("EXTENDED", pascal_type) == 0) btype = EXTENDED;
    else if (strcasecmp("COMP", pascal_type) == 0)     btype = COMP;
    else if (strcasecmp("CHAR", pascal_type) == 0)     btype = CHAR;
    else yyerror("wrong type identifier");
    type = (Type *)malloc(sizeof(Type));
    type->type = btype;
    return type;
}

char *get_c_type(BUILTIN_TYPE type)
{
    switch (type)
    {
        case BYTE:
            return "unsigned char";
        case SHORTINT:
            return "signed char";
        case INTEGER:
            return "int";
        case WORD:
            return "unsigned int";
        case LONGINT:
            return "long int";
        case DOUBLE: case REAL:
            return "double";
        case SINGLE:
            return "float";
        case EXTENDED: case COMP:
            return "long double";
        case CHAR:
            return "char";
    }
    return NULL;
}


char *make_expression(char *expr1, char *op, char *expr2)
{
    char *result;
    asprintf(&result, "%s %s %s", expr1, op, expr2);
    free(expr1);
    free(expr2);
    return result;
}


/* Array declaration helpers */

Type *set_array_subtype(Type **arrayptr, Type *subtype)
{
    Type *array;
    if (! *arrayptr)
        return subtype;
    create_array_if_null(arrayptr);
    array = *arrayptr;
    while (array->u.array->type != NULL)
    {
        array = array->u.array->type;
    }
    array->u.array->type = subtype;
    return *arrayptr;
}

void set_low_array_bound(Type **arrayptr, char *value)
{
    set_array_bound(arrayptr, 0, value);
}

void set_high_array_bound(Type **arrayptr, char *value)
{
    set_array_bound(arrayptr, 1, value);
}


void yyerror(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
    printf(" in line %d\n", yylineno);
    exit(-1);
}

int main()
{
    memset(&GlobalSymbolTable, 0, sizeof(SymbolTable));
    return yyparse();
}
