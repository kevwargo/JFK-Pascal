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

  enum BUILTIN_TYPE
  {
      BYTE = 1,
      SHORTINT = 2,
      INTEGER = 3,
      WORD = 4,
      LONGINT = 5,
      REAL = 6,
      SINGLE = 7,
      DOUBLE = 8,
      EXTENDED = 9,
      COMP = 10,
      CHAR = 11,
      RECORD = 12,
      ARRAY = 13,
      WRONG_TYPE = 14
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
  

  void add_to_list(List **listptr, void *element);
  void add_var(VarList **varsptr, char *varname);
  /* void add_fields(Record **recordptr, VarList *fields); */
  void print_vars(VarList *vars);
  void clear_vars(VarList **varsptr);
  void clear_args(List **arglistptr);
  BUILTIN_TYPE type_parse(char *pascal_type);
  char *get_c_type(BUILTIN_TYPE type);
  void add_vars_to_globals(VarList *vars, Type *type);
  void add_global_symbol(char *name, Type *type);
  char *string_toupper(char *string);
  Type *get_var_type(char *name);
  Type *get_field_type(char *name, List *record);
  void check_const(Type *type, ConstExpr *expr);
  void check_int_const(BUILTIN_TYPE type, char *value);
  void check_char_const(BUILTIN_TYPE type, char *value);
  void check_real_const(BUILTIN_TYPE type, char *value);
  void check_record_const(Type *type, ConstExpr *expr);
  void print_const(SymbolEntry *symbol, ConstExpr *expr);
  void print_var_arg(char *name, VarOpt *vo, int if_scanf);
  void print_funcall(char *funcname, List *arglist);
  void print_type(Type *type);

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
    /* struct Record *record; */
    struct ConstExpr *const_expr;
    struct ConstField *const_field;
    int ival;
}
%token KWD_PROGRAM KWD_BEGIN KWD_END KWD_VAR KWD_CONST KWD_SHR KWD_SHL KWD_MOD KWD_DIV KWD_RECORD WRITE_FUNC WRITELN_FUNC READ_FUNC
%token <text> IDENT INT_CONST CHAR_CONST REAL_CONST STRING_LITERAL
%type <text> expression var
%type <argument> arg var_arg
%type <varlist> var_list var_decl field_decl
%type <list> arg_list const_fields record_body
%type <symbol> constant
%type <varopt> var_options
%type <type> type_rule
/* %type <record> record_body */
%type <const_expr> const_expr
%type <const_field> const_field
%%

Input : prog_header { printf("#include <stdio.h>\n"); }
        declarations
        KWD_BEGIN { printf("int main()\n{\n"); }
        statements
        KWD_END { printf("  return 0;\n}\n"); } '.'
      ;

var : IDENT { $$ = string_toupper($1); }
    | IDENT '.' IDENT {
        Type *type = get_var_type($1);
        if (! type)
        {
            printf("identifier %s", $1);
            yyerror(" undeclared");
        }
        if (type->type != RECORD)
        {
            printf("identifier %s ", $1);
            yyerror("is not of record type");
        }
        if (! get_field_type($3, type->u.record))
        {
            printf("element %s does not have field %s", $1, $3);
            yyerror("");
        }
        asprintf(&($$), "%s.%s",
                 string_toupper($1),
                 string_toupper($3));
        free($1);
        free($3);
      }
    ;

statements :
           | statements { printf("  "); } statement
           ;

statement : func_call
          | var ':' '=' expression ';' { printf("%s = %s;\n", string_toupper($1), $4); free($1); free($4); }
          ;

expression : var { $$ = string_toupper($1); }
           | INT_CONST { $$ = strdup($1); }
           | REAL_CONST { $$ = strdup($1); }
           | '(' expression ')' { char *s = NULL; asprintf(&s, "(%s)", $2); free($2); $$ = s; }
           | expression '*' expression {
               char *s = NULL;
               asprintf(&s, "%s * %s", $1, $3);
               free($1);
               free($3);
               $$ = s;
             }
           | expression '-' expression {
               char *s = NULL;
               asprintf(&s, "%s - %s", $1, $3);
               free($1);
               free($3);
               $$ = s;
             }
           | expression '+' expression {
               char *s = NULL;
               asprintf(&s, "%s + %s", $1, $3);
               free($1);
               free($3);
               $$ = s;
             }
           | expression '/' expression {
               char *s = NULL;
               asprintf(&s, "%s / %s", $1, $3);
               free($1);
               free($3);
               $$ = s;
             }
           | expression KWD_SHL expression {
               char *s = NULL;
               asprintf(&s, "%s << %s", $1, $3);
               free($1);
               free($3);
               $$ = s;
             }
           | expression KWD_SHR expression {
               char *s = NULL;
               asprintf(&s, "%s >> %s", $1, $3);
               free($1);
               free($3);
               $$ = s;
             }
           | expression KWD_MOD expression {
               char *s = NULL;
               asprintf(&s, "%s %% %s", $1, $3);
               free($1);
               free($3);
               $$ = s;
             }
           | expression KWD_DIV expression {
               char *s = NULL;
               asprintf(&s, "%s / %s", $1, $3);
               free($1);
               free($3);
               $$ = s;
             }
           ;

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
                       {
                           printf("identifier %s ", $1);
                           yyerror("not found");
                       }
                       $$ = (Argument *)malloc(sizeof(Argument));
                       $$->varname = $1;
                       $$->string = NULL;
                       $$->opt = $2;
                   }
        ;

var_options : { $$ = NULL; }
            | ':' INT_CONST ':' INT_CONST { $$ = (VarOpt *)malloc(sizeof(VarOpt)); $$->len = $2; $$->precision = $4; }
            ;

prog_header :
            | KWD_PROGRAM IDENT { printf("/* program : %s */\n", $2); } prog_args ';'
            ;

ident_list : IDENT
           | ident_list ',' IDENT
           ;

prog_args :
          | '(' ident_list ')'
          ;

declarations :
             | declarations global_vars
             | declarations global_constants
             ;

global_vars : KWD_VAR var_decl_list
            ;

var_decl_list : var_decl
              | var_decl_list var_decl
              ;

record_body : field_decl  ';' {
                   $$ = NULL;
                   add_to_list(&($$), $1);
              }
            | record_body
              field_decl ';' { add_to_list(&($1), $2); $$ = $1; }
            ;

field_decl : var_list ':' IDENT
             {
                 Type *type = (Type *)malloc(sizeof(Type));
                 type->type = type_parse($3);
                 $1->type = type;
                 $$ = $1;
             }
           ;

type_rule : IDENT
                {
                    $$ = (Type *)malloc(sizeof(Type));
                    $$->type = type_parse($1);
                }
          | KWD_RECORD record_body 
            KWD_END {
              $$ = (Type *)malloc(sizeof(Type));
              $$->type = RECORD;
              $$->u.record = $2;
            }
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

global_constants : KWD_CONST const_decls
                 ;

const_decls : const_decl
            | const_decls const_decl
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

const_decl : constant '=' const_expr ';'
             { check_const($1->type, $3); print_const($1, $3); }
           ;

constant : IDENT ':' type_rule
             {
                 $$ = (SymbolEntry *)malloc(sizeof(SymbolEntry));
                 add_global_symbol($1, $3);
                 $$->name = string_toupper(strdup($1));
                 $$->type = $3;
             }
           ;

%%

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
    if (! string)
        return NULL;
    while (string[i])
    {
        string[i] = toupper(string[i]);
        i++;
    }
    return string;
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
            if (int_in_bounds(value, -2147483648, 2147483647))
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
            printf("field %s ", field->name);
            yyerror("was not found in record");
        }
    }
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
    if (type->type != RECORD)
        printf("%s", get_c_type(type->type));
    else
    {
        int i;
        printf("struct\n{\n");
        for (i = 0; i < type->u.record->count; i++)
        {
            printf("  ");
            print_vars((VarList *)type->u.record->elements[i]);
        }
        printf("}");
    }
}

void print_const(SymbolEntry *symbol, ConstExpr *expr)
{
    print_type(symbol->type);
    if (symbol->type->type != RECORD)
    {
        printf(" %s = %s;\n", symbol->name, expr->value.string);
        free(symbol->name);
        free(symbol);
    }
    else
    {
        int i;
        int printed = 0;
        List *record = symbol->type->u.record;
        List *record_val = expr->value.record;
        printf(" %s = {", symbol->name);
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
    print_type(vars->type);
    putchar(' ');
    for (i = 0; i < vars->vars->count - 1; i++)
        printf("%s, ", string_toupper((char *)vars->vars->elements[i]));
    printf("%s;\n", string_toupper((char *)vars->vars->elements[i]));
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
            printf(",%s%s", function == READ_FUNC ? "&" : "",
                   string_toupper(((Argument *)arglist->elements[i])->varname));

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
    if (get_var_type(name) != NULL)
    {
        int i = 0;
        printf("identifier %s ", name);
        yyerror("redeclared");
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

BUILTIN_TYPE type_parse(char *pascal_type)
{
    if (get_var_type(pascal_type) != NULL) /* symbol with name typename was found */
    {
        yyerror("type identifier expected");
    }
    if (strcasecmp("BYTE", pascal_type) == 0)     return BYTE;
    if (strcasecmp("SHORTINT", pascal_type) == 0) return SHORTINT;
    if (strcasecmp("INTEGER", pascal_type) == 0)  return INTEGER;
    if (strcasecmp("WORD", pascal_type) == 0)     return WORD;
    if (strcasecmp("LONGINT", pascal_type) == 0)  return LONGINT;
    if (strcasecmp("REAL", pascal_type) == 0)     return REAL;
    if (strcasecmp("SINGLE", pascal_type) == 0)   return SINGLE;
    if (strcasecmp("DOUBLE", pascal_type) == 0)   return DOUBLE;
    if (strcasecmp("EXTENDED", pascal_type) == 0) return EXTENDED;
    if (strcasecmp("COMP", pascal_type) == 0)     return COMP;
    if (strcasecmp("CHAR", pascal_type) == 0)     return CHAR;
    return WRONG_TYPE;
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

void yyerror(const char *fmt, ...)
{
  printf("%s in line %d\n", fmt, yylineno);
  exit(-1);
}

int main()
{
    memset(&GlobalSymbolTable, 0, sizeof(SymbolTable));
    return yyparse();
}
