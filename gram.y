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
      WRONG_TYPE = 13
  };
  typedef enum BUILTIN_TYPE BUILTIN_TYPE;

  enum
  {
      TYPE_ERROR_OUTRANGE,
      TYPE_ERROR_INCOMPATIBLE
  };

  struct VarList;

  struct Record
  {
      int size;
      int count;
      struct VarList *varlists;
  };
  typedef struct Record Record;
  
  struct Type
  {
      BUILTIN_TYPE type;
      Record record;
  };
  typedef struct Type Type;

  struct VarList
  {
      int size;
      int count;
      Type *type;
      char **varnames;
  };
  typedef struct VarList VarList;

  struct Variable
  {
      char *name;
      char *type;
  };
  typedef struct Variable Variable;

  struct SymbolEntry
  {
      char *name;
      Type *type;
  };
  typedef struct SymbolEntry SymbolEntry;

  struct SymbolTable
  {
      SymbolEntry *entries;
      int size;
      int count;
  };
  typedef struct SymbolTable SymbolTable;

  struct VarOpt
  {
      int len;
      int precision;
  };
  typedef struct VarOpt VarOpt;

  struct Argument
  {
      char *string;
      char *varname;
      VarOpt *opt;
  };
  typedef struct Argument Argument;

  struct ArgumentList
  {
      int count;
      int size;
      Argument *args;
  };
  typedef struct ArgumentList ArgumentList;

  struct Literal
  {
      yytokentype type;
      char *value;
  };
  typedef struct Literal Literal;
  

  void add_var(VarList **varsptr, char *varname);
  void add_arg(ArgumentList **arglistptr, Argument *arg);
  void add_fields(Record **recordptr, VarList *fields);
  void print_vars(VarList *vars);
  void clear_vars(VarList **varsptr);
  void clear_args(ArgumentList **arglistptr);
  BUILTIN_TYPE type_parse(char *pascal_type);
  char *get_c_type(BUILTIN_TYPE type);
  char *add_vars_to_globals(VarList *vars, Type *type);
  char *string_toupper(char *string);
  Type *get_var_type(char *name);
  void process_var_decl(VarList *vars, Type *type);
  void check_int_const(BUILTIN_TYPE type, char *value);
  void check_char_const(BUILTIN_TYPE type, char *value);
  void check_real_const(BUILTIN_TYPE type, char *value);
  void print_const(SymbolEntry *symbol, char *value);
  void print_var_arg(char *name, VarOpt *vo, int if_scanf);
  void print_funcall(char *funcname, ArgumentList *arglist);
  void print_type(Type *type);

  SymbolTable GlobalSymbolTable;

%}
%union
{
    char *text;
    struct VarList *vars;
    struct SymbolEntry *symbol;
    struct VarOpt *varopt;
    struct Argument *argument;
    struct ArgumentList *arglist;
    struct Type *type;
    struct Record *record;
    int ival;
}
%token KWD_PROGRAM KWD_BEGIN KWD_END KWD_VAR KWD_CONST KWD_SHR KWD_SHL KWD_MOD KWD_DIV KWD_RECORD WRITE_FUNC WRITELN_FUNC READ_FUNC
%token <text> IDENT INT_CONST CHAR_CONST REAL_CONST STRING_LITERAL
%type <text> expression
%type <argument> arg var_arg
%type <vars> var_list var_decl field_decl
%type <arglist> arg_list
%type <symbol> constant
%type <varopt> var_options
%type <type> type_rule
%type <record> record_body
%%

Input : prog_header { printf("#include <stdio.h>\n"); }
        declarations
        KWD_BEGIN { printf("int main()\n{\n"); }
        statements
        KWD_END { printf("  return 0;\n}\n"); } '.'
      ;

statements :
           | statements { printf("  "); } statement
           ;

statement : func_call
          | IDENT ':' '=' expression ';' { printf("%s = %s;\n", string_toupper($1), $4); free($1); free($4); }
          ;

expression : IDENT { $$ = strdup(string_toupper($1)); }
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

arg_list : arg { $$ = NULL; add_arg(&($$), $1); free($1); }
         | arg_list ',' arg { add_arg(&($1), $3); free($3); $$ = $1; }
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
            | ':' INT_CONST ':' INT_CONST { $$ = (VarOpt *)malloc(sizeof(VarOpt)); $$->len = atoi($2); $$->precision = atoi($4); }
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
                   add_fields(&($$), $1);
              }
            | record_body
              field_decl ';' { add_fields(&($1), $2); $$ = $1; }
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
              memcpy(&($$->record), $2, sizeof(Record));
            }
          ;

var_decl : var_list ':' type_rule ';'
           {
               process_var_decl($1, $3);
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

const_record_field : IDENT ':' 
                   ;

const_record_fields :
                    ;

const_record : '(' const_record_fields ')'
             ;

literal : CHAR_CONST {
               $$ = (Literal *)malloc(sizeof(Literal));
               $$->type = CHAR_CONST;
               $$->value = $1;
           }
        | INT_CONST  {
            $$ = (Literal *)malloc(sizeof(Literal));
            $$->type = INT_CONST;
            $$->value = $1;
          }
        | REAL_CONST {
            $$ = (Literal *)malloc(sizeof(Literal));
            $$->type = REAL_CONST;
            $$->value = $1;
          }
        ;

const_value : literal

const_decl : constant '=' literal
             { check_const($1->type->type, $3); print_const($1, $3); } ';'
           | constant '=' const_record ';'
           ;

constant : IDENT ':' type_rule
             {
                 VarList var;
                 SymbolEntry *symbol = (SymbolEntry *)malloc(sizeof(SymbolEntry));
                 var.count = 1;
                 var.varnames = &($1);
                 process_var_decl(&var, $3);
                 symbol->name = string_toupper($1);
                 symbol->type = $3;
                 $$ = symbol;
             }
           ;

%%

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

void process_var_decl(VarList *vars, Type *type)
{
    char *varname;
    varname = add_vars_to_globals(vars, type);
    if (varname != NULL)
    {
        printf("identifier %s ", varname);
        yyerror("redeclared");
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

void check_int_const(BUILTIN_TYPE type, char *value)
{
    if (type == CHAR)
    {
        yyerror("incompatible types 'Char' and 'Integer'");
    }
    switch (type)
    {
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

void check_const()

void check_real_const(BUILTIN_TYPE type, char *value)
{
    switch (type)
    {
        case CHAR:
            yyerror("incompatible types 'Char' and 'Extended'");
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
        default:
            yyerror("incompatible types 'Extended' and 'Char'");
            break;
    }
}

void add_var(VarList **varsptr, char *varname)
{
    if (! varsptr || ! varname)
        return;
    if (! *varsptr)
    {
        *varsptr = (VarList *)malloc(sizeof(VarList));
        (*varsptr)->size = 8;
        (*varsptr)->count = 0;
        (*varsptr)->varnames = (char **)malloc(sizeof(char *) * (*varsptr)->size);
    }
    if ((*varsptr)->count == (*varsptr)->size)
    {
        (*varsptr)->size += 8;
        (*varsptr)->varnames = (char **)realloc((*varsptr)->varnames,
                                                sizeof(char *) * (*varsptr)->size);
    }
    (*varsptr)->varnames[(*varsptr)->count] = varname;
    (*varsptr)->count++;
}


void add_arg(ArgumentList **arglistptr, Argument *arg)
{
    if (! arglistptr)
        return;
    if (! *arglistptr)
    {
        *arglistptr = (ArgumentList *)malloc(sizeof(ArgumentList));
        (*arglistptr)->size = 8;
        (*arglistptr)->count = 0;
        (*arglistptr)->args = (Argument *)malloc(sizeof(Argument) * (*arglistptr)->size);
    }
    if ((*arglistptr)->count == (*arglistptr)->size)
    {
        (*arglistptr)->size += 8;
        (*arglistptr)->args = (Argument *)realloc((*arglistptr)->args,
                                                  sizeof(Argument) * (*arglistptr)->size);
    }
    memcpy(&((*arglistptr)->args[(*arglistptr)->count]), arg, sizeof(Argument));
    (*arglistptr)->count++;
}

void add_fields(Record **recordptr, VarList *fields)
{
    if (! recordptr)
        return;
    if (! *recordptr)
    {
        *recordptr = (Record *)malloc(sizeof(Record));
        (*recordptr)->size = 8;
        (*recordptr)->count = 0;
        (*recordptr)->varlists = (VarList *)malloc(sizeof(VarList) * (*recordptr)->size);
    }
    if ((*recordptr)->count == (*recordptr)->size)
    {
        (*recordptr)->size += 8;
        (*recordptr)->varlists = (VarList *)realloc((*recordptr)->varlists,
                                                    sizeof(VarList) * (*recordptr)->size);
    }
    memcpy(&((*recordptr)->varlists[(*recordptr)->count]), fields, sizeof(VarList));
    (*recordptr)->count++;
}

void print_type(Type *type)
{
    if (type->type != RECORD)
        printf("%s", get_c_type(type->type));
    else
    {
        int i;
        printf("struct\n{\n");
        for (i = 0; i < type->record.count; i++)
        {
            printf("  ");
            print_vars(&(type->record.varlists[i]));
        }
        printf("}");
    }
}

void print_const(SymbolEntry *symbol, char *value)
{
    print_type(symbol->type);
    printf(" %s = %s;\n", symbol->name, value);
}

void print_vars(VarList *vars)
{
    int i;
    print_type(vars->type);
    putchar(' ');
    for (i = 0; i < vars->count - 1; i++)
        printf("%s, ", string_toupper(vars->varnames[i]));
    printf("%s;\n", string_toupper(vars->varnames[i]));
}

void print_funcall(char *funcname, ArgumentList *arglist)
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
        if (arglist->args[i].string)
            printf("%s", arglist->args[i].string);
        else if (arglist->args[i].varname)
            print_var_arg(arglist->args[i].varname,
                          arglist->args[i].opt,
                          function == READ_FUNC);

    if (function == WRITELN_FUNC)
        printf("\\n");
    printf("\"");

    for (i = 0; i < arglist->count; i++)
        if (arglist->args[i].varname)
            printf(",%s%s", function == READ_FUNC ? "&" : "",
                   string_toupper(arglist->args[i].varname));

    printf(");\n");
}

void clear_vars(VarList **varsptr)
{
    if (! varsptr || ! *varsptr)
        return;
    free((*varsptr)->varnames);
    free(*varsptr);
    *varsptr = NULL;
}

void clear_args(ArgumentList **arglistptr)
{
    int i;
    if (! arglistptr && ! *arglistptr)
        return;
    for (i = 0; i < (*arglistptr)->count; i++)
    {
        free((*arglistptr)->args[i].varname);
        free((*arglistptr)->args[i].string);
        free((*arglistptr)->args[i].opt);
    }
    free((*arglistptr)->args);
    free(*arglistptr);
    *arglistptr = NULL;
}

void add_global_symbol(char *name, Type *type)
{
    if (! GlobalSymbolTable.entries)
    {
        GlobalSymbolTable.size = 8;
        GlobalSymbolTable.count = 0;
        GlobalSymbolTable.entries = (SymbolEntry *)malloc(
            sizeof(SymbolEntry) * GlobalSymbolTable.size);
    }
    if (GlobalSymbolTable.count == GlobalSymbolTable.size)
    {
        GlobalSymbolTable.size += 8;
        GlobalSymbolTable.entries = (SymbolEntry *)realloc(GlobalSymbolTable.entries,
            sizeof(SymbolEntry) * GlobalSymbolTable.size);
    }
    GlobalSymbolTable.entries[GlobalSymbolTable.count].name = name;
    GlobalSymbolTable.entries[GlobalSymbolTable.count].type = type;
    GlobalSymbolTable.count++;
}

char *add_vars_to_globals(VarList *vars, Type *type)
{
    int i;
    for (i = 0; i < vars->count; i++)
        if (get_var_type(vars->varnames[i]) != NULL)
            return vars->varnames[i];
        else
            add_global_symbol(vars->varnames[i], type);
    return NULL;
}

Type *get_var_type(char *name)
{
    int i;
    for (i = 0; i < GlobalSymbolTable.count; i++)
        if (strcasecmp(name, GlobalSymbolTable.entries[i].name) == 0)
            return GlobalSymbolTable.entries[i].type;
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
        printf("%d.%d", vo->len, vo->precision);
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
