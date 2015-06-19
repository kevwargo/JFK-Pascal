%{
  #include <stdio.h>
  int yywrap(void);
  int yylex(void);
  void yyerror(const char *fmt, ...);
  char *parse_pascal_string(char *string);
  #include "y.tab.h"
  enum { BODY_LEVEL, INSIDE_SLC, INSIDE_OSC, INSIDE_NSC } inside_comment = BODY_LEVEL;
  int indent = 2;
%}
%s SLC OSC NSC
%%
<INITIAL>[pP][rR][oO][gG][rR][aA][mM]       return KWD_PROGRAM;
<INITIAL>[bB][eE][gG][iI][nN]               return KWD_BEGIN;
<INITIAL>[eE][nN][dD]                       return KWD_END;
<INITIAL>[vV][aA][rR]                       return KWD_VAR;
<INITIAL>[cC][oO][nN][sS][tT]               return KWD_CONST;

<INITIAL>[mM][oO][dD]                       return KWD_MOD;
<INITIAL>[sS][hH][lL]                       return KWD_SHL;
<INITIAL>[sS][hH][rR]                       return KWD_SHR;
<INITIAL>[dD][iI][vV]                       return KWD_DIV;
<INITIAL>[rR][eE][cC][oO][rR][dD]           return KWD_RECORD;
<INITIAL>[aA][rR][rR][aA][yY]               return KWD_ARRAY;
<INITIAL>[oO][fF]                           return KWD_OF;

<INITIAL>[_a-zA-Z][_0-9a-zA-Z]*             { yylval.text = strdup(yytext);
                                              return IDENT;
                                            }

<INITIAL>\.                                 return '.';
<INITIAL>\-                                 return '-';
<INITIAL>\+                                 return '+';
<INITIAL>\/                                 return '/';
<INITIAL>\(                                 return '(';
<INITIAL>\)                                 return ')';
<INITIAL>\,                                 return ',';
<INITIAL>\;                                 return ';';
<INITIAL>\:                                 return ':';
<INITIAL>\=                                 return '=';
<INITIAL>\*                                 return '*';

<INITIAL>\'[a-zA-Z0-9]\'                    {
                                              yylval.text = strdup(yytext);
					      return CHAR_CONST;
					    }
<INITIAL>[+-]?[0-9]+                        { 
                                              yylval.text = strdup(yytext);
					      return INT_CONST;
					    }
<INITIAL>[-+]?[0-9]+(\.?[0-9]*)?([eE][+-]?[0-9]+)?    {
                                              yylval.text = strdup(yytext);
					      return REAL_CONST;
					    }
<INITIAL>\[                                 return '[';
<INITIAL>"(."                               return '[';

<INITIAL>\]                                 return ']';
<INITIAL>".)"                               return ']';

<INITIAL>"//"                               { printf("//"); inside_comment = INSIDE_SLC; BEGIN SLC; } /* single line comment */
<INITIAL>"(*"                               { printf("/*"); inside_comment = INSIDE_OSC; BEGIN OSC; } /* old style comment */
<INITIAL>\{                                 { printf("/*"); inside_comment = INSIDE_NSC; BEGIN NSC; } /* new style comment */

<INITIAL>\}                                 { yyerror("\nUnexpected end of comment '}'");  exit(-1); }
<INITIAL>"*)"                               { yyerror("\nUnexpected end of comment '*)'"); exit(-1); }

<INITIAL>\'([^\']*(\'\')*[^\']*)*\'   { yylval.text = parse_pascal_string(yytext);
         return STRING_LITERAL; }

<INITIAL>" "|\t|\v|\n              ;

<SLC>\n                            { ECHO; inside_comment = BODY_LEVEL; BEGIN INITIAL; }
<SLC>"/*"                          printf("/ *");
<SLC>"*/"                          printf("* /");
<SLC>.                             ECHO;

<OSC>"*)"                          { puts("*/"); inside_comment = BODY_LEVEL; BEGIN INITIAL; }
<OSC>"/*"                          printf("/ *");
<OSC>"*/"                          printf("* /");
<OSC>\n                            ECHO;
<OSC>.                             ECHO;

<NSC>"}"                           { puts("*/"); inside_comment = BODY_LEVEL; BEGIN INITIAL; }
<NSC>"/*"                          printf("/ *");
<NSC>"*/"                          printf("* /");
<NSC>\n                            ECHO;
<NSC>.                             ECHO;


%%
char *parse_pascal_string(char *string)
{
    int len = strlen(string);
    char *result = (char *)malloc(len);
    int i, j;
    for (i = 1, j = 0; i < len - 1; i++, j++)
    {
        result[j] = string[i];
        if (string[i] == '\'')
            i++;
    }
    result[j] = '\0';
    return result;
}

int yywrap(void)
{
  switch(inside_comment)
  {
    case INSIDE_SLC: yyerror("\nUnexpected end of file inside line comment '//'");
                     exit(-1);
    case INSIDE_OSC: yyerror("\nUnexpected end of file inside old style comment '(* *)'");
                     exit(-1);
    case INSIDE_NSC: yyerror("\nUnexpected end of file inside comment '{ }'");
                     exit(-1);
  }
  return 1;
}
