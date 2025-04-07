/* Ocamlyacc parser for Fly */

%{
open Ast
%}

%token SEMI COLON DCOLON DOT COMMA LPAREN RPAREN LBRACE RBRACE
%token LBRACKET RBRACKET PLUS MINUS DIVIDE EXPONENT MODULO TIMES INCR DECR PLUS_ASSIGN MINUS_ASSIGN
%token ASSIGN WALRUS
%token EQ NEQ LT GT LE GE AND OR NOT
%token IF ELSE WHILE FOR BREAK CONT IN
%token INT BOOL CHAR FLOAT STRING LIST TUPLE
%token FUN ARROW RETURN
%token LET MUT MATCH INTERFACE
%token TYPE SELF ENUM BIND AS
%token IMPORT EXPORT
%token <int> LITERAL
%token <bool> BLIT
%token <float> FLIT
%token <char> CLIT
%token <string> SLIT
%token <string> ID
%token EOF

%start program_rule
%type <Ast.program> program_rule

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT
%left PLUS MINUS DIVIDE TIMES
%left PREINCR PREDECR
%right POSTINCR POSTDECR

%%

program_rule:
     { Program }
