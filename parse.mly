/* Ocamlyacc parser for Fly */

%{
open Ast
%}

%token SEMI COLON DCOLON DOT COMMA LPAREN RPAREN LBRACE RBRACE
%token LBRACKET RBRACKET PLUS MINUS DIVIDE MODULO TIMES
%token ASSIGN WALRUS
%token EQ NEQ LT GT AND OR
%token IF ELSE WHILE FOR BREAK CONT IN
%token INT BOOL CHAR FLOAT STRING LIST TUPLE
%token FUN ARROW RETURN
%token LET MUT MATCH
%token TYPE SELF ENUM BIND AS
%token IMPORT EXPORT
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token EOF

%start program_rule
%type <Ast.program> program_rule

%left COMMA
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT
%left PLUS MINUS

%%

program_rule:
