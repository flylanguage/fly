/* Ocamlyacc parser for Fly */

%{
open Ast
%}

%token SEMI COLON DCOLON LPAREN RPAREN LBRACE RBRACE PLUS MINUS COMMA
%token ASSIGN WALRUS
%token EQ NEQ LT AND OR
%token IF ELSE WHILE FOR BREAK CONT
%token INT BOOL CHAR FLOAT
%token FUN ARROW RETURN
%token LET MUT MATCH
%token TYPE ENUM BIND AS
%token IMPORT EXPORT
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token EOF

%start program_rule
%type <Ast.program> program_rule

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT
%left PLUS MINUS

%%

program_rule:
