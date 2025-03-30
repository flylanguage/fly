/* Ocamlyacc parser for Fly */

%{
open Ast
%}

%token SEMI COLON DCOLON LPAREN RPAREN LBRACE RBRACE PLUS MINUS ASSIGN
%token EQ NEQ LT AND OR
%token IF ELSE WHILE FOR BREAK CONT
%token INT BOOL
%token RETURN COMMA ARROW
%token FUN ENUM LET MUT AS MATCH
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
