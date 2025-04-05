/* Ocamlyacc parser for Fly */

%{
open Ast
%}



%token SEMI COLON DCOLON DOT COMMA LPAREN RPAREN LBRACE RBRACE
%token LBRACKET RBRACKET PLUS MINUS DIVIDE MODULO TIMES PREINCR PREDECR POSTINCR POSTDECR
%token ASSIGN WALRUS
%token EQ NEQ LT GT AND OR NOT
%token IF ELSE WHILE FOR BREAK CONT IN
%token INT BOOL CHAR FLOAT STRING LIST TUPLE
%token FUN ARROW RETURN
%token LET MUT MATCH INTERFACE
%token TYPE SELF ENUM BIND AS
%token IMPORT EXPORT
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token EOF
/* add float? */
/* add char literal ? */

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

/* add function declaration */
program_rule:
	decls EOF { $1 }

vdecl:
	LET ID COLON typename ASSIGN expr { VarDecl($4, $2, $6) } /* VarDecl(type, name, value) - to implement */
	| LET ID WALRUS expr { VarDeclInfer($2, $4) } /* VarDeclInfer(name, value) - to implement */