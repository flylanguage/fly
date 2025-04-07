/* Ocamlyacc parser for Fly */

%{
open Ast
%}

%token SEMI COLON DCOLON DOT COMMA LPAREN RPAREN LBRACE RBRACE
%token LBRACKET RBRACKET PLUS MINUS DIVIDE MODULO TIMES PREINCR PREDECR POSTINCR POSTDECR
%token ASSIGN WALRUS
%token EQ NEQ LT GT AND OR NOT
%token IF ELSE WHILE FOR BREAK CONT IN
%token INT BOOL CHAR FLOAT STRING LIST TUPLE UNIT TRUE FALSE
%token FUN ARROW RETURN
%token LET MUT MATCH INTERFACE
%token TYPE SELF ENUM BIND AS
%token IMPORT EXPORT
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token EOF

%start program_rule
%type <Ast.program> program_rule

%right ASSIGN
%right WALRUS
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

typ:
    INT { Int }
  | BOOL { Bool }
  | CHAR { Char }
  | FLOAT { Float }
  | STRING { String }
  | LIST LT typ GT { List($3) }
  | UNIT { Unit }

vdecl:
  | LET ID COLON TYPE EQUAL expr SEMI { Decl($2, $4, $6) }  (* let x: int = 5; *)
  | LET ID WALRUS expr SEMI           { Decl($2, $4) }      (* let x := 5; *)

expr:
    LITERAL          { Literal($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | NOT expr         { Unop(Not, $2) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
