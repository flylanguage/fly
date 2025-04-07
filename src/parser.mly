/* Ocamlyacc parser for Fly */

%{
open Ast
%}

%token SEMI COLON DCOLON DOT COMMA LPAREN RPAREN LBRACE RBRACE
%token LBRACKET RBRACKET PLUS MINUS DIVIDE MODULO TIMES PREINCR PREDECR POSTINCR POSTDECR EXPONENT
%token PLUS_ASSIGN MINUS_ASSIGN INCR DECR
%token EQUAL WALRUS
%token BEQ NEQ LT LEQ GT GEQ AND OR NOT

%token IF ELSE WHILE FOR BREAK CONT IN
%token INT BOOL CHAR FLOAT STRING LIST TUPLE UNIT TRUE FALSE
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
%right WALRUS
%left OR
%left AND
%left EQ NEQ
%left LT GT
%left PLUS MINUS DIVIDE TIMES
%left EXPONENT
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
  | TUPLE LT typ_list GT { Tuple($3) }
  | UNIT { Unit }
  | ID     { UserType($1) }

typ_list:
    typ { [$1] }
  | typ_list COMMA typ { $3 :: $1 }

vdecl:
  | LET ID COLON typ EQUAL expr SEMI { Decl($2, $4, $6) }  (* let x: int = 5; *)
  | LET ID WALRUS expr SEMI           { Decl($2, $4) }      (* let x := 5; *)

fdecl:
  FUN ID LPAREN formals_opt RPAREN ARROW typ LBRACE body_list RBRACE { Unit }

formals_opt:
  (* empty *) { [] }
  | formal_list   { List.rev $1 } (*Why List.rev? *)

formal_list:
    ID COLON typ                   { [($1,$3)] }
  | formal_list COMMA ID COLON typ { ($3,$5) :: $1 }

body_list:
  (* empty *) { [] }
  | stmt body_list { $1 :: $2 }
  | vdecl body_list { $1 :: $2 }
  | fdecl body_list { $1 :: $2 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Unit}
  | RETURN expr SEMI { Return $2 }
  | LBRACE body_list RBRACE { Block($2) }
  | if_stmt { $1 }
  | WHILE expr LBRACE body_list RBRACE { While($2, $4) }
  (* Save FOR for later *)

if_stmt:
  | IF expr LBRACE body_list RBRACE                              { If($2, Block($4), Block([])) }
  | IF expr LBRACE body_list RBRACE ELSE LBRACE body_list RBRACE { If($2, Block($4), Block($8)) }
  | IF expr LBRACE body_list RBRACE ELSE if_stmt                 { If($2, Block($4), $7) }

expr:
    LITERAL          { Literal($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr BEQ    expr { Binop($1, Equal, $3) }
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

actuals_opt:
  (* empty *) { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
