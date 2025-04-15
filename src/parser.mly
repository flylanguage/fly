/* Ocamlyacc parser for Fly */
%{
open Ast
%}

%token SEMI COLON DCOLON DOT COMMA LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token PLUS MINUS DIVIDE MODULO TIMES INCR DECR EXPONENT
%token EQUAL WALRUS PLUS_ASSIGN MINUS_ASSIGN
%token BEQ NEQ LT LEQ GT GEQ AND OR NOT

%token IF ELSE WHILE FOR BREAK CONT IN
%token INT BOOL CHAR FLOAT STRING LIST TUPLE UNIT 
%token FUN ARROW RETURN
%token LET MUT MATCH UNDERSCORE INTERFACE
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

%right EQUAL PLUS_ASSIGN MINUS_ASSIGN WALRUS
%right DCOLON
%left OR
%left AND
%nonassoc NOT
%nonassoc BEQ NEQ LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right EXPONENT


%%

program_rule:
  block_list EOF { { body = $1} } (* mirror PyN *)

block_list:
 {[]}
 | block block_list { $1 :: $2 }

block:
  declaration         { $1 }
  | assignment          { $1 }
  | func_def            { $1 }
  | func_call           { $1 }
  | udt_def             { $1 }
  | control_flow        { $1 }

declaration:
  LET MUT ID COLON typ EQUAL expr SEMI  { MutDeclTyped($3, $5, $7) }  (* let x: int = 5; *)
  | LET MUT ID WALRUS expr SEMI           { MutDeclInfer($3, $5) }      (* let x := 5; *)
  | LET ID COLON typ EQUAL expr SEMI      { DeclTyped($2, $4, $6) }  (* let x: int = 5; *)
  | LET ID WALRUS expr SEMI               { DeclInfer($2, $4) }      (* let x := 5; *)

assignment:
  ID EQUAL expr SEMI                       { Assign($1, IdentityAssign, $3) }
  | ID PLUS_ASSIGN expr SEMI             { Assign($1, PlusAssign, $3 ) }
  | ID MINUS_ASSIGN expr SEMI             { Assign($1, MinusAssign, $3 ) }

func_def:
  FUN ID LPAREN formals_opt RPAREN ARROW typ LBRACE block_list RBRACE
  {
    FunctionDefintion($7, $2, $4, $9)
  }
  (* first argument to bound function must be self *)
| BIND ID LT typ GT LPAREN SELF formals_opt RPAREN ARROW typ LBRACE block_list RBRACE
  {
    BoundFunctionDefintion($11, $2, ("self", $4) :: $8, $13, $4)
  }

formals_opt:
  (* empty *) { [] }
| formal_list { List.rev $1 }


formal_list:
    ID COLON typ                   { [($1,$3)] }
  | formal_list COMMA ID COLON typ { ($3,$5) :: $1 }


func_call:
  ID LPAREN list_elements RPAREN       { Call($1, $3) } (* Function call *)

udt_def:
  TYPE ID LBRACE udt_members RBRACE       { UDTDef($2, $4) }

udt_members:
  ID COLON typ                        {[($1, $3)]}
  | ID COLON typ COMMA udt_members    {($1, $3) :: $5}

typ:
    INT { Int }
  | BOOL { Bool }
  | CHAR { Char }
  | FLOAT { Float }
  | STRING { String }
  | LIST LT typ GT { List($3) }
  | TUPLE LT typ_list GT { Tuple($3) }
  | ID     { UserType($1) }
  | UNIT { Unit }

typ_list:
  typ                  {[$1]}
  | typ COMMA typ_list {$1 :: $3}


expr:
    LITERAL                            { Literal($1)  } (* base types *)
  | BLIT                               { BoolLit($1)  }
  | FLIT                               { FloatLit($1) }
  | CLIT                               { CharLit($1)  }
  | SLIT                               { StringLit($1)}
  | ID                                 { Id($1) }

  | expr PLUS   expr                   { Binop($1, Add,   $3) } (* arithmetic expressions *)
  | expr MINUS  expr                   { Binop($1, Sub,   $3) }
  | expr TIMES  expr                   { Binop($1, Mult,  $3) }
  | expr DIVIDE expr                   { Binop($1, Div,   $3) }
  | expr MODULO expr                   { Binop($1, Mod, $3)}
  | expr EXPONENT expr                 { Binop($1, Exp, $3)}
  | ID INCR                            { UnopSideEffect($1, Postincr)   }
  | ID DECR                            { UnopSideEffect($1, Postdecr)   }
  | INCR ID                            { UnopSideEffect($2, Preincr)    }
  | DECR ID                            { UnopSideEffect($2, Predecr)    }

  | expr BEQ    expr                   { Binop($1, Equal, $3) } (* logical expressions *)
  | expr NEQ    expr                   { Binop($1, Neq,   $3) }
  | expr LT     expr                   { Binop($1, Less,  $3) }
  | expr LEQ    expr                   { Binop($1, Leq,   $3) }
  | expr GT     expr                   { Binop($1, Greater, $3) }
  | expr GEQ    expr                   { Binop($1, Geq,   $3) }
  | expr AND    expr                   { Binop($1, And,   $3) }
  | expr OR     expr                   { Binop($1, Or,    $3) }
  | NOT expr                           { Unop($2, Not)        }

  | list                               { $1 } (* list literal declaration *)
  | ID LBRACKET expr RBRACKET          { IndexingList($1, $3) }  (* indexing into list *)

  | tuple                              { $1 } (* tuple literal declaration. need to handle indexing into tuple *)

  | udt_instance                       { $1 } (* Instantiating a user defined type *)
  | ID DOT ID                          { UDTAccess($1, $3) } (* access member variable of user defined type *)

  | LPAREN expr RPAREN                 { $2 }
  | MATCH LPAREN expr RPAREN LBRACE case_list RBRACE { Match($3, $6) } (* match is an expression and should evaluate to something *)

case_list:
  case_item                    {[$1]} (* Base case *)
  | case_item COMMA case_list { $1 :: $3 }

case_item:
  pattern ARROW expr { ($1, $3) }

(* Only literals allowed here. TBH, this really needs discussion *)
pattern:
    LITERAL          { Literal($1)  }
  | BLIT             { BoolLit($1)  }
  | FLIT             { FloatLit($1) }
  | CLIT             { CharLit($1)  }
  | SLIT             { StringLit($1)}
  | ID               { Id($1) }
  | UNDERSCORE       { Wildcard } (* Wildcard for match *)

list_elements:
  expr                      {[$1]}
  | expr COMMA list_elements  {$1 :: $3}

list:
  LBRACKET list_elements RBRACKET      { ListElements($2) }

tuple:
  LPAREN COMMA RPAREN                               { TupleElements([]) }
  | LPAREN expr COMMA RPAREN                        { TupleElements([$2]) }
  | LPAREN expr COMMA list_elements RPAREN          { TupleElements($2 :: $4) }

udt_instance:
  ID LBRACE udt_contents RBRACE         { UDTInstance($1, $3) }

udt_contents:
  | udt_element                        { [$1] }
  | udt_element COMMA udt_contents     { $1 :: $3}

udt_element:
  ID COLON expr                        { ($1, $3) }


control_flow:
  if_stmt       { $1 }
  | while_loop  { $1 }
  | BREAK       { Break }
  | CONT        { Continue }
  | RETURN SEMI { ReturnUnit}
  | RETURN expr SEMI { ReturnVal $2 }

if_stmt:
  IF LPAREN expr RPAREN LBRACE block_list RBRACE                            { IfEnd($3, $6) }
  | IF LPAREN expr RPAREN LBRACE block_list RBRACE elif_stmt                { IfNonEnd($3, $6, $8) }

elif_stmt:
  ELSE IF LPAREN expr RPAREN LBRACE block_list RBRACE elif_stmt             { ElifNonEnd($4, $7, $9) }
  | ELSE IF LPAREN expr RPAREN LBRACE block_list RBRACE                     { ElifEnd($4, $7) }
  | ELSE LBRACE block_list RBRACE                                           { ElseEnd($3) }

while_loop:
  WHILE LPAREN expr RPAREN LBRACE block_list RBRACE     { While($3, $6) }