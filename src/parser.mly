/* Ocamlyacc parser for Fly */
%{
open Ast
%}

%token SEMI COLON DCOLON DOT COMMA LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token PLUS MINUS DIVIDE MODULO TIMES INCR DECR EXPONENT
%token EQUAL WALRUS PLUS_ASSIGN MINUS_ASSIGN MULT_ASSIGN DIV_ASSIGN
%token BEQ NEQ LT LEQ GT GEQ AND OR NOT

%token IF ELSE WHILE FOR BREAK CONT IN
%token INT BOOL CHAR FLOAT STRING LIST TUPLE 
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

%right EQUAL PLUS_ASSIGN MINUS_ASSIGN MULT_ASSIGN DIV_ASSIGN WALRUS
%right DCOLON


%%

program_rule:
  block_list EOF { { body = $1} } (* mirror PyN *)

block_list:
  /* empty */       {[]}
 | block block_list { $1 :: $2 }


literal:
    LITERAL                           { Literal($1)  } (* base types *)
  | BLIT                              { BoolLit($1)  }
  | FLIT                              { FloatLit($1) }
  | CLIT                              { CharLit($1)  }
  | SLIT                              { StringLit($1)}

literal_expr:
  | literal                           { $1 }
  | list                              { $1 } (* list literal declaration *)
  | tuple                             { $1 } (* tuple literal declaration *)

side_effect_expr:
  | ID INCR                           { UnopSideEffect($1, Postincr) }
  | ID DECR                           { UnopSideEffect($1, Postdecr) }
  | INCR ID                           { UnopSideEffect($2, Preincr)  }
  | DECR ID                           { UnopSideEffect($2, Predecr)  }

access_expr:
  | ID DOT udt_access                   { UDTAccess($1, $3) }
  | ID DCOLON func_call                 { UDTStaticAccess($1, $3) }
  | SELF DOT udt_access                 { UDTAccess ("self", $3) }
  | ID DCOLON ID                        { EnumAccess($1, $3) }
  | expr10 LBRACKET expr10 RBRACKET       { Index($1, $3) }

udt_access:
  | ID                                 { UDTVariable($1) }
  | ID LPAREN list_elements_opt RPAREN { UDTFunction($1, $3) }

match_expr:
  MATCH LPAREN expr RPAREN LBRACE case_list RBRACE  { Match($3, $6) }

expr:
  expr1                                        { $1 }

expr1:
 | expr1 OR expr2         { Binop($1, Or, $3) }
 | expr2                  { $1 }

expr2:
 | expr2 AND expr3        { Binop($1, And, $3) }
 | expr3                  { $1 }

expr3:
 | expr3 DCOLON expr4     { Binop($1, Cons, $3) }
 | expr4                  { $1 }

expr4:
 | expr4 BEQ expr5        { Binop($1, Equal, $3) }
 | expr4 NEQ expr5        { Binop($1, Neq, $3) }
 | expr5                  { $1 }

expr5:
 | expr5 GT expr6         { Binop($1, Greater, $3) }
 | expr5 GEQ expr6        { Binop($1, Geq, $3) }
 | expr5 LT expr6         { Binop($1, Less, $3) }
 | expr5 LEQ expr6        { Binop($1, Leq, $3) }
 | expr6                  { $1 }

expr6:
 | expr6 PLUS expr7       { Binop($1, Add, $3) }
 | expr6 MINUS expr7      { Binop($1, Sub, $3) }
 | expr7                  { $1 }

expr7:
 | expr7 TIMES expr8      { Binop($1, Mult, $3) }
 | expr7 DIVIDE expr8     { Binop($1, Div, $3) }
 | expr7 MODULO expr8     { Binop($1, Mod, $3) }
 | expr8                  { $1 }

expr8:
 | expr8 EXPONENT expr9   { Binop($1, Exp, $3) }
 | expr9                  { $1 }

expr9:
 | NOT expr9              { Unop($2, Not) }
 | expr10                  { $1 } 

expr10:
 | literal_expr                              { $1 }
 | ID                                        { Id($1) }
 | side_effect_expr                          { $1 }
 | access_expr                               { $1 }
 | udt_instance                              { $1 } (* Instantiating a user defined type *)
 | match_expr                                { $1 } (* match is an expression and should evaluate to something *)
 | func_call                                 { FunctionCall($1) }
 | expr10 AS typ                              { TypeCast($3, $1) }
 | parens_expr                               { $1 }


parens_expr:
  | LPAREN expr1 RPAREN                        { $2 }

block:
  | declaration          { $1 }
  | assignment           { $1 }
  | control_flow         { $1 }
  | expr SEMI            { Expr($1) }

typ:
  | INT                   { Int }
  | BOOL                  { Bool }
  | CHAR                  { Char }
  | FLOAT                 { Float }
  | STRING                { String }
  | LIST LT typ GT        { List($3) }
  | TUPLE LT typ_list GT  { Tuple($3) }
  | typ_id                { $1 }
  | LPAREN RPAREN         { Unit }

typ_id:
  ID                      { UserType($1) }

typ_list:
  | typ                 { [$1] }
  | typ COMMA typ_list  { $1 :: $3 }

declaration:
  | var_decl                               { $1 }
  | func_decl                              { $1 }
  | udt_decl                               { $1 }
  | enum_decl                              { $1 }

var_decl:
  | LET MUT ID COLON typ EQUAL expr SEMI    { MutDeclTyped($3, $5, $7) }  (* let mut x: int = 5; *)
  | LET MUT ID WALRUS expr SEMI             { MutDeclInfer($3, $5) }      (* let mut x := 5; *)
  | LET ID COLON typ EQUAL expr SEMI        { DeclTyped($2, $4, $6) }     (* let x: int = 5; *)
  | LET ID WALRUS expr SEMI                 { DeclInfer($2, $4) }         (* let x := 5; *)

func_decl:
  FUN ID LPAREN formals_opt RPAREN ARROW typ LBRACE block_list RBRACE
  {
    FunctionDefinition($7, $2, $4 , $9)
  }
| FUN ID LPAREN formals_opt RPAREN LBRACE block_list RBRACE (* Unspecified return type defaults to Unit. The semantic checker will check if this holds. All other return types must be specified *)
  {
    FunctionDefinition(Unit, $2, $4, $7)
  }
  (* first argument to bound function must be self *)
  (* Need to differentiate between bound static and non-static functions*)
| BIND ID LT typ GT LPAREN SELF formals_opt RPAREN ARROW typ LBRACE block_list RBRACE
  {
    BoundFunctionDefinition($11, $2, ("self", $4) :: $8, $13, $4)
  }
| BIND ID LT typ GT LPAREN SELF formals_opt RPAREN LBRACE block_list RBRACE (* Unit return type bound function*)
  {
    BoundFunctionDefinition(Unit, $2, ("self", $4) :: $8, $11, $4)
  }
| BIND ID LT typ GT LPAREN formals_opt RPAREN ARROW typ LBRACE block_list RBRACE (* This is a static function *)
  {
    BoundFunctionDefinition($4, $2, $7, $12, $4)
  }

formals_opt:
  /* empty */                      {[]}
  | formals                        { $1 }

formals:
  | ID COLON typ                   { [($1,$3)] }
  | ID COLON typ COMMA formals_opt { ($1,$3) :: $5 }


udt_decl:
  TYPE ID LBRACE udt_members_opt RBRACE       { UDTDef($2, $4) }

udt_members_opt:
 /* Empty */                        { [] }
 | udt_members                      { $1 }

udt_members:
  | ID COLON typ                    { [($1, $3)] }
  | ID COLON typ COMMA udt_members  { ($1, $3) :: $5 }

enum_decl:
  ENUM ID LBRACE enum_variants RBRACE { EnumDeclaration($2, $4) }

enum_variants:
  | enum_variant                     { [$1] }
  | enum_variant COMMA enum_variants { $1::$3 }

enum_variant:
  | ID                            { EnumVariantDefault($1) }
  | ID EQUAL LITERAL              { EnumVariantExplicit($1, $3) }

assignment:
 expr assign_op expr SEMI         { Assign($1, $2, $3) }


assign_op:
  | EQUAL                         { IdentityAssign }
  | PLUS_ASSIGN                   { PlusAssign }
  | MINUS_ASSIGN                  { MinusAssign }
  | MULT_ASSIGN                   { MultAssign }
  | DIV_ASSIGN                    { DivAssign }


func_call:
  ID LPAREN list_elements_opt RPAREN   { ($1, $3) } (* Function call *)

case_list:
  | case_item                 {[$1]} (* Base case *)
  | case_item COMMA case_list { $1 :: $3 }

case_item:
  pattern ARROW expr { ($1, $3) }

(* Only literals allowed here. TBH, this really needs discussion *)
(* Added P to indicate these are patterns *)
pattern:
  | LITERAL                     { PLiteral($1)  }
  | BLIT                        { PBoolLit($1)  }
  | FLIT                        { PFloatLit($1) }
  | CLIT                        { PCharLit($1)  }
  | SLIT                        { PStringLit($1)}
  | ID                          { PId($1) }
  | UNDERSCORE                  { PWildcard }
  | LBRACKET RBRACKET           { PEmptyList }
  | pattern DCOLON pattern      { PCons($1, $3) }

list_elements_opt:
  /* empty */                 { [] }
  | list_elements             { $1 }

list_elements:
  | expr10                      {[$1]}
  | expr10 COMMA list_elements  {$1 :: $3}

list:
  LBRACKET list_elements_opt RBRACKET             { List($2) }

tuple:
  LPAREN expr COMMA list_elements_opt RPAREN      { Tuple($2 :: $4) }

udt_instance:
  ID LBRACE udt_contents_opt RBRACE    { UDTInstance($1, $3) }

udt_contents_opt:
  /* Empty */                          { [] }
  | udt_contents                       { $1 }

udt_contents:
  | udt_element                        { [$1] }
  | udt_element COMMA udt_contents     { $1 :: $3 }

udt_element:
  ID COLON expr10                       { ($1, $3) }

control_flow:
  | if_stmt          { $1 }
  | while_loop       { $1 }
  | for_loop         { $1 }
  | BREAK SEMI       { Break }
  | CONT SEMI        { Continue }
  | RETURN SEMI      { ReturnUnit}
  | RETURN expr SEMI { ReturnVal($2) }

if_stmt:
  | IF LPAREN expr RPAREN LBRACE block_list RBRACE                          { IfEnd($3, $6) }
  | IF LPAREN expr RPAREN LBRACE block_list RBRACE elif_stmt                { IfNonEnd($3, $6, $8) }

elif_stmt:
  ELSE IF LPAREN expr RPAREN LBRACE block_list RBRACE elif_stmt             { ElifNonEnd($4, $7, $9) }
  | ELSE IF LPAREN expr RPAREN LBRACE block_list RBRACE                     { ElifEnd($4, $7) }
  | ELSE LBRACE block_list RBRACE                                           { ElseEnd($3) }

while_loop:
  WHILE LPAREN expr RPAREN LBRACE block_list RBRACE     { While($3, $6) }

(* Currently allow only list or variable as iterators *)
for_loop:
  | FOR ID WALRUS list LBRACE block_list RBRACE           { For($2, $4, $6) }
  | FOR ID WALRUS ID LBRACE block_list RBRACE             { For($2, Id($4), $6) }