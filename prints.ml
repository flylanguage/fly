{
    open Pparser (* replace with Parser *)
}

let print_token = function
  | PLUS -> "PLUS" (* Basic arithmetic operations *)
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIVIDE -> "DIVIDE"
  | EXPONENT -> "EXPONENT"
  | MOD -> "MOD"

  | AND -> "AND" (* Logical operators *)
  | OR -> "OR"
  | NOT -> "NOT"

  | EQ -> "EQ" (* Equivalence operators *)
  | NEQ -> "NEQ"
  | LT -> "LT"
  | LTE -> "LTE"
  | GT -> "GT"
  | GTE -> "GTE"

  | LPAREN -> "LPAREN" (* Brackets, braces, arrows *)
  | RPAREN -> "RPAREN"
  | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK"
  | LCURL -> "LCURL"
  | RCURL -> "RCURL"
  | LARROW -> "LARROW"
  | RARROW -> "RARROW"

  | LET -> "LET" (* Assignment *)
  | MUT -> "MUT"
  | VARIABLE(s) -> Printf.sprintf "VARIABLE(%s)" s
  | COLON -> "COLON"
  | WALRUS -> "WALRUS"
  | ASSIGN -> "ASSIGN"
  | PLUSASSIGN -> "PLUSASSIGN"
  | MINUSASSIGN -> "MINUSASSIGN"
  | PREINCREMENT -> "PREINCREMENT"
  | POSTINCREMENT -> "POSTINCREMENT"
  | PREDECREMENT -> "PREDECREMENT"
  | POSTDECREMENT -> "POSTDECREMENT"
  | SEMI -> "SEMI"

  | IF -> "IF" (* Control flow *)
  | ELSE -> "ELSE"
  | WHILE -> "WHILE"
  | FOR -> "FOR"
  | BREAK -> "BREAK"
  | CONTINUE -> "CONTINUE"
  | RETURN -> "RETURN"

  | BOOLLIT(true) -> "BOOLLIT(true)" (* Primitive types / literals *)
  | BOOLLIT(false) -> "BOOLLIT(false)"
  | FLOATLIT(f) -> Printf.sprintf "FLOATLIT(%f)" f
  | INTLIT(i) -> Printf.sprintf "INTLIT(%d)" i
  | CHARLIT(s) -> Printf.sprintf "CHARLIT(%s)" s
  | STRINGLIT(s) -> Printf.sprintf "STRINGLIT(%s)" s

  | SINGLEQUOTE -> "'"
  | DOUBLEQUOTE -> "\""

  | INT -> "INT" (* type declaration? *)
  | FLOAT -> "FLOAT"
  | BOOL -> "BOOL"
  | CHAR -> "CHAR"
  | STR -> "STR"

  | DOT -> "DOT"
  | CONS -> "CONS"

  | NEWLINE -> "NEWLINE"
  | ARROW -> "ARROW"
  | EOF -> "EOF"
