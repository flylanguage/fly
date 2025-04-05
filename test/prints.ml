open Fly_lib.Parser


let print_token = function
  | PLUS -> "PLUS" (* Basic arithmetic operations *)
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIVIDE -> "DIVIDE"
  | EXPONENT -> "EXPONENT"
  | MODULO -> "MODULO"

  | AND -> "AND" (* Logical operators *)
  | OR -> "OR"
  | NOT -> "NOT"

  | EQ -> "EQ" (* Equivalence operators *)
  | NEQ -> "NEQ"
  | LT -> "LT"
  | LE -> "LTE"
  | GT -> "GT"
  | GE -> "GTE"

  | LPAREN -> "LPAREN" (* Brackets, braces, arrows *)
  | RPAREN -> "RPAREN"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"

  | LET -> "LET" (* Assignment *)
  | MUT -> "MUT"
  | ID(s) -> Printf.sprintf "ID(%s)" s
  | COLON -> "COLON"
  | WALRUS -> "WALRUS"
  | ASSIGN -> "ASSIGN"
  | PLUS_ASSIGN -> "PLUSASSIGN"
  | MINUS_ASSIGN -> "MINUSASSIGN"
  | INCR -> "INCREMENT"
  | DECR -> "DECREMENT"
  | SEMI -> "SEMI"

  | IF -> "IF" (* Control flow *)
  | ELSE -> "ELSE"
  | WHILE -> "WHILE"
  | FOR -> "FOR"
  | BREAK -> "BREAK"
  | CONT -> "CONTINUE"
  | MATCH -> "MATCH"

  | FUN -> "FUN"
  | ARROW -> "ARROW" (* this is -> *)
  | RETURN -> "RETURN"

  | BLIT(true) -> "BOOL(true)" (* Primitive types / literals *)
  | BLIT(false) -> "BOOL(false)"
  | FLIT(f) -> Printf.sprintf "FLOAT(%f)" f
  | LITERAL(i) -> Printf.sprintf "INT(%d)" i
  | CLIT(s) -> Printf.sprintf "CHAR(%s)" (String.make 1 s)
  | SLIT(s) -> Printf.sprintf "STRING(%s)" s

  | INT -> "INT" (* type declaration? *)
  | FLOAT -> "FLOAT"
  | BOOL -> "BOOL"
  | CHAR -> "CHAR"
  | STRING -> "STRING"
  | TYPE -> "TYPE"
  | COMMA -> "COMMA"

  | INTERFACE -> "INTERFACE"
  | SELF -> "SELF"
  | ENUM -> "ENUM"
  | BIND -> "BIND"
  | AS -> "AS"

  | DOT -> "DOT"

  | DCOLON -> "DCOLON"
  | IN -> "IN"
  | LIST -> "LIST"
  | TUPLE -> "TUPLE"

  | IMPORT -> "IMPORT"
  | EXPORT -> "EXPORT"
  | EOF -> "EOF"
