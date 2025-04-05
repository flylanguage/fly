open Parser


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
  | LTE -> "LTE"
  | GT -> "GT"
  | GTE -> "GTE"

  | LPAREN -> "LPAREN" (* Brackets, braces, arrows *)
  | RPAREN -> "RPAREN"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LARROW -> "LARROW" (* this is < *)
  | RARROW -> "RARROW" (* this is > *)

  | LET -> "LET" (* Assignment *)
  | MUT -> "MUT"
  | ID(s) -> Printf.sprintf "ID(%s)" s
  | COLON -> "COLON"
  | WALRUS -> "WALRUS"
  | ASSIGN -> "ASSIGN"
  | PLUSASSIGN -> "PLUSASSIGN"
  | MINUSASSIGN -> "MINUSASSIGN"
  | PREINCR -> "PREINCR"
  | POSTINCR -> "POSTINCRE"
  | PREDECR -> "PREDECR"
  | POSTDECR -> "POSTDECR"
  | SEMI -> "SEMI"

  | IF -> "IF" (* Control flow *)
  | ELSE -> "ELSE"
  | WHILE -> "WHILE"
  | FOR -> "FOR"
  | BREAK -> "BREAK"
  | CONTINUE -> "CONTINUE"
  | MATCH -> "MATCH"

  | FUN -> "FUN"
  | ARROW -> "ARROW" (* this is -> *)
  | RETURN -> "RETURN"

  | BLIT(true) -> "BOOLLIT(true)" (* Primitive types / literals *)
  | BLIT(false) -> "BOOLLIT(false)"
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
  | STRING -> "STRING"
  | TYPE -> "TYPE"
  | TYPE_NAME(s) -> Printf.sprintf "TYPE_NAME(%s)" s
  | FIELD_NAME(s) -> Printf.sprintf "FIELD_NAME(%s)" s
  | COMMA -> "COMMA"

  | INTERFACE -> "INTERFACE"
  | SELF -> "SELF"
  | ENUM -> "ENUM"
  | BIND -> "BIND"
  | AS -> "AS"

  | DOT -> "DOT"
  | CONS -> "CONS"

  | COMMENT -> "COMMENT"
  | NEWLINE -> "NEWLINE"

  | IMPORT -> "IMPORT"
  | EXPORT -> "EXPORT"
  | EOF -> "EOF"
