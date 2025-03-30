open Scanner

let print_token = function
  | Parser.PLUS -> "PLUS" (* Basic arithmetic operations *)
  | Parser.MINUS -> "MINUS"
  | Parser.TIMES -> "TIMES"
  | Parser.DIVIDE -> "DIVIDE"
  | Parser.EXPONENT -> "EXPONENT"
  | Parser.MOD -> "MOD"

  | Parser.AND -> "AND" (* Logical operators *)
  | Parser.OR -> "OR"
  | Parser.NOT -> "NOT"

  | Parser.EQ -> "EQ" (* Equivalence operators *)
  | Parser.LT -> "LT"
  | Parser.LEQ -> "LEQ"
  | Parser.NEQ -> "NEQ"
  | Parser.GT -> "GT"
  | Parser.GEQ -> "GEQ"

  | Parser.LPAREN -> "LPAREN" (* Brackets, braces, arrows *)
  | Parser.RPAREN -> "RPAREN"
  | Parser.LBRACK -> "LBRACK"
  | Parser.RBRACK -> "RBRACK"
  | Parser.LCURL -> "LCURL"
  | Parser.RCURL -> "RCURL"
  | Parser.LARROW -> "LARROW"
  | Parser.RARROW -> "RARROW"

  | Parser.LET -> "LET" (* Assignment *)
  | Parser.MUT -> "MUT"
  | Parser.VARIABLE(s) -> Printf.sprintf "VARIABLE(%s)" s
  | Parser.COLON -> "COLON"
  | Parser.WALRUS -> "WALRUS"
  | Parser.ASSIGN -> "ASSIGN"
  | Parser.PLUSASSIGN -> "PLUSASSIGN"
  | Parser.MINUSASSIGN -> "MINUSASSIGN"
  | Parser.PREINCREMENT -> "PREINCREMENT"
  | Parser.POSTINCREMENT -> "POSTINCREMENT"
  | Parser.PREDECREMENT -> "PREDECREMENT"
  | Parser.POSTDECREMENT -> "POSTDECREMENT"
  | Parser.SEMI -> "SEMI"

  | Parser.IF -> "IF" (* Control flow *)
  | Parser.ELSE -> "ELSE"
  | Parser.WHILE -> "WHILE"
  | Parser.FOR -> "FOR"
  | Parser.BREAK -> "BREAK"
  | Parser.CONTINUE -> "CONTINUE"
  | Parser.RETURN -> "RETURN"

  | Parser.BOOLLIT(true) -> "BOOLLIT(true)" (* Primitive types / literals *)
  | Parser.BOOLLIT(false) -> "BOOLLIT(false)"
  | Parser.FLOATLIT(f) -> Printf.sprintf "FLOATLIT(%f)" f
  | Parser.INTLIT(i) -> Printf.sprintf "INTLIT(%d)" i
  | Parser.CHARLIT(s) -> Printf.sprintf "CHARLIT(%s)" s
  | Parser.STRINGLIT(s) -> Printf.sprintf "STRINGLIT(%s)" s

  | Parser.INT -> "INT"
  | Parser.STR -> "STR"
  | Parser.BOOL -> "BOOL"
  | Parser.FLOAT -> "FLOAT"
  | Parser.LIST -> "LIST"
  | Parser.DICT -> "DICT"
  | Parser.VOID -> "VOID"

  | Parser.DOT -> "DOT"
  | Parser.CONS -> "CONS"

  | Parser.NEWLINE -> "NEWLINE"
  | Parser.ARROW -> "ARROW"
  | Parser.EOF -> "EOF"