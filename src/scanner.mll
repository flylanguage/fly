{ open Parser }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ident = letter (letter | digit | '_')*
let int = digit+
(* TODO: float has to be declared in the parser.mll file as a literal *)
let float = digit+ '.' digit*
let whitespace = [' ' '\t' '\r' '\n']+
let squote = '\''

rule token = parse
  | whitespace { token lexbuf }  (* Ignore whitespace *)
  | "/*" { gcomment lexbuf }    (* General comments *)
  | "//" { lcomment lexbuf }    (* Line comments *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | ';' { SEMI }
  | ':' { COLON }
  | "::" { DCOLON }
  | '.' { DOT }
  | ',' { COMMA }
  (* Operators *)
  | '+' { PLUS }
  | "++" int { PREINCR }
  | int "++" { POSTINCR }
  | '-' { MINUS }
  | "--" int { PREDECR }
  | int "--" { POSTDECR }
  | '*' { TIMES }
  | "**" { EXPONENT }
  | '/' { DIVIDE }
  | '%' { MODULO }
  | "=" { ASSIGN }
  | "+=" { PLUS_ASSIGN }
  | "-=" { MINUS_ASSIGN }
  | ":=" { WALRUS }
  | "==" { EQ }
  | "!=" { NEQ }
  | '<' { LT }
  | '>' { GT }
  (* TODO: decide if we want a negation operator such as ~ (see functionality
           in python's numpy library *)
  | "&&" { AND }
  | "|" { OR }
  | "<" ident ">" { TYPEAPP }
  (* Keywords *)
  | "if" { IF }
  | "else" { ELSE }
  | "while" { WHILE }
  (* TODO: this was not included in the keywords of our manual but was in the
     parser.mly file
  | "interface" { INTERFACE }
  *)
  | "struct" { STRUCT }
  | "self" { SELF }
  | "tuple" { TUPLE }
  | "string" { STRING }
  | "for" { FOR }
  | "break" { BREAK }
  | "continue" { CONT }
  | "in" { IN }
  | "int" { INT }
  | "bool" { BOOL }
  | "char" { CHAR }
  | "float" { FLOAT }
  | "string" { STRING }
  | "list" { LIST }
  | "tuple" { TUPLE }
  | "fun" { FUN }
  | "->" { ARROW }
  | "return" { RETURN }
  | "let" { LET }
  | "mut" { MUT }
  | "match" { MATCH }
  | "type" { TYPE }
  | "self" { SELF }
  | "enum" { ENUM }
  | "bind" { BIND }
  | "as" { AS }
  | "import" { IMPORT }
  | "export" { EXPORT }
  (* Literals *)
  | "true" { BLIT(true) }
  | "false" { BLIT(false) }
  | int as num { LITERAL(int_of_string num) }
  (* TODO: the FLOATLIT has yet to be declared in the parser file
  | float as num { FLOATLIT(float_of_string num) }
  *)
  | ident as id { ID(id) }
  (* TODO: Character literals ... CLIT not currently defined in the parser.mly file
     not sure if this is needed ... '
  | squote _ squote as c { CLIT(c.[1]) }
  *)
  (* EOF *)
  | eof { EOF }
  (* Unrecognized character *)
  | _ as c { raise (Failure ("Illegal character " ^ Char.escaped c)) }
(* use and keyword so that you can mutually recurse ... shortcut so i don't
   have to manually declare functions as let rec ... *)
and gcomment = parse
  | "*/" { token lexbuf }
  | eof { token lexbuf }
  | _ { gcomment lexbuf }

and lcomment = parse
  | '\n' { token lexbuf }
  | eof { token lexbuf }
  | _ { lcomment lexbuf }
