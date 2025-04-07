{
    open Parser
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ident = letter (letter | digit | '_')*
let int = '-'? digit+
(* TODO: float has to be declared in the parser.mll file as a literal *)
let exp = ['e' 'E'] ['+' '-']? digit+
let float = '-'? digit+ ('.' digit*)? (digit)? (*TODO: need to define exponent *)
let whitespace = [' ' '\t' '\r' '\n']+
let squote = '\''

let escape = '\\' ['\\' '\'' '\"' 'n' 't' 'b' 'r']
let char_inner = escape | [^ '\'' '\\']
let charlit = '\'' char_inner '\''

let str_char = escape | [^ '\"' '\\']
let stringlit = '\"' str_char* '\"'

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
  | "->" { ARROW }
  (* Operators *)
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | "**" { EXPONENT }
  | '/' { DIVIDE }
  | '%' { MODULO }
  | "=" { EQUAL }
  | ":=" { WALRUS }
  | "+=" { PLUS_ASSIGN }
  | "-=" { MINUS_ASSIGN }
  | "==" { BEQ }
  | "!=" { NEQ }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | '<' { LT }
  | '>' { GT }
  | "&&" { AND }
  | "|" { OR }
  | "!" { NOT }
  (* TODO: Do we need to treat them separately?
  | "++" int { PREINCR }
  | int "++" { POSTINCR }
  | "--" int { PREDECR }
  | int "--" { POSTDECR }*)
  | "++" { INCR }
  | "--" { DECR }
  (* TODO: decide if we want a negation operator such as ~ (see functionality
           in python's numpy library *)
  (* Keywords *)
  | "as" { AS }
  | "bind" { BIND }
  | "bool" { BOOL }
  | "break" { BREAK }
  | "char" { CHAR }
  | "continue" { CONT }
  | "else" { ELSE }
  | "enum" { ENUM }
  | "export" { EXPORT }
  | "false" { BLIT(false) }
  | "float" { FLOAT }
  | "for" { FOR }
  | "fun" { FUN }
  | "if" { IF }
  | "import" { IMPORT }
  | "in" { IN }
  | "int" { INT }
  | "let" { LET }
  | "list" { LIST }

  | "match" { MATCH }
  | "mut" { MUT }
  | "return" { RETURN }
  | "self" { SELF }
  | "string" { STRING }
  | "type" { TYPE }
  (* TODO: Should test be a keyword too?
  | "test" { TEST }
  *)
  | "true" { BLIT(true) }
  | "type" { TYPE }
  | "tuple" { TUPLE }
  | "while" { WHILE }
  (* TODO: this was not included in the keywords of our manual but was in the
     parser.mly file
  | "interface" { INTERFACE }
  *)

  (* Literals *)
  | int as num { LITERAL(int_of_string num) }
  (* Assuming FLOATLIT is defined in parser*)
  | float as f { FLOATLIT(float_of_string f) }  
  | int as i { LITERAL(int_of_string i)}
  (* Assuming CHARLIT and STRINGLIT are defined in parser*)
  | charlit as c { CHARLIT(c.[1])}
  | stringlit as s { STRINGLIT(String.sub s 1 (String.length s - 2)) }

  (* Identifiers *)
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
  | eof { EOF }
  | _ { gcomment lexbuf }

and lcomment = parse
  | '\n' { token lexbuf }
  | eof { EOF }
  | _ { lcomment lexbuf }
