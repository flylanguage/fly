{
    open Parser
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ident = letter (letter | digit | '_')*
let int = '-'? digit+
let exp = ['e' 'E'] ['+' '-']? digit+
let float = '-'? digit+ ('.' digit*)? (exp)?
let whitespace = [' ' '\t' '\r' '\n']+
let squote = '\''

let escape = '\\' ['\\' '\'' '\"' 'n' 't' 'b' 'r']
let char_inner = escape | [^ '\'' '\\']
let charlit = '\'' char_inner '\''

let str_char = escape | [^ '\"' '\\']
let stringlit = '\"' str_char* '\"'

rule tokenize = parse
  | whitespace { tokenize lexbuf }  (* Ignore whitespace *)
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
  | '=' { EQUAL }
  | ":=" { WALRUS }
  | "+=" { PLUS_ASSIGN }
  | "-=" { MINUS_ASSIGN }
  | "*=" {MULT_ASSIGN}
  | "/=" {DIV_ASSIGN}
  | "==" { BEQ }
  | "!=" { NEQ }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | '<' { LT }
  | '>' { GT }
  | "&&" { AND }
  | "||" { OR }
  | '!' { NOT }
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
  | "_" { UNDERSCORE }
  | "mut" { MUT }
  | "return" { RETURN }
  | "self" { SELF }
  | "string" { STRING }
  | "type" { TYPE }
  | "tuple" { TUPLE }
  | "while" { WHILE }
  (* TODO: this was not included in the keywords of our manual but was in the
     parser.mly file
  | "interface" { INTERFACE }
  *)

  (* Literals *)
  | "false" { BLIT(false) }
  | "true" { BLIT(true) }
  | int as num { LITERAL(int_of_string num) }
  | float as f { FLIT(float_of_string f) }  
  (* Assuming CHARLIT and STRINGLIT are defined in parser*)
  | charlit as c { CLIT(c.[1])}
  | stringlit as s { SLIT(String.sub s 1 (String.length s - 2)) }

  (* Identifiers *)
  | ident as id { ID(id) }

  (* EOF *)
  | eof { EOF }
  (* Unrecognized character *)
  | _ as c { raise (Failure ("Illegal character " ^ Char.escaped c)) }
(* use and keyword so that you can mutually recurse ... shortcut so i don't
   have to manually declare functions as let rec ... *)
and gcomment = parse
  | "*/" { tokenize lexbuf }
  | eof { EOF }
  | _ { gcomment lexbuf }

and lcomment = parse
  | '\n' { tokenize lexbuf }
  | eof { EOF }
  | _ { lcomment lexbuf }
