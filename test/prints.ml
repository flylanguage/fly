open Fly_lib.Parser
open Fly_lib.Ast

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
  | BEQ -> "BEQ" (* Equivalence operators *)
  | NEQ -> "NEQ"
  | LT -> "LT"
  | LEQ -> "LEQ"
  | GT -> "GT"
  | GEQ -> "GEQ"
  | LPAREN -> "LPAREN" (* Brackets, braces, arrows *)
  | RPAREN -> "RPAREN"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LET -> "LET" (* Assignment *)
  | MUT -> "MUT"
  | ID s -> Printf.sprintf "ID(%s)" s
  | COLON -> "COLON"
  | WALRUS -> "WALRUS"
  | EQUAL -> "EQUAL"
  | PLUS_ASSIGN -> "PLUS_ASSIGN"
  | MINUS_ASSIGN -> "MINUS_ASSIGN"
  | INCR -> "INCR"
  | DECR -> "DECR"
  | SEMI -> "SEMI"
  | IF -> "IF" (* Control flow *)
  | ELSE -> "ELSE"
  | WHILE -> "WHILE"
  | FOR -> "FOR"
  | BREAK -> "BREAK"
  | CONT -> "CONTINUE"
  | MATCH -> "MATCH"
  | UNDERSCORE -> "UNDERSCORE"
  | FUN -> "FUN"
  | ARROW -> "ARROW" (* this is -> *)
  | RETURN -> "RETURN"
  | BLIT true -> "BLIT(true)" (* Primitive types / literals *)
  | BLIT false -> "BLIT(false)"
  | FLIT f -> Printf.sprintf "FLIT(%f)" f
  | LITERAL i -> Printf.sprintf "LITERAL(%d)" i
  | CLIT s -> Printf.sprintf "CLIT(%s)" (String.make 1 s)
  | SLIT s -> Printf.sprintf "SLIT(%s)" s
  | UNIT -> "UNIT"
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
;;

(* Print functions for AST *)
let string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Exp -> "**"
  | Mod -> "%"
  | Preincr -> "++"
  | Predecr -> "--"
  | Postincr -> "++"
  | Postdecr -> "--"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "|"
  | Not -> "!"

let string_of_assign_op = function
  IdentityAssign -> "="
  | PlusAssign -> "+="
  | MinusAssign -> "-="

let rec string_of_expr = function
  Literal l -> string_of_int l
  | BoolLit true -> "True"
  | BoolLit false -> "False"
  | FloatLit f -> string_of_float f
  | CharLit c -> Char.escaped c
  | StringLit s -> s
  | Id id -> id
  | Unit -> "()"
  | Binop (e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | UnopSideEffect (id, op) -> 
    
     ( match op with
      Preincr | Predecr -> string_of_op op ^ "" ^ id
      | Postincr| Postdecr -> id ^ "" ^ string_of_op op
      | _ -> raise (Failure "Invalid operation in UnopSideEffect"))
    
  | Unop (e, op) -> string_of_op op ^ "" ^ string_of_expr e
  | Assign (id, assign_op, e) -> id ^ " " ^ string_of_assign_op assign_op ^ " " ^ string_of_expr e
  | ListElements elems -> "[" ^ String.concat ", " (List.map string_of_expr elems) ^ "]"
  | TupleElements elems -> "(" ^ String.concat ", " (List.map string_of_expr elems) ^ ")"
  | Call (func_name, func_args) ->
      func_name ^ "("
      ^ String.concat ", " (List.map string_of_expr func_args)
      ^ ")"
  | UDTInstance (udt_name, udt_members) -> udt_name ^ "{" ^ string_of_udt_instance udt_members ^ "}"
  | UDTAccess (udt_name, udt_accessed_member) -> udt_name ^ "." ^ udt_accessed_member
  | UDTStaticAccess (udt_name, udt_accessed_member) -> udt_name ^ "::" ^ udt_accessed_member 
  | IndexingList (list_name, idx_expr) -> list_name ^ "[" ^ string_of_expr idx_expr ^ "]"
  | Match (e1, case_list) -> "match (" ^ string_of_expr e1 ^ "){\n" ^ string_of_case_list case_list ^ "}"
  | Wildcard -> "_"

and string_of_case_list = function 
  [] -> "" (* empty case *)
  | hd :: tl -> string_of_expr (fst hd) ^ " -> " ^ string_of_expr (snd hd) ^ ",\n" ^ string_of_case_list tl

and string_of_udt_instance = function
  [] -> ""
  | hd :: tl -> (fst hd) ^ ": " ^ string_of_expr (snd hd) ^ ", " ^ string_of_udt_instance tl

let rec string_of_type = function
  Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Float -> "float"
  | String -> "string"
  | List (t) -> "list<" ^ string_of_type t ^ ">"
  | Tuple (t_list) -> "tuple<" ^ (String.concat ", " (List.map string_of_type t_list)) ^ ">"
  | Unit -> "()"
  | UserType (udt_name) -> udt_name

let rec string_of_func_args = function
  [] -> ""
  | hd :: tl -> (fst hd) ^ ": " ^ string_of_type (snd hd) ^ ", " ^ string_of_func_args tl

let rec string_of_block = function
  MutDeclTyped (id, typ, e) -> "let mut " ^ id ^ ": " ^ string_of_type typ ^ " = " ^ string_of_expr e
  | MutDeclInfer (id, e) ->  "let mut " ^ id ^ " := " ^ string_of_expr e
  | DeclTyped (id, typ, e) -> "let " ^ id ^ ": " ^ string_of_type typ ^ " = " ^ string_of_expr e
  | DeclInfer (id, e) -> "let " ^ id ^ " := " ^ string_of_expr e
  | Assign (id, assign_op, e) -> id ^ string_of_assign_op assign_op ^ string_of_expr e 
  | FunctionDefinition  (rtyp, func_name, func_args, func_body) ->
    "fun " ^ func_name  ^ "(" ^  string_of_func_args func_args ^ ") -> " ^ string_of_type rtyp ^ "{\n"
    ^ String.concat "" (List.map string_of_block func_body)
    ^ "\n}"
  | BoundFunctionDefinition (rtyp, func_name, func_args, func_body, bound_type) -> 
    "bind " ^ func_name  ^ "<" ^ string_of_type bound_type ^ ">" ^ "(" ^  string_of_func_args func_args ^ ") -> " ^ string_of_type rtyp ^ "{\n"
    ^ String.concat "" (List.map string_of_block func_body)
    ^ "\n}"
  | Call (func_name, func_args) ->
    func_name ^ "("
    ^ String.concat ", " (List.map string_of_expr func_args)
    ^ ")"
  | UDTDef (udt_name, udt_members) -> 
    "type " ^ udt_name ^ "{\n"
    ^ string_of_func_args udt_members (* Re-use string_of_func_args  as it generates name: type string*)
    ^ "\n}" 
  | IfEnd (e, bl) ->
    "if (" ^ string_of_expr e ^ ") {\n"
    ^ String.concat "\n" (List.map string_of_block bl)
    ^ "\n}"
  | IfNonEnd (e, bl, nbl) ->
    "if (" ^ string_of_expr e ^ ") {\n"
    ^ String.concat "\n" (List.map string_of_block bl)
    ^ string_of_block nbl
    ^ "\n}"
  | ElifEnd (e, bl) ->
    "else if (" ^ string_of_expr e ^ ") {\n"
    ^ String.concat "\n" (List.map string_of_block bl)
    ^ "\n}"
  | ElifNonEnd (e, bl, nbl) ->
    "else if (" ^ string_of_expr e ^ ") {\n"
    ^ String.concat "\n" (List.map string_of_block bl)
    ^ string_of_block nbl
    ^ "\n}"
  | ElseEnd bl ->
    "else {" ^ String.concat "\n" (List.map string_of_block bl) ^ "\n}"
  | While (e, block_list) -> 
    "while (" ^ string_of_expr e ^ ") {\n"
    ^ String.concat "" (List.map string_of_block block_list)
    ^ "\n}"
  | Break -> "break"
  | Continue -> "continue"
  | ReturnUnit -> "return"
  | ReturnVal (e) -> "return " ^ string_of_expr e



let string_of_program fdecl =
  "\n\nParsed program: \n\n"
  ^ String.concat "" (List.map string_of_block fdecl.body)
  ^ "\n"