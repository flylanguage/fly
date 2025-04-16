open Fly_lib.Parser
(* open Fly_lib.Ast *)

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
(* let string_of_op = function
  | Iden -> "="
  | Add -> "+"
  | Sub -> "-"
  | Div -> "//"
  | Mult -> "*"
  | FDiv -> "/"
  | Exp -> "**"
  | Eq -> "=="
  | Neq -> "!="
  | Less -> "<"
  | More -> ">"
  | And -> "&&"
  | Or -> "||"
  | Xor -> "^"
  | Mod -> "%"
  | LShift -> "<<"
  | RShift -> ">>"
  | BitAnd -> "&"
  | BitOr -> "|"
  | BitXor -> "^"
  | Geq -> ">="
  | Leq -> "<="

let string_of_assign_op = function
  | IdentityAssign -> "="
  | PlusAssign -> "+="
  | MinusAssign -> "-="

let rec string_of_var = function
  | Var v -> v
  (**| VarDot(v1, v2) -> string_of_var v1 ^ "." ^ string_of_var v2*)
  | VarIndex (v, e) -> string_of_var v ^ "[" ^ string_of_expr e ^ "]"

and string_of_expr = function
  Literal l -> string_of_int l
  | BoolLit true -> "True"
  | BoolLit false -> "False"
  | FloatLit f -> string_of_float f
  | StringLit s -> s
  | Binop (e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | VarExpr v -> string_of_var v
  | List list -> "[" ^ String.concat ", " (List.map string_of_expr list) ^ "]"
  | Dict d ->
      let expr_expr_printer = function
        | e1, e2 -> string_of_expr e1 ^ " : " ^ string_of_expr e2
      in
      "{" ^ String.concat ", " (List.map expr_expr_printer d) ^ "}"
  | FuncCall (v, e) ->
      string_of_var v ^ "("
      ^ String.concat ", " (List.map string_of_expr e)
      ^ ")"
  | ListCompUnconditional (e1, v, e2) ->
      "[" ^ string_of_expr e1 ^ " for " ^ string_of_var v ^ " in "
      ^ string_of_expr e2 ^ "]"
  | ListCompConditional (e1, v, e2, e3) ->
      "[" ^ string_of_expr e1 ^ " for " ^ string_of_var v ^ " in "
      ^ string_of_expr e2 ^ " if " ^ string_of_expr e3 ^ "]"
  | IndexingStringLit (s, e) -> s ^ "[" ^ string_of_expr e ^ "]"
  | IndexingExprList (e1, e2) ->
      string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
  | DictCompConditional (e1, e2, v1, v2, e3, e4) ->
      "{" ^ string_of_expr e1 ^ " : " ^ string_of_expr e2 ^ " for ("
      ^ string_of_var v1 ^ ", " ^ string_of_var v2 ^ ") in " ^ string_of_expr e3
      ^ "if" ^ string_of_expr e4 ^ "}"
  | DictCompUnconditional (e1, e2, v1, v2, e3) ->
      "{" ^ string_of_expr e1 ^ " : " ^ string_of_expr e2 ^ " for ("
      ^ string_of_var v1 ^ ", " ^ string_of_var v2 ^ ") in " ^ string_of_expr e3
      ^ "}"
  | _ -> raise (Failure "Unable to run string_of_expr on this expression.")

let rec string_of_typevar = function
  TypeVariable v -> v
  | List t -> "list[" ^ string_of_typevar t ^ "]"


let string_of_arg = function
  | v, t -> string_of_var v ^ " : " ^ string_of_typevar t

let string_of_func_sig = function
  | v, args, ret_type ->
      "def " ^ string_of_var v ^ "("
      ^ String.concat ", " (List.map string_of_arg args)
      ^ ") -> " ^ string_of_typevar ret_type

let rec string_of_block = function
  | BlockAssign (v, spec_assign, expr) ->
      string_of_var v ^ " "
      ^ string_of_special_assignment spec_assign
      ^ " " ^ string_of_expr expr ^ "\n"
  | VarDec (t, v, expr) ->
      string_of_var v ^ ": " ^ string_of_typevar t ^ " = " ^ string_of_expr expr
      ^ "\n"
  | While (e, block_list) ->
      "while " ^ string_of_expr e ^ ":\n"
      ^ String.concat "" (List.map string_of_block block_list)
      ^ "\n"
  | For (v, e, block_list) ->
      "for " ^ string_of_var v ^ " in " ^ string_of_expr e ^ ":\n"
      ^ String.concat "" (List.map string_of_block block_list)
      ^ "\n"
  | FuncBlockCall (v, e) ->
      string_of_var v ^ "("
      ^ String.concat ", " (List.map string_of_expr e)
      ^ ")" ^ "\n"
  | ReturnVal e -> "return " ^ string_of_expr e ^ "\n"
  | ReturnVoid -> "return" ^ "\n"
  | FunctionSignature signature -> string_of_func_sig signature ^ "\n"
  | FunctionDefinition (signature, block_list) ->
      string_of_func_sig signature
      ^ "\n"
      ^ String.concat "" (List.map string_of_block block_list)
      ^ "\n"
  | Break -> "break\n"
  | Continue -> "continue\n"
  | Pass -> "pass\n"
  | IfEnd (e, bl) ->
      "if " ^ string_of_expr e ^ ":\n"
      ^ String.concat "\n" (List.map string_of_block bl)
      ^ "\n"
  | IfNonEnd (e, bl, nbl) ->
      "if " ^ string_of_expr e ^ ":\n"
      ^ String.concat "\n" (List.map string_of_block bl)
      ^ string_of_block nbl
  | ElifEnd (e, bl) ->
      "elif " ^ string_of_expr e ^ ":\n"
      ^ String.concat "\n" (List.map string_of_block bl)
      ^ "\n"
  | ElifNonEnd (e, bl, nbl) ->
      "elif " ^ string_of_expr e ^ ":\n"
      ^ String.concat "\n" (List.map string_of_block bl)
      ^ string_of_block nbl
  | ElseEnd bl ->
      "else:\n" ^ String.concat "\n" (List.map string_of_block bl) ^ "\n"

let string_of_program fdecl =
  "\n\nParsed program: \n\n"
  ^ String.concat "" (List.map string_of_block fdecl.body)
  ^ "\n" *)
