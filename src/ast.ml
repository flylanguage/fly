type op =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Exp
  | Preincr
  | Postincr
  | Predecr
  | Postdecr
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
  | Not
  | Cons

type pattern =
  | PLiteral of int
  | PBoolLit of bool
  | PFloatLit of float
  | PCharLit of char
  | PStringLit of string
  | PId of string
  | PWildcard
  | PEmptyList
  | PCons of pattern * pattern

type assign_op =
  | IdentityAssign
  | PlusAssign
  | MinusAssign
  | MultAssign
  | DivAssign

type typ =
  | Int
  | Bool
  | Char
  | Float
  | String
  | List of typ
  | Tuple of typ list
  | Unit
  | UserType of string

type expr =
  | Literal of int
  | BoolLit of bool
  | FloatLit of float
  | CharLit of char
  | StringLit of string
  | Unit
  | Id of string
  | Tuple of expr list
  | Binop of expr * op * expr
  | Unop of expr * op (* this is for not *)
  | UnopSideEffect of string * op (* this is for postincr, postdecr, preincr, postdecr *)
  | FunctionCall of func
  | UDTInstance of string * kv_list
  | UDTAccess of string * udt_access
  | UDTStaticAccess of string * func
  | EnumAccess of string * string
  | Index of expr * expr
  | List of expr list
  | Match of expr * (pattern * expr) list
  | Wildcard
  | TypeCast of typ * expr

and func = string * expr list
and kv_list = (string * expr) list (* for user defined types *)

and udt_access =
  | UDTVariable of string
  | UDTFunction of func

type enum_variant =
  | EnumVariantDefault of string
  | EnumVariantExplicit of string * int

type block =
  | MutDeclTyped of string * typ * expr
  | MutDeclInfer of string * expr
  | DeclTyped of string * typ * expr
  | DeclInfer of string * expr
  | Assign of expr * assign_op * expr
  | FunctionDefinition of
      typ
      * string
      * (string * typ) list
      * block list (* rtyp, func_name, func_args, func_body *)
  | BoundFunctionDefinition of
      typ
      * string
      * (string * typ) list
      * block list
      * typ (* rtyp, func_name, func_args, func_body, bound_type *)
  | EnumDeclaration of string * enum_variant list
  | UDTDef of string * (string * typ) list
  | IfEnd of expr * block list
  | IfNonEnd of expr * block list * block
  | ElifNonEnd of expr * block list * block
  | ElifEnd of expr * block list
  | ElseEnd of block list
  | While of expr * block list
  | For of string * expr * block list
  | Break
  | Continue
  | ReturnUnit
  | ReturnVal of expr
  | Expr of expr

type program = { body : block list }

(* Print functions for AST *)
let rec string_of_type = function
  | Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Float -> "float"
  | String -> "string"
  | List t -> "list<" ^ string_of_type t ^ ">"
  | Tuple t_list -> "tuple<" ^ String.concat ", " (List.map string_of_type t_list) ^ ">"
  | Unit -> "()"
  | UserType udt_name -> udt_name
;;

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
  | Cons -> "::"
;;

let string_of_assign_op = function
  | IdentityAssign -> " = "
  | PlusAssign -> " += "
  | MinusAssign -> " -= "
  | MultAssign -> " *= "
  | DivAssign -> " /= "
;;

let rec string_of_expr = function
  | Literal l -> string_of_int l
  | BoolLit true -> "true"
  | BoolLit false -> "false"
  | FloatLit f -> string_of_float f
  | CharLit c -> Printf.sprintf "\'%s\'" (String.make 1 c)
  | StringLit s -> Printf.sprintf "\"%s\"" s
  | Unit -> "()"
  | Id id -> id
  | Binop (e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | UnopSideEffect (id, op) ->
    (match op with
     | Preincr | Predecr -> string_of_op op ^ "" ^ id
     | Postincr | Postdecr -> id ^ "" ^ string_of_op op
     | _ -> raise (Failure "Invalid operation in UnopSideEffect"))
  | Unop (e, op) -> string_of_op op ^ "" ^ string_of_expr e
  | List elems -> "[" ^ String.concat ", " (List.map string_of_expr elems) ^ "]"
  | Tuple elems -> "(" ^ String.concat ", " (List.map string_of_expr elems) ^ ")"
  | FunctionCall (func_name, func_args) ->
    func_name ^ "(" ^ String.concat ", " (List.map string_of_expr func_args) ^ ")"
  | UDTInstance (udt_name, udt_members) ->
    udt_name ^ "{" ^ string_of_udt_instance udt_members ^ "}"
  | UDTAccess (udt_name, udt_access) -> udt_name ^ "." ^ string_of_udt_access udt_access
  | UDTStaticAccess (udt_name, udt_function) ->
    udt_name
    ^ "::"
    ^ fst udt_function
    ^ "("
    ^ String.concat ", " (List.map string_of_expr (snd udt_function))
    ^ ")"
  | Index (indexed_expr, idx) ->
    string_of_expr indexed_expr ^ "[" ^ string_of_expr idx ^ "]"
  | Match (e1, case_list) ->
    "match (" ^ string_of_expr e1 ^ ") {\n" ^ string_of_case_list case_list ^ "}"
  | Wildcard -> "_"
  | EnumAccess (enum_name, enum_variant) -> enum_name ^ "::" ^ enum_variant
  | TypeCast (type_name, e) -> string_of_expr e ^ " as " ^ string_of_type type_name

and string_of_pattern = function
  | PLiteral num -> string_of_int num
  | PBoolLit true -> "true"
  | PBoolLit false -> "false"
  | PFloatLit f -> string_of_float f
  | PCharLit c -> Printf.sprintf "\'%s\'" (String.make 1 c)
  | PStringLit s -> Printf.sprintf "\"%s\"" s
  | PId id -> id
  | PWildcard -> "_"
  | PEmptyList -> "[]"
  | PCons (pattern1, pattern2) ->
    string_of_pattern pattern1 ^ "::" ^ string_of_pattern pattern2

and string_of_case_list = function
  | [] -> "" (* empty case *)
  | hd :: tl ->
    string_of_pattern (fst hd)
    ^ " -> "
    ^ string_of_expr (snd hd)
    ^ ",\n"
    ^ string_of_case_list tl

and string_of_udt_instance = function
  | [] -> ""
  | hd :: tl -> fst hd ^ ": " ^ string_of_expr (snd hd) ^ ", " ^ string_of_udt_instance tl

and string_of_udt_access = function
  | UDTVariable udt_var -> udt_var
  | UDTFunction udt_func ->
    fst udt_func ^ "(" ^ String.concat ", " (List.map string_of_expr (snd udt_func)) ^ ")"
;;

let string_of_enum_variant = function
  | EnumVariantDefault variant_name -> variant_name
  | EnumVariantExplicit (variant_name, variant_num) ->
    variant_name ^ " = " ^ string_of_int variant_num
;;

let rec string_of_func_args = function
  | [] -> ""
  | hd :: tl -> fst hd ^ ": " ^ string_of_type (snd hd) ^ ", " ^ string_of_func_args tl
;;

let rec string_of_block = function
  | MutDeclTyped (id, typ, e) ->
    "let mut " ^ id ^ ": " ^ string_of_type typ ^ " = " ^ string_of_expr e ^ ";\n"
  | MutDeclInfer (id, e) -> "let mut " ^ id ^ " := " ^ string_of_expr e ^ ";\n"
  | DeclTyped (id, typ, e) ->
    "let " ^ id ^ ": " ^ string_of_type typ ^ " = " ^ string_of_expr e ^ ";\n"
  | DeclInfer (id, e) -> "let " ^ id ^ " := " ^ string_of_expr e ^ ";\n"
  | Assign (e1, assign_op, e2) ->
    string_of_expr e1 ^ string_of_assign_op assign_op ^ string_of_expr e2 ^ ";\n"
  | FunctionDefinition (rtyp, func_name, func_args, func_body) ->
    "fun "
    ^ func_name
    ^ "("
    ^ string_of_func_args func_args
    ^ ") -> "
    ^ string_of_type rtyp
    ^ " {\n"
    ^ String.concat "" (List.map string_of_block func_body)
    ^ "\n}\n"
  | BoundFunctionDefinition (rtyp, func_name, func_args, func_body, bound_type) ->
    "bind "
    ^ func_name
    ^ "<"
    ^ string_of_type bound_type
    ^ ">"
    ^ "("
    ^ string_of_func_args func_args
    ^ ") -> "
    ^ string_of_type rtyp
    ^ " {\n"
    ^ String.concat "" (List.map string_of_block func_body)
    ^ "\n}\n"
  | UDTDef (udt_name, udt_members) ->
    "type "
    ^ udt_name
    ^ "{\n"
    ^ string_of_func_args
        udt_members (* Re-use string_of_func_args  as it generates name: type string*)
    ^ "\n}"
  | EnumDeclaration (enum_name, enum_variants) ->
    "enum "
    ^ enum_name
    ^ " {\n"
    ^ String.concat ",\n" (List.map string_of_enum_variant enum_variants)
    ^ "\n}"
  | IfEnd (e, bl) ->
    "if ("
    ^ string_of_expr e
    ^ ") {\n"
    ^ String.concat "\n" (List.map string_of_block bl)
    ^ "\n}"
  | IfNonEnd (e, bl, nbl) ->
    "if ("
    ^ string_of_expr e
    ^ ") {\n"
    ^ String.concat "\n" (List.map string_of_block bl)
    ^ "\n} "
    ^ string_of_block nbl
  | ElifEnd (e, bl) ->
    "else if ("
    ^ string_of_expr e
    ^ ") {\n"
    ^ String.concat "\n" (List.map string_of_block bl)
    ^ "\n}"
  | ElifNonEnd (e, bl, nbl) ->
    "else if ("
    ^ string_of_expr e
    ^ ") {\n"
    ^ String.concat "\n" (List.map string_of_block bl)
    ^ "\n} "
    ^ string_of_block nbl
  | ElseEnd bl -> "else {\n" ^ String.concat "\n" (List.map string_of_block bl) ^ "\n}"
  | While (e, block_list) ->
    "while ("
    ^ string_of_expr e
    ^ ") {\n"
    ^ String.concat "" (List.map string_of_block block_list)
    ^ "\n}"
  | For (idx, it, block_list) ->
    "for "
    ^ idx
    ^ " := "
    ^ string_of_expr it
    ^ " {\n"
    ^ String.concat "" (List.map string_of_block block_list)
    ^ "\n}"
  | Break -> "break;"
  | Continue -> "continue;"
  | ReturnUnit -> "return;\n"
  | ReturnVal e -> "return " ^ string_of_expr e ^ ";\n"
  | Expr e -> string_of_expr e ^ ";\n"
;;

let string_of_program fdecl = String.concat "" (List.map string_of_block fdecl.body)
