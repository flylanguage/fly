open Ast

type sexpr = typ * sx

and sx =
  | SLiteral of int
  | SBoolLit of bool
  | SFloatLit of float
  | SCharLit of char
  | SStringLit of string
  | SUnit
  | SId of string
  | STuple of sexpr list
  | SBinop of sexpr * op * sexpr
  | SUnop of sexpr * op (* this is for not *)
  | SUnopSideEffect of string * op (* this is for postincr, postdecr, preincr, postdecr *)
  | SFunctionCall of sfunc
  | SUDTInstance of string * skv_list
  | SUDTAccess of string * sudt_access
  | SUDTStaticAccess of string * sfunc
  | SEnumAccess of string * string
  | SIndex of sexpr * sexpr
  | SList of sexpr list
  | SMatch of sexpr * (pattern * sexpr) list
  | SWildcard
  | STypeCast of typ * sexpr

and sfunc = string * sexpr list
and skv_list = (string * sexpr) list (* for user defined types *)

and sudt_access =
  | SUDTVariable of string
  | SUDTFunction of sfunc

type senum_variant =
  | SEnumVariantDefault of string
  | SEnumVariantExplicit of string * int

type sblock =
  | SMutDeclTyped of string * typ * sexpr
  | SMutDeclInfer of string * sexpr
  | SDeclTyped of string * typ * sexpr
  | SDeclInfer of string * sexpr
  | SAssign of sexpr * assign_op * sexpr
  | SFunctionDefinition of
      typ
      * string
      * (string * typ) list
      * sblock list (* rtyp, func_name, func_args, func_body *)
  | SBoundFunctionDefinition of
      typ
      * string
      * (string * typ) list
      * sblock list
      * typ (* rtyp, func_name, func_args, func_body, bound_type *)
  | SEnumDeclaration of string * enum_variant list
  | SUDTDef of string * (string * typ) list
  | SIfEnd of sexpr * sblock list
  | SIfNonEnd of sexpr * sblock list * sblock
  | SElifNonEnd of sexpr * sblock list * sblock
  | SElifEnd of sexpr * sblock list
  | SElseEnd of sblock list
  | SWhile of sexpr * sblock list
  | SFor of string * sexpr * sblock list
  | SBreak
  | SContinue
  | SReturnUnit
  | SReturnVal of sexpr
  | SExpr of sexpr

type sprogram = { body : sblock list }

(* SAST print functions *)
(* let rec string_of_sexpr (t, e) = 
  string_of_type t ^
    (match e with
    | Literal l -> string_of_int l
    | BoolLit true -> "true"
    | BoolLit false -> "false"
    | FloatLit f -> string_of_float f
    | CharLit c -> Printf.sprintf "\'%s\'" (String.make 1 c)
    | StringLit s -> Printf.sprintf "\"%s\"" s
    | Unit -> "()"
    | Id id -> id
    | Binop (e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
    | UnopSideEffect (id, op) -> 
      
      ( match op with
        Preincr | Predecr -> string_of_op op ^ "" ^ id
        | Postincr| Postdecr -> id ^ "" ^ string_of_op op
        | _ -> raise (Failure "Invalid operation in UnopSideEffect"))
      
    | Unop (e, op) -> string_of_op op ^ "" ^ string_of_sexpr e
    | List elems -> "[" ^ String.concat ", " (List.map string_of_sexpr elems) ^ "]"
    | Tuple elems -> "(" ^ String.concat ", " (List.map string_of_sexpr elems) ^ ")"
    | FunctionCall (func_name, func_args) ->
        func_name ^ "("
        ^ String.concat ", " (List.map string_of_sexpr func_args)
        ^ ")"
    | UDTInstance (udt_name, udt_members) -> udt_name ^ "{" ^ string_of_udt_instance udt_members ^ "}"
    | UDTAccess (udt_name, udt_access) -> udt_name ^ "." ^ string_of_udt_access udt_access
    | UDTStaticAccess (udt_name, udt_function) -> 
        udt_name ^ "::" ^ (fst udt_function) ^ "(" ^ (String.concat ", " (List.map string_of_sexpr (snd udt_function))) ^ ")"
    | Index (indexed_expr, idx) -> string_of_sexpr indexed_expr ^ "[" ^ string_of_sexpr idx ^ "]"
    | Match (e1, case_list) -> "match (" ^ string_of_sexpr e1 ^ ") {\n" ^ string_of_case_list case_list ^ "}"
    | Wildcard -> "_"
    | EnumAccess (enum_name, enum_variant) -> enum_name ^ "::" ^ enum_variant
    | TypeCast (type_name, e) ->  string_of_sexpr e ^ " as " ^ string_of_type type_name)
  and string_of_pattern = function
    | PLiteral ( num ) -> string_of_int num
    | PBoolLit true -> "true"
    | PBoolLit false -> "false"
    | PFloatLit f -> string_of_float f
    | PCharLit c -> Printf.sprintf "\'%s\'" (String.make 1 c)
    | PStringLit s -> Printf.sprintf "\"%s\"" s
    | PId (id) -> id
    | PWildcard -> "_"
    | PEmptyList -> "[]"
    | PCons (pattern1, pattern2) -> string_of_pattern pattern1 ^ "::" ^ string_of_pattern pattern2
  and string_of_case_list = function 
    [] -> "" (* empty case *)
    | hd :: tl -> string_of_pattern (fst hd) ^ " -> " ^ string_of_sexpr (snd hd) ^ ",\n" ^ string_of_case_list tl

  and string_of_udt_instance = function
    [] -> ""
    | hd :: tl -> (fst hd) ^ ": " ^ string_of_sexpr (snd hd) ^ ", " ^ string_of_udt_instance tl
  and string_of_udt_access = function
    | UDTVariable (udt_var) -> udt_var
    | UDTFunction (udt_func) -> (fst udt_func) ^ "("
      ^ String.concat ", " (List.map string_of_sexpr (snd udt_func))
      ^ ")"


  let rec string_of_sblock = function
    | MutDeclTyped (id, typ, e) -> "let mut " ^ id ^ ": " ^ string_of_type typ ^ " = " ^ string_of_sexpr e ^ ";\n"
    | MutDeclInfer (id, e) ->  "let mut " ^ id ^ " := " ^ string_of_sexpr e ^ ";\n"
    | DeclTyped (id, typ, e) -> "let " ^ id ^ ": " ^ string_of_type typ ^ " = " ^ string_of_sexpr e ^ ";\n"
    | DeclInfer (id, e) -> "let " ^ id ^ " := " ^ string_of_sexpr e ^ ";\n"
    | Assign (e1, assign_op, e2) -> string_of_sexpr e1 ^ string_of_assign_op assign_op ^ string_of_sexpr e2 ^ ";\n"
    | FunctionDefinition  (rtyp, func_name, func_args, func_body) ->
      "fun " ^ func_name  ^ "(" ^  string_of_func_args func_args ^ ") -> " ^ string_of_type rtyp ^ " {\n"
      ^ String.concat "" (List.map string_of_block func_body)
      ^ "\n}\n"
    | BoundFunctionDefinition (rtyp, func_name, func_args, func_body, bound_type) -> 
      "bind " ^ func_name  ^ "<" ^ string_of_type bound_type ^ ">" ^ "(" ^  string_of_func_args func_args ^ ") -> " ^ string_of_type rtyp ^ " {\n"
      ^ String.concat "" (List.map string_of_sblock func_body)
      ^ "\n}\n"
    | UDTDef (udt_name, udt_members) -> 
      "type " ^ udt_name ^ "{\n"
      ^ string_of_func_args udt_members (* Re-use string_of_func_args  as it generates name: type string*)
      ^ "\n}"
    | EnumDeclaration (enum_name, enum_variants) ->
      "enum " ^ enum_name ^ " {\n" 
      ^ String.concat ",\n" (List.map string_of_enum_variant enum_variants)
      ^ "\n}"
    | IfEnd (e, bl) ->
      "if (" ^ string_of_sexpr e ^ ") {\n"
      ^ String.concat "\n" (List.map string_of_sblock bl)
      ^ "\n}"
    | IfNonEnd (e, bl, nbl) ->
      "if (" ^ string_of_sexpr e ^ ") {\n"
      ^ String.concat "\n" (List.map string_of_sblock bl) 
      ^ "\n} "
      ^ string_of_sblock nbl
    | ElifEnd (e, bl) ->
      "else if (" ^ string_of_sexpr e ^ ") {\n"
      ^ String.concat "\n" (List.map string_of_sblock bl)
      ^ "\n}"
    | ElifNonEnd (e, bl, nbl) ->
      "else if (" ^ string_of_sexpr e ^ ") {\n"
      ^ String.concat "\n" (List.map string_of_sblock bl)
      ^ "\n} "
      ^ string_of_sblock nbl
    | ElseEnd bl ->
      "else {\n" ^ String.concat "\n" (List.map string_of_sblock bl) ^ "\n}"
    | While (e, block_list) -> 
      "while (" ^ string_of_sexpr e ^ ") {\n"
      ^ String.concat "" (List.map string_of_sblock block_list)
      ^ "\n}"
    | For (idx, it, block_list) -> 
        "for " ^ idx ^ " := " ^ string_of_sexpr it ^ " {\n"
        ^ String.concat "" (List.map string_of_sblock block_list)
        ^ "\n}"
    | Break -> "break;"
    | Continue -> "continue;"
    | ReturnUnit -> "return;\n"
    | ReturnVal (e) -> "return " ^ string_of_sexpr e ^ ";\n"
    | Expr (e) -> string_of_sexpr e ^ ";\n"


  let string_of_sprogram fdecl = String.concat "" (List.map string_of_sblock fdecl.body) *)
