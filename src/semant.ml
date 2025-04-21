open Ast
open Sast

module StringMap = Map.Make(String)
(* A list that contains mappings of type variables to a primitive type *)
(* typ is defined in Ast and can refer to Int, Bool, Char, Float, String, List<>, Tuple<>, Unit, UserType *)
type substitutions = (string * typ) list 

(* ref is ocaml syntax for pointers. See here for more: https://cs3110.github.io/textbook/chapters/mut/refs.html*)
(* Char.code 'a' returns the ASCII code of 'a' *)
(* type_variable is used for variables whose types are not explicity specified for the user, and require inference *)
let type_variable = ref (Char.code 'a')

let gen_new_type =
  let new_type_var = !type_variable in
  incr type_variable;
  Char.escaped (Char.chr new_type_var)

let rec annotate_expr expr = 
  match expr with 
  | Literal (i) -> (Int, SLiteral (i))
  | BoolLit (b) -> (Bool, SBoolLit (b))
  | FloatLit (f) -> (Float, SFloatLit (f))
  | CharLit (c) -> (Char, SCharLit (c))
  | StringLit (s) -> (String, SStringLit (s))
  | Unit -> (Unit, SUnit)
  | Id (id_name) -> 
  | Tuple (expr_list) -> 
  | Binop (e1, binop, e2) -> 
  | Unop (e, unop) -> 
  | UnopSideEffect (id_name, side_effect_op) -> 
  | FunctionCall (func_name, func_args) -> 
  | UDTInstance (udt_name, udt_members) -> 
  | UDTAccess (id_name, udt_accessed_member) -> 
    match udt_accessed_member with
    | UDTVariable (udt_member_name) ->
    | UDTFunction (udt_bound_function) ->
  | UDTStaticAccess (udt_name, udt_static_func_name, udt_static_func_args) -> 
  | EnumAccess (enum_name, enum_variant) -> 
  | Index (indexed_expr, calculate_index) -> 
  | List (expr_list) -> 
  | Match (matched_expr, match_arms) -> of expr * (pattern * expr) list
  | Wildcard -> 
  | TypeCast -> (target_typ, e)

let rec annotate_block block = 
  | MutDeclTyped (var_name, t, e) -> SMutDeclTyped (var_name, t, annotate_expr e)
  | MutDeclInfer (var_name, e) -> 
  | DeclTyped (var_name, t, e) -> 
  | DeclInfer (var_name, e) -> 
  | Assign (e1, op, e2) -> 
  | FunctionDefinition (rtyp, func_name, func_args, func_body) -> 
  | BoundFunctionDefinition (rtyp, func_name, func_args, func_body, bound_type) -> 
  | EnumDeclaration (enum_name, enum_variants)
  | UDTDef (udt_name, udt_members)
  | IfEnd (condition, body)
  | IfNonEnd (condition, body, other_arm) 
  | ElifNonEnd (condition, body, other_arm)
  | ElifEnd (condition, body)
  | ElseEnd (body)
  | While (condition, body)
  | For (index_var, iterator, body)
  | Break -> SBreak
  | Continue -> SContinue
  | ReturnUnit -> SReturnUnit
  | ReturnVal (return_expr) -> annotate_expr return_expr
  | Expr (expr) -> annotate_expr expr

let rec annotate_block_list block_list env =
  match blk_list with 
  | [] -> []
  | curr_block :: rest -> annotate_block curr_block :: annotate_block_list rest env

let check block_list = 
  let annotated_block_list = annotate_block_list block_list in
  let constraints = collect_block_list annotated_block_list in
  let sblock_list = unify constraints in
  sblock_list