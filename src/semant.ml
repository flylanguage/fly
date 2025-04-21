open Ast
open Sast

module StringMap = Map.Make(String)
(* A list that contains mappings of type variables to a primitive type *)
(* typ is defined in Ast and can refer to Int, Bool, Char, Float, String, List<>, Tuple<>, Unit, UserType *)
type substitutions = (string * typ) list 

let type_variable = ref (Char.code 'a')

(* generates a new unknown type placeholder.
   returns T(string) of the generated alphabet *)
let gen_new_type () =
  let type_var = !type_variable in
  incr type_variable; TypeVar (Char.escaped (Char.chr type_var))

(* ref is ocaml syntax for pointers. See here for more: https://cs3110.github.io/textbook/chapters/mut/refs.html*)
(* Char.code 'a' returns the ASCII code of 'a' *)
(* type_variable is used for variables whose types are not explicity specified for the user, and require inference *)
let rec find_symbol v env =
  try StringMap.find v env 
  with Not_found -> 
    raise (Failure ("Undeclared identifier " ^ v))

and annotate_expr expr env = 
  match expr with 
  | Literal i -> (Int, SLiteral i)
  | BoolLit b -> (Bool, SBoolLit b)
  | FloatLit f -> (Float, SFloatLit f)
  | CharLit c -> (Char, SCharLit c)
  | StringLit s -> (String, SStringLit s)
  | Unit -> (Unit, SUnit)

  | Id id_name -> 
    let t = find_symbol id_name env in
    (t, SId id_name)

  | Tuple expr_list -> 
    let sexpr_list = List.map (fun e -> annotate_expr e env) expr_list in 
    let (typs, sexprs) = List.split sexpr_list in
    (Tuple typs, STuple sexprs)

  | Binop (e1, binop, e2) -> 
    let et1 = annotate_expr e1 env in
    let et2 = annotate_expr e2 env in
    let new_type = gen_new_type () in
    (new_type, SBinop (et1, binop, et2))

  | Unop (e, unop) -> 
    let et = annotate_expr e env in
    let new_type = gen_new_type () in
    (new_type, SUnop (et, unop))

  | UnopSideEffect (id_name, side_effect_op) -> 
    let typ = find_symbol id_name env in
    (typ, SUnopSideEffect (id_name, side_effect_op))

  | FunctionCall (func_name, func_args) -> 
    let sfunc_args = List.map (fun arg -> annotate_expr arg env) func_args in
    let t = find_symbol func_name env in
    (t, SFunctionCall (func_name, sfunc_args))

  | UDTInstance (udt_name, udt_members) -> 
    let sexpr_list = List.map (fun e -> annotate_expr e env) udt_members in
    let typ = find_symbol udt_name env in
    (typ, SUDTInstance (udt_name, List.map snd sexpr_list))

  | UDTAccess (id_name, udt_accessed_member) -> 
    let typ = find_symbol id_name env in
    let new_type = gen_new_type () in
    begin match udt_accessed_member with
    | UDTVariable member_name ->
        (new_type, SUDTAccess (id_name, SUDTVariable member_name))
    | UDTFunction func_args ->
        let sfunc_args = List.map (fun arg -> annotate_expr arg env) func_args in
        (new_type, SUDTAccess (id_name, SUDTFunction sfunc_args))
    end

  | UDTStaticAccess (udt_name, func_name, args) -> 
    let sfunc_args = List.map (fun arg -> annotate_expr arg env) args in
    let new_type = gen_new_type () in
    (new_type, SUDTStaticAccess (udt_name, func_name, sfunc_args))

  | EnumAccess (enum_name, variant) -> 
    let t = find_symbol enum_name env in
    (t, SEnumAccess (enum_name, variant))

  | Index (e1, e2) -> 
    let et1 = annotate_expr e1 env in
    let et2 = annotate_expr e2 env in
    let new_type = gen_new_type () in
    (new_type, SIndex (et1, et2))

  | List expr_list ->
    let sexpr_list = List.map (fun e -> annotate_expr e env) expr_list in
    let (typs, sexprs) = List.split sexpr_list in
    let list_type = gen_new_type () in
    (List list_type, SList sexprs)

  | Match (matched_expr, match_arms) -> 
    let s_matched = annotate_expr matched_expr env in
    let annotated_arms = List.map (fun (pattern, arm_expr) ->
      let s_arm_expr = annotate_expr arm_expr env in
      (pattern, s_arm_expr)
    ) match_arms in
    let result_type = gen_new_type () in
    (result_type, SMatch (s_matched, annotated_arms))

  | Wildcard -> 
    let wildcard_type = gen_new_type () in
    (wildcard_type, SWildcard)

  | TypeCast (target_typ, e) ->
    let (_, sexpr) = annotate_expr e env in
    (target_typ, STypeCast (target_typ, sexpr))


and annotate_block block env = 
  match block with
  | MutDeclTyped (var_name, t, e) ->
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
  | ReturnVal (return_expr) -> 
    let (return_type, return_sexpr) = annotate_expr return_expr in
    (return_type, SReturnVal(return_sexpr))
  | Expr (expr) -> 
    let (typ, sexpr) = annotate_expr expr in
    (typ, SExpr(sexpr))

let rec annotate_block_list block_list env =
  match blk_list with 
  | [] -> []
  | curr_block :: rest -> annotate_block curr_block env :: annotate_block_list rest env

let check block_list = 
  let initial_env = StringMap.empty in
  let annotated_block_list = annotate_block_list block_list initial_env in
  let constraints = collect_block_list annotated_block_list in
  let sblock_list = unify constraints in
  sblock_list