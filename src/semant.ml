open Ast
open Sast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(* Type definitions for function signatures, user-defined types, and enums *)
type func_sig = {
  args: (string * typ) list;
  rtyp: typ;
}

type udt_def = (string * typ) list

type enum_def = (string * int) list 

type envs = {
  var_env:  typ StringMap.t;
  func_env: func_sig StringMap.t;
  udt_env:  udt_def StringMap.t;
  enum_env: enum_def StringMap.t;
}

(* ref is ocaml syntax for pointers. See here for more: https://cs3110.github.io/textbook/chapters/mut/refs.html*)
(* Char.code 'a' returns the ASCII code of 'a' *)
(* type_variable is used for variables whose types are not explicity specified for the user, and require inference *)
let rec find_var var_name env =
  try StringMap.find var_name env 
  with Not_found -> 
    raise (Failure ("Undeclared variable " ^ var_name))

and find_func func_name env =
  try StringMap.find func_name env
  with Not_found -> raise (Failure ("Undefined function " ^ func_name))

and find_udt udt_name env = 
  try StringMap.find udt_name env
  with Not_found -> raise (Failure ("Undefined user type " ^ udt_name))

and find_enum enum_name env =
  try StringMap.find enum_name env
  with Not_found -> raise (Failure ("Undefined enum " ^ enum_name))

(* Arguments: variable name, variable type, environments *)
and var_dec_helper var_name t envs =
  let open StringMap in 
  let env_checks = [
    mem var_name envs.var_env;
    mem var_name envs.func_env;
    mem var_name envs.udt_env;
    mem var_name envs.enum_env;
  ] in
  if List.exists (fun x -> x) env_checks then
    raise (Failure(var_name ^ "already exists!"))
  else
    StringMap.add var_name t envs.var_env

and udt_def_helper udt_name udt_members envs =
  let open StringMap in 
  let env_checks = [
    mem udt_name envs.var_env;
    mem udt_name envs.func_env;
    mem udt_name envs.udt_env;
    mem udt_name envs.enum_env;
  ] in
  if List.exists (fun x -> x) env_checks then
    raise (Failure(udt_name ^ "already exists!"))
  else
    StringMap.add udt_name udt_members envs.udt_env

and enum_dec_helper enum_name enum_variants envs =
  let open StringMap in 
  let env_checks = [
    mem enum_name envs.var_env;
    mem enum_name envs.func_env;
    mem enum_name envs.udt_env;
    mem enum_name envs.enum_env;
  ] in
  if List.exists (fun x -> x) env_checks then
    raise (Failure(enum_name ^ "already exists!"))
  else
    StringMap.add enum_name enum_variants envs.udt_env

and check_expr expr envs special_blocks = 
  match expr with 
  | Literal i -> (Int, SLiteral i)
  | BoolLit b -> (Bool, SBoolLit b)
  | FloatLit f -> (Float, SFloatLit f)
  | CharLit c -> (Char, SCharLit c)
  | StringLit s -> (String, SStringLit s)
  | Unit -> (Unit, SUnit)
  | Id id_name -> 
    let t = find_var id_name envs.var_env in
    (t, SId id_name)

  | Tuple expr_list -> 
    let sexpr_list = List.map (fun e -> check_expr e envs special_blocks) expr_list in 
    let (typs, sexprs) = List.split sexpr_list in
    (Tuple typs, STuple sexpr_list)

  | Binop (e1, binop, e2) -> 
    let et1 = check_expr e1 envs special_blocks in
    let et2 = check_expr e2 envs special_blocks in
    let new_type = gen_new_type () in
    (new_type, SBinop (et1, binop, et2))

  | Unop (e, unop) -> 
    let et = check_expr e envs special_blocks in
    let new_type = gen_new_type () in
    (new_type, SUnop (et, unop))

  | UnopSideEffect (id_name, side_effect_op) -> 
    let typ = find_var id_name envs.var_env in
    (typ, SUnopSideEffect (id_name, side_effect_op))

  | FunctionCall (func_name, func_args) -> 
    let sfunc_args = List.map (fun arg -> check_expr arg envs special_blocks) func_args in
    let t = find_func func_name envs.func_env in (* t is return type of this function call *)
    (t.rtyp, SFunctionCall (func_name, sfunc_args))

  | UDTInstance (udt_name, udt_members) -> 
    let udt_def = find_udt udt_name envs.udt_env in (* udt_def is a (string * typ) list *)
    let (def_names, _) = List.split udt_def in
    let (instance_names, instance_exprs) = List.split udt_members in
    if def_names = instance_names then (* this means the order of members in the instance must in the same order as definition *)
      let sexpr_list = List.map (fun e -> check_expr e envs special_blocks) instance_exprs in
      let skv_list = List.combine instance_names sexpr_list in
      (UserType(udt_name), SUDTInstance (udt_name, skv_list))
  else
    raise (Failure("Incorrect instantiation of " ^ udt_name))


  | UDTAccess (id_name, udt_accessed_member) -> 
    let udt_typ = find_var id_name envs.var_env in
    begin match StringMap.find_opt udt_typ envs.udt_env with
    | Some(udt_def) ->
    | None(_) -> 
    
    if find_udt udt_typ envs.udt_env then
      begin match udt_accessed_member with
      | UDTVariable member_name ->
        if find_udt 
          (new_type, SUDTAccess (id_name, SUDTVariable member_name))
      | UDTFunction func_args ->
          let sfunc_args = List.map (fun arg -> check_expr arg envs special_blocks) func_args in
          (new_type, SUDTAccess (id_name, SUDTFunction sfunc_args))
      end
    else  
      let new_type = gen_new_type () in
      begin match udt_accessed_member with
      | UDTVariable member_name ->
          (new_type, SUDTAccess (id_name, SUDTVariable member_name))
      | UDTFunction func_args ->
          let sfunc_args = List.map (fun arg -> check_expr arg envs special_blocks) func_args in
          (new_type, SUDTAccess (id_name, SUDTFunction sfunc_args))
      end

  | UDTStaticAccess (udt_name, (func_name, args)) -> 
    let sfunc_args = List.map (fun arg -> check_expr arg envs special_blocks) args in
    let new_type = gen_new_type () in
    (new_type, SUDTStaticAccess (udt_name, func_name, sfunc_args))

  | EnumAccess (enum_name, variant) -> 
    let t = find_enum enum_name envs.enum_env in
    (t, SEnumAccess (enum_name, variant))

  | Index (e1, e2) -> 
    let et1 = check_expr e1 envs special_blocks in
    let et2 = check_expr e2 envs special_blocks in
    let new_type = gen_new_type () in
    (new_type, SIndex (et1, et2))

  | List expr_list ->
    let sexpr_list = List.map (fun e -> check_expr e envs special_blocks) expr_list in
    let (typs, sexprs) = List.split sexpr_list in
    let list_type = gen_new_type () in
    (List list_type, SList sexprs)

  | Match (matched_expr, match_arms) -> 
    let s_matched = check_expr matched_expr envs special_blocks in
    let annotated_arms = List.map (fun (pattern, arm_expr) ->
      let updated_special_blocks = StringSet.add "wildcard" special_blocks in
      let s_arm_expr = check_expr arm_expr envs updated_special_blocks in
      (pattern, s_arm_expr)
    ) match_arms in
    let result_type = gen_new_type () in
    (result_type, SMatch (s_matched, annotated_arms))

  | Wildcard -> 
    if StringSet.mem "wildcard" special_blocks then
      let wildcard_type = gen_new_type () in
      (wildcard_type, SWildcard)
    else
      raise (Failure ("Unallowed wildcard"))

  | TypeCast (target_typ, e) ->
    (* we can throw away the type returned in this scenario because we know target_typ is the exact type we are trying to cast to *)
    let (_, sexpr) = check_expr e envs special_blocks in
    (target_typ, STypeCast (target_typ, sexpr))


and check_block block envs special_blocks = 
  match block with
  | MutDeclTyped (var_name, t, e) -> 
    let annotated_e = check_expr e envs special_blocks in
    let (typ, sexpr) =  annotated_e in
    if typ <> t then
      raise (Failure (var_name ^ " is supposed to have type " ^ t ^ " but expression has type " ^ typ))
    else
      let new_var_env = var_dec_helper var_name t envs in
      let updated_envs = { envs with var_env = new_var_env } in
      (updated_envs, SMutDeclTyped (var_name, t, annotated_e))

  | MutDeclInfer (var_name, e) -> 
    let annotated_e = check_expr e envs special_blocks in
    let (typ, sexpr) = annotated_e in
    let new_var_env = var_dec_helper var_name typ envs in
    let updated_envs = { envs with var_env = new_var_env } in
    (updated_envs, SMutDeclInfer(var_name, annotated_e))

  | DeclTyped (var_name, t, e) -> 
    let annotated_e = check_expr e envs special_blocks in
    let (typ, sexpr) =  annotated_e in
    if typ <> t then
      raise (Failure (var_name ^ " is supposed to have type " ^ t ^ " but expression has type " ^ typ))
    else
      let new_var_env = var_dec_helper var_name t envs in
      let updated_envs = { envs with var_env = new_var_env } in
      (updated_envs, SDeclTyped (var_name, t, annotated_e))
    
  | DeclInfer (var_name, e) -> 
    let annotated_e = check_expr e envs special_blocks in
    let (typ, sexpr) = annotated_e in
    let new_var_env = var_dec_helper var_name typ envs in
    let updated_envs = { envs with var_env = new_var_env } in
    (updated_envs, SDeclInfer(var_name, annotated_e))

  | Assign (e1, op, e2) -> 
    let annotated_e1 = check_expr e1 envs special_blocks in
    let annotated_e2 = check_expr e2 envs special_blocks in
    (envs, SAssign(annotated_e1, op, annotated_e2))

  (* | FunctionDefinition (rtyp, func_name, func_args, func_body) -> 
    
  | BoundFunctionDefinition (rtyp, func_name, func_args, func_body, bound_type) ->  *)

  | EnumDeclaration (enum_name, enum_variants) -> 
    let new_enum_env = enum_dec_helper enum_name enum_variants envs in
    let updated_envs = { envs with enum_env = new_enum_env } in
    (updated_envs, SEnumDeclaration(enum_name, enum_variants) )

  | UDTDef (udt_name, udt_members) -> 
    let new_udt_env = udt_def_helper udt_name udt_members envs in
    let updated_envs = { envs with udt_env = new_udt_env } in
    (updated_envs, SUDTDef(udt_name, udt_members))

  | IfEnd (condition, body) -> 
    let annotated_condition = check_expr condition envs special_blocks in
    let annotated_body = check_block_list body envs special_blocks in
    ( envs, SIfEnd (annotated_condition, annotated_body ))

  | IfNonEnd (condition, body, other_arm) -> 
    let annotated_condition = check_expr condition envs special_blocks in
    let annotated_body = check_block_list body envs special_blocks in
    let annotated_other_arm = check_block other_arm envs special_blocks in
    ( envs, SIfEnd (annotated_condition, annotated_body, annotated_other_arm ))

  | ElifNonEnd (condition, body, other_arm) ->
    let annotated_condition = check_expr condition envs special_blocks in
    let annotated_body = check_block_list body envs special_blocks in
    let annotated_other_arm = check_block other_arm envs special_blocks in
    ( envs, SElifNonEnd (annotated_condition, annotated_body, annotated_other_arm ))

  | ElifEnd (condition, body) ->
    let annotated_condition = check_expr condition envs special_blocks in
    let annotated_body = check_block_list body envs special_blocks in
    ( envs, SElifEnd (annotated_condition, annotated_body))

  | ElseEnd (body) -> 
    let annotated_body = check_block_list body envs special_blocks in
    ( envs, SElseEnd (annotated_body))

  | While (condition, body) -> 
    let annotated_condition = check_expr condition envs special_blocks in
    let updated_special_blocks = StringSet.add "break" (StringSet.add "continue" (StringSet.add "return" special_blocks)) in
    let annotated_body = check_block_list body envs updated_special_blocks in
    (envs, SWhile (annotated_condition, annotated_body))

  | For (index_var, iterator, body) ->
    let annotated_condition = check_expr condition envs special_blocks in
    let updated_special_blocks = StringSet.add "break" (StringSet.add "continue" (StringSet.add "return" special_blocks)) in
    let annotated_body = check_block_list body envs updated_special_blocks in
    (envs, SWhile (annotated_condition, annotated_body)) 
  
  | Break -> 
    if StringSet.mem "break" then
      (envs, SBreak)
    else
      raise (Failure ("Unallowed break statement")) 

  | Continue ->
    if StringSet.mem "continue" then
      (envs, SContinue)
    else
      raise (Failure ("Unallowed continue statement")) 

  | ReturnUnit ->     
    if StringSet.mem "return" then
      (envs, SReturnUnit)
    else
      raise (Failure ("Unallowed return statement")) 

  | ReturnVal (return_expr) -> 
    if StringSet.mem "return" then
      (envs, SReturnVal (check_expr return_expr envs special_blocks))
    else
      raise (Failure ("Unallowed return statement")) 

  | Expr (expr) -> SExpr(check_expr envs special_blocks)

let rec check_block_list block_list envs special_blocks =
  match block_list with 
  | [] -> []
  | curr_block :: rest -> 
    let (updated_envs, sblock) = 
      check_block curr_block envs special_blocks
    in
    sblock :: check_block_list rest updated_envs special_blocks

let check block_list = 
  (* 4 separate hash maps for variables, function definitions, user defined types and enums *)
  let initial_envs: envs = {
    var_env = StringMap.empty;
    func_env = StringMap.empty;
    udt_env = StringMap.empty;
    enum_env = StringMap.empty;
  } in
  (* Special blocks are limited to return, continue, break, wildcard.
   We need this to indicate whether these symbols are allowed in their current context.
   For example, a return is only allowed inside a function defintion and a break is only allowed inside a loop 
   I should really come up with a better name for this *)
  let special_blocks = StringSet.empty in
  check_block_list initial_envs special_blocks in