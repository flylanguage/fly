open Ast
open Sast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(* Type definitions for function signatures, user-defined types, and enums *)
type func_sig = {
  args: (string * typ) list;
  rtyp: typ;
}

type udt_def = {
  members: (string * typ) list;
  methods: string list;
}

type enum_def = (string * int) list 

type envs = {
  var_env:  typ StringMap.t;
  func_env: func_sig StringMap.t;
  udt_env:  udt_def StringMap.t;
  enum_env: enum_def StringMap.t;
}

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

and check_binop e1 binop e2 envs special_blocks = 
  let t1, e1' = check_expr e1 envs special_blocks in
  let t2, e2' = check_expr e2 envs special_blocks in
  let rtype = get_binop_return_type t1 binop t2 in
  (rtype, SBinop((t1, e1'), binop, (t2, e2')))

and get_binop_return_type t1 binop t2 = 
  match binop with
  | Add | Sub | Mult | Div | Mod | Exp -> (
    match (t1, t2) with
    | Int, Int -> Int
    | Float, Float -> Float
    | _, _ -> raise (Failure("Invalid types for arithmetic operation"))
  )
  | Equal | Neq -> (
    if t1 = t2 then Bool
    else raise (Failure("Invalid types for checking equality"))
  )
  | Less | Leq | Greater | Geq -> (
    match (t1, t2) with
    | Int, Int -> Bool
    | Float, Float -> Bool
    | _, _ -> raise (Failure("Invalid types for checking equality"))
  )
  | And | Or  -> (
    match (t1, t2) with
    | Bool, Bool -> Bool
    | _, _ -> raise (Failure("Invalid types for logical operator"))
  )
  | Cons -> (
    match (t1, t2) with
    | t1', List t2' when t1' = t2' -> List t1' 
    | _, _ -> raise (Failure ("Invalid types for appending to list"))
  )
  | _ -> raise (Failure("Invalid binary operator"))

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

  | List expr_list ->
    let sexpr_list = List.map (fun e -> check_expr e envs special_blocks) expr_list in
    let (typs, sexprs) = List.split sexpr_list in
    begin match typs with 
    | [] -> raise (Failure("Cannot infer type on empty list")) (* TODO: Allow empty lists *)
    | first_typ :: rest -> 
      if List.for_all (fun x -> x = first_typ) rest then
        (List first_typ, SList sexpr_list)
      else
        raise (Failure ("Lists must only have 1 type!"))
    end

  | UDTInstance (udt_name, udt_members) -> 
      let udt_def = find_udt udt_name envs.udt_env in (* udt_def is a (string * typ) list *)
      let (def_names, def_types) = List.split udt_def.members in
      let (instance_names, instance_exprs) = List.split udt_members in
      if def_names = instance_names then (* this means the order of members in the instance must in the same order as definition *)
        let sexpr_list = List.map (fun e -> check_expr e envs special_blocks) instance_exprs in
        let (instance_types, _ ) = List.split sexpr_list in
        if def_types = instance_types then
          let skv_list = List.combine instance_names sexpr_list in
          (UserType(udt_name), SUDTInstance (udt_name, skv_list))
        else
          raise (Failure("Incorrect types used to instantiate " ^ udt_name))
      else
        raise (Failure("Incorrect ordering when instantiating " ^ udt_name))

  | Binop (e1, binop, e2) -> check_binop e1 binop e2 envs special_blocks

  | Unop (e, unop) -> 
    let (t, e') = check_expr e envs special_blocks in
    if t <> Bool then
      raise (Failure ("Trying to do NOT on a non-boolean expression!"))
    else
      (Bool, SUnop((t, e'), unop))

  | UnopSideEffect (id_name, side_effect_op) -> 
    let typ = find_var id_name envs.var_env in
    if typ = Int || typ = Float then
      (typ, SUnopSideEffect (id_name, side_effect_op))
    else
      raise (Failure ("Trying to do increment/decrement on a non-numeric expression!"))

  | FunctionCall (func_name, func_args) -> 
    let sfunc_args = List.map (fun arg -> check_expr arg envs special_blocks) func_args in
    let t = find_func func_name envs.func_env in (* t is return type of this function call *)
    (t.rtyp, SFunctionCall (func_name, sfunc_args))


  | UDTAccess (id_name, udt_accessed_member) -> 
    let udt_typ = find_var id_name envs.var_env in
    let udt_def = find_udt (string_of_type udt_typ) envs.udt_env in
    begin match udt_accessed_member with
    | UDTVariable udt_var -> 
      begin match (List.assoc_opt udt_var udt_def.members) with
      | Some accessed_type -> (accessed_type, SUDTAccess (id_name, SUDTVariable udt_var))
      | None -> raise (Failure(udt_var ^ "is not in " ^ string_of_type udt_typ))
      end
    | UDTFunction udt_func -> 
      begin match (List.find_opt (fun x -> x = fst udt_func) udt_def.methods) with
      | Some _ -> 
        let func_sig = find_func (fst udt_func) envs.func_env in
        let _, def_arg_types = List.split func_sig.args in
        let sexpr_list = List.map (fun e -> check_expr e envs special_blocks) (snd udt_func) in
        let arg_types, _  = List.split sexpr_list in
        if arg_types = def_arg_types then
          (func_sig.rtyp, SUDTAccess (id_name, SUDTFunction (fst udt_func, sexpr_list)))
        else
          raise (Failure("Incorrect types passed to this method"))
      | None -> raise (Failure((fst udt_func) ^ "is not a method bound to " ^ string_of_type udt_typ))
      end
    end


  | UDTStaticAccess (udt_name, (func_name, args)) -> 
    let sfunc_args = List.map (fun arg -> check_expr arg envs special_blocks) args in
    (new_type, SUDTStaticAccess (udt_name, func_name, sfunc_args))

  | EnumAccess (enum_name, variant) -> 
    let t = find_enum enum_name envs.enum_env in
    (t, SEnumAccess (enum_name, variant))

  | Index (e1, e2) -> 
    let et1 = check_expr e1 envs special_blocks in
    let et2 = check_expr e2 envs special_blocks in
    (new_type, SIndex (et1, et2))



  | Match (matched_expr, match_arms) -> 
    let s_matched = check_expr matched_expr envs special_blocks in
    let annotated_arms = List.map (fun (pattern, arm_expr) ->
      let updated_special_blocks = StringSet.add "wildcard" special_blocks in
      let s_arm_expr = check_expr arm_expr envs updated_special_blocks in
      (pattern, s_arm_expr)
    ) match_arms in
    (result_type, SMatch (s_matched, annotated_arms))

  | Wildcard -> 
    if StringSet.mem "wildcard" special_blocks then
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
  check_block_list initial_envs special_blocks