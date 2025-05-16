open Sast
module StringSet = Set.Make (String)

let rec add_func_args args vars =
  match args with
  | [] -> vars
  | curr :: rest ->
    let new_vars = StringSet.add (fst curr) vars in
    add_func_args rest new_vars

and fresh_var_name base_name vars counter =
  let candidate = base_name ^ string_of_int counter in
  if StringSet.mem candidate vars
  then fresh_var_name base_name vars (counter + 1)
  else candidate

and unbind_sexpr se replace_self new_var_name =
  let t', se' = se in
  match se' with
  | SLiteral _
  | SBoolLit _
  | SFloatLit _
  | SCharLit _
  | SStringLit _
  | SUnit
  | SEnumAccess _ -> t', se'
  | SUnopSideEffect _ | SMatch _ | SWildcard -> failwith "Dropping"
  | SBinop (se1, binop, se2) ->
    let unbound_se1 = unbind_sexpr se1 replace_self new_var_name in
    let unbound_se2 = unbind_sexpr se2 replace_self new_var_name in
    t', SBinop (unbound_se1, binop, unbound_se2)
  | SUnop (se, unop) ->
    let unbound_se = unbind_sexpr se replace_self new_var_name in
    t', SUnop (unbound_se, unop)
  | SFunctionCall (func_name, func_args) ->
    let unbound_args =
      List.map (fun arg -> unbind_sexpr arg replace_self new_var_name) func_args
    in
    t', SFunctionCall (func_name, unbound_args)
  | SId id -> 
    if replace_self && id = "self"
    then
      t', SId new_var_name
    else t', SId id
  | STuple se_list ->
    let unbound_se_list =
      List.map (fun elem -> unbind_sexpr elem replace_self new_var_name) se_list
    in
    t', STuple unbound_se_list
  | SUDTInstance (udt_name, udt_members) ->
    let unbound_udt_members =
      List.map
        (fun member -> fst member, unbind_sexpr (snd member) replace_self new_var_name)
        udt_members
    in
    t', SUDTInstance (udt_name, unbound_udt_members)
  | SUDTAccess (udt_se, udt_member) ->
    let unbound_udt_se = unbind_sexpr udt_se replace_self new_var_name in
    (match udt_member with
     | SUDTVariable x -> t', SUDTAccess (unbound_udt_se, SUDTVariable x)
     | SUDTFunction (udt_func_name, udt_func_params) ->
       let unbound_params1 =
         List.map
           (fun param -> unbind_sexpr param replace_self new_var_name)
           udt_func_params
       in
       let unbound_params2 = unbound_params1 @ [ unbound_udt_se ] in
       t', SFunctionCall (udt_func_name, unbound_params2))

  | SUDTStaticAccess (udt_name, udt_static_func) ->
    t', SUDTStaticAccess (udt_name, udt_static_func)
  | SIndex (indexed_se, index_val) ->
    let unbound_indexed_se = unbind_sexpr indexed_se replace_self new_var_name in
    let unbound_index_val = unbind_sexpr index_val replace_self new_var_name in
    t', SIndex (unbound_indexed_se, unbound_index_val)
  | SList se_list ->
    let unbound_se_list =
      List.map (fun elem -> unbind_sexpr elem replace_self new_var_name) se_list
    in
    t', SList unbound_se_list
  | STypeCast (new_rt, target_se) ->
    let unbound_target_se = unbind_sexpr target_se replace_self new_var_name in
    t', STypeCast (new_rt, unbound_target_se)

and unbind_block sblk variables replace_self new_var_name =
  match sblk with
  | SMutDeclTyped _ | SAssign _ -> failwith "Dropping"
  | SDeclTyped (var_name, rt, se) ->
    let updated_variables = StringSet.add var_name variables in
    updated_variables, SDeclTyped (var_name, rt, unbind_sexpr se replace_self new_var_name)
  | SFunctionDefinition (rt, func_name, func_args, body) ->
    let updated_variables1 = add_func_args func_args variables in
    let updated_variables2, unbound_body = unbind_block_list body updated_variables1 replace_self new_var_name in
    updated_variables2, SFunctionDefinition (rt, func_name, func_args, unbound_body)
  | SBoundFunctionDefinition (rt, func_name, func_args, body, _) ->
    let updated_variables1 = add_func_args func_args variables in
    let updated_variables2, _ = unbind_block_list body updated_variables1 replace_self new_var_name in
    let var_name = fresh_var_name "tmp" updated_variables2 0 in
    let updated_func_args =
      List.map
        (fun (arg_name, arg_type) ->
           if arg_name = "self" then 
            (var_name, arg_type)
          else arg_name, arg_type)
        func_args
    in
    let updated_variables3 = add_func_args updated_func_args variables in
    let _, unbound_body = unbind_block_list body updated_variables3 true var_name in
    variables, SFunctionDefinition (rt, func_name, updated_func_args, unbound_body)
  | SEnumDeclaration (enum_name, enum_variants) ->
    variables, SEnumDeclaration (enum_name, enum_variants)
  | SUDTDef (udt_name, udt_members) -> variables, SUDTDef (udt_name, udt_members)
  | SIfEnd (cond, if_body) ->
    let unbound_cond = unbind_sexpr cond replace_self new_var_name in
    variables, SIfEnd (unbound_cond, if_body)
  | SIfNonEnd (cond, if_body, other) ->
    let unbound_cond = unbind_sexpr cond replace_self new_var_name in
    let updated_variables1, unbound_if_body = unbind_block_list if_body variables replace_self new_var_name in
    let updated_variables2, unbound_other =
      unbind_block other updated_variables1 replace_self new_var_name
    in
    updated_variables2, SIfNonEnd (unbound_cond, unbound_if_body, unbound_other)
  | SElifNonEnd (cond, elif_body, other) ->
    let unbound_cond = unbind_sexpr cond replace_self new_var_name in
    let updated_variables1, unbound_elif_body = unbind_block_list elif_body variables replace_self new_var_name in
    let updated_variables2, unbound_other =
      unbind_block other updated_variables1 replace_self new_var_name
    in
    updated_variables2, SElifNonEnd (unbound_cond, unbound_elif_body, unbound_other)
  | SElifEnd (cond, elif_body) ->
    let unbound_cond = unbind_sexpr cond replace_self new_var_name in
    let updated_variables, unbound_elif_body = unbind_block_list elif_body variables replace_self new_var_name in
    updated_variables, SElifEnd (unbound_cond, unbound_elif_body)
  | SElseEnd else_body ->
    let updated_variables, unbound_else_body = unbind_block_list else_body variables replace_self new_var_name in
    updated_variables, SElseEnd unbound_else_body
  | SWhile (se, while_body) ->
    let unbound_se = unbind_sexpr se replace_self new_var_name in
    variables, SWhile (unbound_se, while_body)
  | SFor (iterator, iterable_se, for_body) ->
    let unbound_iterable_se = unbind_sexpr iterable_se replace_self new_var_name in
    let updated_variables, unbound_for_body = unbind_block_list for_body variables replace_self new_var_name in
    updated_variables, SFor (iterator, unbound_iterable_se, unbound_for_body)
  | SBreak -> variables, SBreak
  | SContinue -> variables, SContinue
  | SReturnUnit -> variables, SReturnUnit
  | SReturnVal se -> variables, SReturnVal (unbind_sexpr se replace_self new_var_name)
  | SExpr se -> variables, SExpr (unbind_sexpr se replace_self new_var_name)

and unbind_block_list sblk_list variables replace_self var_name =
  match sblk_list with
  | [] -> variables, []
  | sblk :: rest ->
    let updated_variables, unbound_sblk = unbind_block sblk variables replace_self var_name in
    updated_variables, unbound_sblk :: snd (unbind_block_list rest updated_variables replace_self var_name)
;;

let unbind sblk_list =
  let variables = StringSet.empty in
  let _, unbound_sblk_list = unbind_block_list sblk_list variables false "" in
  unbound_sblk_list
;;