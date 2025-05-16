open Sast
module StringSet = Set.Make (String)

let rec unbind_sexpr se =
  let t', se' = se in
  match se' with
  | SLiteral _ | SBoolLit _ | SFloatLit _ | SCharLit _ | SStringLit _ | SUnit | SEnumAccess _ -> t', se'
  | SUnopSideEffect _ | SMatch _ | SWildcard -> failwith("Dropping")
  | SBinop (se1, binop, se2) -> failwith("Unimplemented")
  | SUnop (se, unop) -> failwith("Unimplemented")
  | SFunctionCall (func_name, func_args) -> failwith("Unimplemented")
  | SId id -> failwith("Unimplemented")
  | STuple (se_list) -> failwith("Unimplemented")
  | SUDTInstance (udt_name, udt_members) -> failwith("Unimplemented")
  | SUDTAccess (udt_se, udt_member) -> failwith("Unimplemented")
  | SUDTStaticAccess (udt_name, udt_static_func) -> failwith("Unimplemented")
  | SIndex (indexed_se, index_val) -> failwith("Unimplemented")
  | SList (se_list) -> failwith("Unimplemented")
  | STypeCast (new_rt, target_se) -> failwith("Unimplemented")

and unbind_block sblk variables =
  match sblk with
  | SMutDeclTyped _ | SAssign _ -> 
    failwith("Dropping")
  | SDeclTyped (var_name, rt, se) -> 
    variables, SDeclTyped(var_name, rt, unbind_sexpr se)
  | SFunctionDefinition (rt, func_name, func_args, body) -> 
    variables, SFunctionDefinition (rt, func_name, func_args, body)
  | SBoundFunctionDefinition (rt, func_name, func_args, body, bound_type) -> 
    variables, failwith("Unimplemented")
  | SEnumDeclaration (enum_name, enum_variants) -> 
    variables, failwith("Unimplemented")
  | SUDTDef (udt_name, udt_members) -> 
    variables, failwith("Unimplemented")
  | SIfEnd (cond, if_body) -> 
    variables, failwith("Unimplemented")
  | SIfNonEnd (cond, if_body, other) -> 
    variables, failwith("Unimplemented")
  | SElifNonEnd (cond, elif_body, other) -> 
    variables, failwith("Unimplemented")
  | SElifEnd (cond, elif_body) -> 
    variables, failwith("Unimplemented")
  | SElseEnd (else_body) -> 
    variables, failwith("Unimplemented")
  | SWhile (se, while_body) -> 
    variables, failwith("Unimplemented")
  | SFor (iterator, iterable_se, for_body) -> 
    variables, failwith("Unimplemented")
  | SBreak -> 
    variables, SBreak
  | SContinue -> 
    variables, SContinue
  | SReturnUnit -> 
    variables, SReturnUnit
  | SReturnVal se -> 
    variables, SReturnVal (unbind_sexpr se)
  | SExpr se -> 
    variables, SExpr (unbind_sexpr se)

and unbind_block_list sblk_list variables =
  match sblk_list with
  | [] -> []
  | sblk :: rest -> 
    let updated_variables, unbound_sblk = unbind_block sblk variables in
    unbound_sblk :: unbind_block_list rest updated_variables
;;
