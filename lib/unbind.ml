open Sast
module StringSet = Set.Make (String)

let rec unbind_sexpr se =
  let t', se' = se in
  match se' with
  | SLiteral | SBoolLit | SFloatLit | SCharLit | SStringLit | SUnit | SEnumAccess -> t', se'
  | SBinop (se1, binop, se2) -> failwith("Unimplemented")
  | SUnop (se, unop) -> failwith("Unimplemented")
  | SUnopSideEffect | SMatch | SWildcard -> failwith("Dropping")
  | SFunctionCall (func_name, func_args) -> failwith("Unimplemented")
  | SId id -> failwith("Unimplemented")
  | STuple -> failwith("Unimplemented")
  | SFunctionCall -> failwith("Unimplemented")
  | SUDTInstance -> failwith("Unimplemented")
  | SUDTAccess -> failwith("Unimplemented")
  | SUDTStaticAccess -> failwith("Unimplemented")
  | SIndex -> failwith("Unimplemented")
  | SList -> failwith("Unimplemented")
  | STypeCast -> failwith("Unimplemented")

and unbind_block sblk =
  match sblk with
  | SMutDeclTyped _ | SAssign _ -> failwith("Dropping")
  | SDeclTyped (var_name, rt, se) -> SDeclTyped(var_name, rt, unbind_sexpr se)
  | SFunctionDefinition sfunc_def -> SFunctionDefinition sfunc_def
  | SBoundFunctionDefinition -> failwith("Unimplemented")
  | SEnumDeclaration -> failwith("Unimplemented")
  | SUDTDef -> failwith("Unimplemented")
  | SIfEnd -> failwith("Unimplemented")
  | SIfNonEnd -> failwith("Unimplemented")
  | SElifNonEnd -> failwith("Unimplemented")
  | SElifEnd -> failwith("Unimplemented")
  | SElseEnd -> failwith("Unimplemented")
  | SWhile -> failwith("Unimplemented")
  | SFor -> failwith("Unimplemented")
  | SBreak -> failwith("Unimplemented")
  | SContinue -> failwith("Unimplemented")
  | SReturnUnit -> failwith("Unimplemented")
  | SReturnVal -> failwith("Unimplemented")
  | SExpr se -> unbind_sexpr se

and unbind_block_list sblk_list variables =
  match sblk_list with
  | [] -> []
  | sblk :: rest -> 
    let unbound_sblk, updated_variables = unbind_block sblk in
    unbound_sblk :: unbind_block_list rest updated_variables
;;
