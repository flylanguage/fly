module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make (String)

let context = L.global_context ()

type scope =
  | Local
  | Global

type variable =
  { v_value : L.llvalue
  ; v_scope : scope
  ; v_type : A.typ
  }

let l_int = L.i32_type context
and l_bool = L.i1_type context
and l_char = L.i8_type context
and l_unit = L.void_type context
and l_float = L.float_type context

let l_str = L.pointer_type l_char

let ltype_of_typ = function
  | A.Int -> l_int
  | A.Bool -> l_bool
  | A.Float -> l_float
  (* | A.Char -> l_char *)
  | A.Unit -> l_unit
  | A.String -> l_str
  | t ->
    raise (Failure (Printf.sprintf "type not implemented: %s" (Utils.string_of_type t)))
;;

let l_printf : L.lltype = L.var_arg_function_type l_int [| L.pointer_type l_char |]
let print_func the_module : L.llvalue = L.declare_function "printf" l_printf the_module
let int_format_str builder = L.build_global_stringptr "%d\n" "int_fmt" builder
let str_format_str builder = L.build_global_stringptr "%s\n" "str_fmt" builder
let float_format_str builder = L.build_global_stringptr "%f\n" "float_fmt" builder

let get_lformals_arr (formals : A.formal list) =
  let lformal_list = List.map ltype_of_typ (List.map snd formals) in
  Array.of_list lformal_list
;;

let lookup (vars : variable StringMap.t) var =
  try StringMap.find var vars with
  | Not_found ->
    raise (Failure (Printf.sprintf "var lookup error: failed to find variable %s\n" var))
;;

let lookup_value (vars : variable StringMap.t) var =
  try
    let vbl = StringMap.find var vars in
    vbl.v_value
  with
  | Not_found ->
    raise (Failure (Printf.sprintf "var lookup error: failed to find variable %s\n" var))
;;

let rec build_expr expr (vars : variable StringMap.t) the_module builder func_blocks =
  let sx = snd expr in
  match sx with
  | SLiteral l -> L.const_int l_int l
  | SBoolLit b -> L.const_int l_bool (if b then 1 else 0)
  | SFloatLit f -> L.const_float l_float f
  | SId var ->
    let vbl = lookup vars var in
    (* strings are pointers, they should not be load-ed like other variables 
       Now, local strings already exist in a variable in the function scope, 
       and build_load is okay here as we're loading from the variable, not 
       the raw pointer.
       Therefore, we have this special case for Global strings
    *)
    if vbl.v_scope == Global && vbl.v_type == A.String
    then vbl.v_value
    else L.build_load vbl.v_value var builder
  | SUnop (e, op) ->
    let llval = build_expr e vars the_module builder func_blocks in
    let typ = fst e in
    (match op with
     | A.Not ->
       (match typ with
        | A.Bool -> L.build_not llval "tmp_not" builder
        | _ -> failwith ("Unary Not not supported for type " ^ Utils.string_of_type typ))
     | _ -> failwith ("Unary operator not supported for type " ^ Utils.string_of_type typ))
  | SUnopSideEffect (var, op) ->
    let typ = fst expr in
    let ll_ptr = lookup_value vars var in
    let ll_original_val = L.build_load ll_ptr var builder in
    let ll_one =
      match typ with
      | A.Int -> L.const_int l_int 1
      | A.Float -> L.const_float l_float 1.0
      | _ -> failwith (Utils.string_of_type typ ^ " does not support unary incr or decr")
    in
    let ll_new_val =
      match op with
      | A.Postincr | A.Preincr ->
        (match typ with
         | A.Int -> L.build_add ll_original_val ll_one "incr_val" builder
         | A.Float -> L.build_fadd ll_original_val ll_one "incr_val" builder
         | _ -> failwith "Post/Preincr failed")
      | A.Postdecr | A.Predecr ->
        (match typ with
         | A.Int -> L.build_sub ll_original_val ll_one "incr_val" builder
         | A.Float -> L.build_fsub ll_original_val ll_one "incr_val" builder
         | _ -> failwith "Post/Predecr failed")
      | _ -> failwith ("Operand " ^ Utils.string_of_op op ^ " Not found")
    in

    ignore (L.build_store ll_new_val ll_ptr builder);

    (match op with
     | A.Postincr | A.Postdecr -> ll_original_val
     | A.Preincr | A.Predecr -> ll_new_val
     | _ -> failwith "Could apply incr/decr to variable")
  | SFunctionCall (func_name, params) -> 
    if func_name = print_func_name then
      print (func_name, params) vars the_module builder
    else
      (* print_endline "HELLO"; *)
      (*List.iter (fun (f, _, _) -> print_endline (L.value_name f)) func_blocks;*)
      let (fdef, _, _) = List.find (fun (f, _, _) -> L.value_name f = func_name) func_blocks in
      let llargs = List.map (fun param -> build_expr param vars the_module builder func_blocks) params in
      let result = if L.type_of fdef = l_unit then "" else func_name ^ "_result" in
      L.build_call fdef (Array.of_list llargs) result builder
  (*| SFunctionCall (func_name, args) ->
    (* let (fdef, fast) = StringMap.find *)
    let func_ll = StringMap.find (string_of_svar func_name) func_map in
    let llargs = List.rev (List.map (build_expr var_map func_map builder) (List.rev args))
    in
    let result = string_of_svar func_name ^ "_result" in
    L.build_call func_ll (Array.of_list llargs) result builder*)
  | SEnumAccess (enum_name, variant_name) ->
    let key = enum_name ^ "::" ^ variant_name in
    if not (StringMap.mem key vars)
    then
      raise
        (Failure
           (Printf.sprintf "Enum variant %s not found in enum %s" variant_name enum_name))
    else lookup_value vars key
  | SBinop (e1, op, e2) ->
    let typ = fst e1 in
    let se1 = build_expr e1 vars the_module builder func_blocks in
    let se2 = build_expr e2 vars the_module builder func_blocks in
    let lval =
      match typ with
      | A.Int ->
        (match op with
         | A.Add -> L.build_add
         | A.Sub -> L.build_sub
         | A.Mult -> L.build_mul
         | A.Div -> L.build_sdiv
         | A.Mod -> L.build_srem
         | A.Equal -> L.build_icmp L.Icmp.Eq
         | A.Neq -> L.build_icmp L.Icmp.Ne
         | A.Less -> L.build_icmp L.Icmp.Slt
         | A.Leq -> L.build_icmp L.Icmp.Sle
         | A.Greater -> L.build_icmp L.Icmp.Sgt
         | A.Geq -> L.build_icmp L.Icmp.Sge
         | _ ->
           failwith
             (Printf.sprintf
                "Integer binary operator %s not yet implemented"
                (Utils.string_of_op op)))
      | A.Float ->
        (match op with
         | A.Add -> L.build_fadd
         | A.Sub -> L.build_fsub
         | A.Mult -> L.build_fmul
         | A.Div -> L.build_fdiv
         | A.Mod -> L.build_frem
         | A.Equal -> L.build_fcmp L.Fcmp.Oeq
         | A.Neq -> L.build_fcmp L.Fcmp.One
         | A.Less -> L.build_fcmp L.Fcmp.Olt
         | A.Leq -> L.build_fcmp L.Fcmp.Ole
         | A.Greater -> L.build_fcmp L.Fcmp.Ogt
         | A.Geq -> L.build_fcmp L.Fcmp.Oge
         | _ ->
           failwith
             (Printf.sprintf
                "Float binary operator %s not yet implemented"
                (Utils.string_of_op op)))
      | A.Bool ->
        (match op with
         | A.And -> L.build_and
         | A.Or -> L.build_or
         | A.Equal -> L.build_icmp L.Icmp.Eq
         | A.Neq -> L.build_icmp L.Icmp.Ne
         | _ ->
           failwith
             (Printf.sprintf
                "Boolean binary operator %s not yet implemented"
                (Utils.string_of_op op)))
      | _ ->
        failwith
          (Printf.sprintf
             "Binary operator %s not yet implemented for type %s"
             (Utils.string_of_op op)
             (Utils.string_of_type typ))
    in
    lval se1 se2 ("tmp_" ^ Utils.string_of_type typ) builder
  | SStringLit s -> L.build_global_stringptr s "str" builder
  | e ->
    raise (Failure (Printf.sprintf "expr not implemented: %s" (Utils.string_of_sexpr e)))

(* This is the "print" prelude function, it is a special function that exists within fly

   Preferrably this function should exist somewhere else, but it needs to be defined with build_expr
*)
and print (func : sfunc) vars the_module builder =
  if List.length (snd func) != 1
  then failwith "Incorrect number of args to print: expected 1"
  else (
    let func_arg = List.hd (snd func) in
    let lexpr = build_expr func_arg vars the_module builder [] in
    let arr =
      match fst func_arg with
      | A.Int -> [| int_format_str builder; lexpr |]
      | A.Bool ->
        (* For bool prints, we actually print a string: "true" for true, and "false" for false 
           This is pretty tricky, requiring us to create branches and use a phi conditional (some IR stuff) 
           to determine which one to print
        *)
        let parent_block = L.insertion_block builder in
        let the_function = L.block_parent parent_block in
        let true_block = L.append_block context "true_case" the_function in
        let false_block = L.append_block context "false_case" the_function in
        let merge_block = L.append_block context "merge" the_function in
        ignore (L.build_cond_br lexpr true_block false_block builder);

        (* Build true block *)
        L.position_at_end true_block builder;
        let true_str = L.build_global_stringptr "true" "true_str" builder in
        ignore (L.build_br merge_block builder);

        L.position_at_end false_block builder;
        let false_str = L.build_global_stringptr "false" "false_str" builder in
        ignore (L.build_br merge_block builder);

        L.position_at_end merge_block builder;

        (* phi changes behavior dependent on which branch we arrived from
             if we arrived from true_block, use true_str
             if we arrived from false_block, use false_str
        *)
        let bool_str =
          L.build_phi [ true_str, true_block; false_str, false_block ] "bool_str" builder
        in
        [| str_format_str builder; bool_str |]
      | A.Float -> [| float_format_str builder; lexpr |]
      | A.String -> [| str_format_str builder; lexpr |]
      | _ -> failwith "print not implemented for type"
    in
    L.build_call
      (print_func the_module)
      arr
      "printf" (* call the LLVM IR "printf" function *)
      builder)
;;

let assert_types typ1 typ2 =
  if typ1 != typ2
  then
    failwith
      (Printf.sprintf
         "Type mismatch. SAST should catch this! (%s vs %s)"
         (Utils.string_of_type typ1)
         (Utils.string_of_type typ2))
;;

let add_local_val typ var vars (expr : A.typ * Sast.sx) the_module builder =
  let expr_type = fst expr in
  assert_types expr_type typ;

  let local_var_allocation : L.llvalue = L.build_alloca (ltype_of_typ typ) var builder in
  let ll_initializer_value : L.llvalue = build_expr expr vars the_module builder [] in

  ignore (L.build_store ll_initializer_value local_var_allocation builder);

  let vbl = { v_value = local_var_allocation; v_type = typ; v_scope = Local } in
  StringMap.add var vbl vars
;;

let add_global_val typ var (vars : variable StringMap.t) expr the_module =
  let etyp = fst expr in
  assert_types etyp typ;

  let global =
    match typ, snd expr with
    | A.Int, SLiteral i ->
      let init = L.const_int l_int i in
      L.define_global var init the_module
    | A.String, SStringLit s ->
      (* Create fake temporary function to create a builder *)
      let temp_fn_type = L.function_type (L.void_type context) [||] in
      let temp_fn = L.define_function "temp_fn" temp_fn_type the_module in
      let builder = L.builder context in
      L.position_at_end (L.entry_block temp_fn) builder;

      let init = L.build_global_stringptr s "str" builder in

      L.delete_function temp_fn;

      init
    | t, e ->
      raise
        (Failure
           (Printf.sprintf
              "global assignment not completed: typ: %s, expr: %s"
              (Utils.string_of_type t)
              (Utils.string_of_sexpr e)))
  in
  let vbl = { v_value = global; v_type = typ; v_scope = Global } in
  StringMap.add var vbl vars
;;

let add_terminal builder instr =
  match L.block_terminator (L.insertion_block builder) with
  | Some _ -> ()
  | None -> ignore (instr builder)
;;

let translate blocks =
  let the_module = L.create_module context "Fly" in
  let local_vars : variable StringMap.t = StringMap.empty in

  let declare_function typ id (formals : A.formal list) body func_blocks =
    let lfunc =
      L.define_function
        id
        (L.function_type (ltype_of_typ typ) (get_lformals_arr formals))
        the_module
    in
    ignore (L.builder_at_end context (L.entry_block lfunc));
    (* Add function block in blocks-to-declare list *)
    (lfunc, formals, body) :: func_blocks
  in

  (* before build_call, collect all function declarations *)
  let rec collect_func_decls blocks func_blocks =
    match blocks with
    | [] -> func_blocks
    | SFunctionDefinition (typ, id, formals, body) :: rest ->
        let func_blocks' = declare_function typ id formals body func_blocks in
        (* nested case *)
        let func_blocks'' = collect_func_decls body func_blocks' in
        collect_func_decls rest func_blocks''
    | SIfEnd (_, blks) :: rest ->
        let func_blocks' = collect_func_decls blks func_blocks in
        collect_func_decls rest func_blocks'
    | SIfNonEnd (_, blks, else_blk) :: rest ->
        let func_blocks' = collect_func_decls blks func_blocks in
        let func_blocks'' = collect_func_decls [else_blk] func_blocks' in
        collect_func_decls rest func_blocks''
    | _ :: rest -> collect_func_decls rest func_blocks
  in

  let rec process_func_blocks func_blocks vars =
    match func_blocks with
    | [] -> ()
    | (lfunc, formals, blocks) :: rest ->
        let builder = L.builder_at_end context (L.entry_block lfunc) in
        let vars_with_formals = 
          List.fold_left2
            (fun acc (name, typ) param ->
              let alloca = L.build_alloca (ltype_of_typ typ) name builder in
              ignore (L.build_store param alloca builder);
              StringMap.add name 
                { v_value = alloca; v_type = typ; v_scope = Local }
                acc)
            vars
            formals
            (Array.to_list (L.params lfunc))
        in
        
        (* function body *)
        ignore (process_blocks blocks vars_with_formals (Some lfunc) func_blocks (Some builder));
        
        (* add return if it isn't there *)
        (match L.block_terminator (L.insertion_block builder) with
         | Some _ -> ()
         | None ->
             if L.return_type (L.type_of lfunc) = l_unit
             then ignore (L.build_ret_void builder));
        
        (* Process remaining functions *)
        process_func_blocks rest vars

(*  and process_func_block (func_block : L.llvalue * A.formal list * sblock list) vars func_blocks =
    let (lfunc, formals, blocks) = func_block in
    let builder = L.builder_at_end context (L.entry_block lfunc) in
    let vars_with_formals = 
      List.fold_left2
        (fun acc (name, typ) param ->
          let alloca = L.build_alloca (ltype_of_typ typ) name builder in
          ignore (L.build_store param alloca builder);
          StringMap.add name 
            { v_value = alloca; v_type = typ; v_scope = Local }
            acc)
        vars
        formals
        (Array.to_list (L.params lfunc))
    in
    process_blocks blocks vars_with_formals (Some lfunc) func_blocks (Some builder)
*)
  and process_blocks blocks vars (curr_func : L.llvalue option) func_blocks builder =
    match blocks with
    | [] -> ()
    | block :: rest ->
      let updated_vars, updated_curr_func, u_func_blocks, u_builder =
        process_block block vars curr_func func_blocks builder
      in
      process_blocks rest updated_vars updated_curr_func u_func_blocks u_builder

  and process_block block vars (curr_func : L.llvalue option) func_blocks builder =
    match block with
    | SDeclTyped (id, typ, expr) ->
      if Option.is_some curr_func
      then
        ( add_local_val typ id vars expr the_module (Option.get builder)
        , curr_func
        , func_blocks
        , builder )
      else add_global_val typ id vars expr the_module, curr_func, func_blocks, builder
    | SFunctionDefinition (_, _, _, _) ->
      (* handled by collect_func_decls already *)
      vars, curr_func, func_blocks, builder
    | SReturnUnit ->
      ignore (L.build_ret_void (Option.get builder));
      vars, curr_func, func_blocks, builder
    | SReturnVal expr ->
      let ret = build_expr expr vars the_module (Option.get builder) func_blocks in
      ignore (L.build_ret ret (Option.get builder));
      vars, curr_func, func_blocks, builder
    | SExpr expr ->
      ignore (build_expr expr vars the_module (Option.get builder) func_blocks);
      vars, curr_func, func_blocks, builder
    | SIfEnd (expr, blks) ->
      let bool_val = build_expr expr vars the_module (Option.get builder) func_blocks in

      (* We require curr_func to be Some - no if-else in global scope *)
      let then_bb = L.append_block context "then" (Option.get curr_func) in
      let then_builder = Some (L.builder_at_end context then_bb) in
      ignore (process_blocks blks vars curr_func func_blocks then_builder);

      let end_bb = L.append_block context "if_end" (Option.get curr_func) in
      let build_br_end = L.build_br end_bb in
      add_terminal (L.builder_at_end context then_bb) build_br_end;

      ignore (L.build_cond_br bool_val then_bb end_bb (Option.get builder));
      let u_builder = Some (L.builder_at_end context end_bb) in
      vars, curr_func, func_blocks, u_builder
    | SIfNonEnd (expr, blks, else_blk) ->
      assert_types (fst expr) A.Bool;

      let bool_val = build_expr expr vars the_module (Option.get builder) func_blocks in

      let then_bb = L.append_block context "then" (Option.get curr_func) in
      let then_builder = Some (L.builder_at_end context then_bb) in
      ignore (process_blocks blks vars curr_func func_blocks then_builder);

      let end_bb = L.append_block context "if_end" (Option.get curr_func) in

      (* skip this "if_end", ElseEnd or ElifEnd will process it *)
      (* We require curr_func to be Some - no if-else in global scope *)
      let else_bb = L.append_block context "else" (Option.get curr_func) in
      let else_builder = Some (L.builder_at_end context else_bb) in
      ignore (L.build_cond_br bool_val then_bb else_bb (Option.get builder));

      let u_builder =
        process_elseifs vars else_blk end_bb curr_func func_blocks else_builder
      in

      let build_br_end = L.build_br end_bb in
      add_terminal (L.builder_at_end context then_bb) build_br_end;
      add_terminal (L.builder_at_end context else_bb) build_br_end;

      vars, curr_func, func_blocks, u_builder
    | SEnumDeclaration (id, variants) ->
      let enum_type = L.named_struct_type context id in
      let fields = Array.of_list (List.map (fun _ -> L.i32_type context) variants) in
      ignore (L.struct_set_body enum_type fields true);
      let rec assign_enum_values variants last_value =
        match variants with
        | [] -> []
        | SEnumVariantDefault n :: rest ->
          let lval = L.const_int l_int last_value in
          let vbl = { v_value = lval; v_type = A.Int; v_scope = Global } in
          let curr = id ^ "::" ^ n, vbl in

          curr :: assign_enum_values rest (last_value + 1)
        | SEnumVariantExplicit (n, v) :: rest ->
          let lval = L.const_int l_int v in
          let vbl = { v_value = lval; v_type = A.Int; v_scope = Global } in
          let curr = id ^ "::" ^ n, vbl in

          curr :: assign_enum_values rest (v + 1)
      in
      let vars =
        assign_enum_values variants 0
        |> List.fold_left (fun acc (name, value) -> StringMap.add name value acc) vars
      in
      vars, curr_func, func_blocks, builder
    | b ->
      raise
        (Failure
           (Printf.sprintf "expression not implemented: %s" (Utils.string_of_sblock b)))
  (*  and process_blocks blocks vars (curr_func : L.llvalue option) func_blocks builder =
    match blocks with
    (* We've declared all objects, lets fill in all function bodies *)
    | [] -> ()
    | block :: rest ->
      let updated_vars, updated_curr_func, u_func_blocks, u_builder =
        process_block block vars curr_func func_blocks builder
      in
      process_blocks rest updated_vars updated_curr_func u_func_blocks u_builder*)
  and process_elseifs vars block end_bb curr_func func_blocks builder =
    match block with
    | SElseEnd blks ->
      ignore (process_blocks blks vars curr_func func_blocks builder);

      (* TODO: Throw an error or warning if the code is unreachable? *)
      let u_builder = Some (L.builder_at_end context end_bb) in
      u_builder
    | SElifEnd (expr, blks) ->
      assert_types (fst expr) A.Bool;

      let bool_val = build_expr expr vars the_module (Option.get builder) func_blocks in

      let then_bb = L.append_block context "then" (Option.get curr_func) in
      let then_builder = Some (L.builder_at_end context then_bb) in
      ignore (process_blocks blks vars curr_func func_blocks then_builder);

      let build_br_end = L.build_br end_bb in
      add_terminal (L.builder_at_end context then_bb) build_br_end;

      ignore (L.build_cond_br bool_val then_bb end_bb (Option.get builder));

      let u_builder = Some (L.builder_at_end context end_bb) in
      u_builder
    | SElifNonEnd (expr, blks, else_blk) ->
      assert_types (fst expr) A.Bool;

      let bool_val = build_expr expr vars the_module (Option.get builder) func_blocks in

      let then_bb = L.append_block context "then" (Option.get curr_func) in
      let then_builder = Some (L.builder_at_end context then_bb) in
      ignore (process_blocks blks vars curr_func func_blocks then_builder);

      let build_br_end = L.build_br end_bb in
      add_terminal (L.builder_at_end context then_bb) build_br_end;

      let else_bb = L.append_block context "else" (Option.get curr_func) in
      let else_builder = Some (L.builder_at_end context else_bb) in
      ignore (L.build_cond_br bool_val then_bb else_bb (Option.get builder));

      let u_builder =
        process_elseifs vars else_blk end_bb curr_func func_blocks else_builder
      in

      u_builder
    | _ -> raise (Failure "Only SElseEnd, SElifEnd and SElifNonEnd can follow SIfNonEnd")
  in

  (* process global variables first *)
  let process_globals blocks vars =
    let rec aux blocks vars =
      match blocks with
      | [] -> vars
      | SDeclTyped (id, typ, expr) :: rest ->
          let updated_vars = add_global_val typ id vars expr the_module in
          aux rest updated_vars
      | _ :: rest -> aux rest vars
    in
    aux blocks vars
  in

  let func_blocks = collect_func_decls blocks [] in
  
  (*(* Debug print: print all function names in func_blocks *)
  List.iter (fun (f, _, _) -> print_endline (L.value_name f)) func_blocks;
  (* ..and start off with no builder.. *)
  let builder = None in
  process_func_blocks func_blocks local_vars;*)
  let vars_with_globals = process_globals blocks local_vars in
  
  process_func_blocks func_blocks vars_with_globals;
  the_module
;;