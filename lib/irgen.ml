module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make (String)

let context = L.global_context ()

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
  | A.Char -> l_char
  | A.Unit -> l_unit
  | A.String -> l_str
  | t ->
    raise (Failure (Printf.sprintf "type not implemented: %s" (Utils.string_of_type t)))
;;

let l_printf : L.lltype = L.var_arg_function_type l_int [| L.pointer_type l_char |]
let print_func the_module : L.llvalue = L.declare_function "printf" l_printf the_module
let int_format_str builder = L.build_global_stringptr "%d\n" "fmt" builder
let str_format_str builder = L.build_global_stringptr "%s\n" "fmt" builder
let float_format_str builder = L.build_global_stringptr "%f\n" "fmt" builder

let get_lformals_arr (formals : A.formal list) =
  let lformal_list = List.map ltype_of_typ (List.map snd formals) in
  Array.of_list lformal_list
;;

let lookup (vars : L.llvalue StringMap.t) var =
  try StringMap.find var vars with
  | Not_found ->
    raise (Failure (Printf.sprintf "var lookup error: failed to find variable %s\n" var))
;;

let rec build_expr expr vars the_module builder =
  match expr with
  | SLiteral l -> L.const_int l_int l
  | SBoolLit b -> L.const_int l_bool (if b then 1 else 0)
  | SFloatLit f -> L.const_float l_float f
  | SId var -> L.build_load (lookup vars var) var builder
  | SFunctionCall func ->
    let func_name = fst func in

    if func_name <> print_func_name
    then
      raise (Failure (Printf.sprintf "funccall: %s" func_name))
      (* TODO: print with no args should print new line *)
    else if List.length (snd func) != 1
    then failwith "Incorrect number of args to print: expected 1"
    else (
      let func_arg = List.hd (snd func) in
      let arr =
        match func_arg with
        | A.Int, e ->
          let lexpr = build_expr e vars the_module builder in
          [| int_format_str builder; lexpr |]
        | A.Bool, e ->
          let lexpr = build_expr e vars the_module builder in

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
            L.build_phi
              [ true_str, true_block; false_str, false_block ]
              "bool_str"
              builder
          in
          [| str_format_str builder; bool_str |]
        | A.Float, e ->
          let lexpr = build_expr e vars the_module builder in
          [| float_format_str builder; lexpr |]
        | _, _ -> failwith "Incorrect call to print"
      in
      let _res =
        L.build_call
          (print_func the_module)
          arr
          "printf" (* call the LLVM IR "printf" function *)
          builder
      in
      _res)
  | e ->
    raise (Failure (Printf.sprintf "expr not implemented: %s" (Utils.string_of_sexpr e)))
;;

let sanitize_types typ1 typ2 =
  if typ1 != typ2
  then
    raise
      (Failure
         (Printf.sprintf
            "Type mismatch. SAST should catch this! (%s vs %s)"
            (Utils.string_of_type typ1)
            (Utils.string_of_type typ2)))
;;

let add_local_val typ var vars expr builder =
  let etyp = fst expr in
  sanitize_types etyp typ;

  let local =
    match typ, snd expr with
    | A.Int, SLiteral i ->
      let local = L.build_alloca (ltype_of_typ typ) var builder in
      let v = L.const_int (ltype_of_typ typ) i in
      ignore (L.build_store v local builder);
      local
    | A.Bool, SBoolLit b ->
      let local = L.build_alloca (ltype_of_typ typ) var builder in
      let v = L.const_int (ltype_of_typ typ) (if b then 1 else 0) in
      ignore (L.build_store v local builder);
      local
    | A.Float, SFloatLit f ->
      let local = L.build_alloca (ltype_of_typ typ) var builder in
      let v = L.const_float (ltype_of_typ typ) f in
      ignore (L.build_store v local builder);
      local
    | t, e ->
      raise
        (Failure
           (Printf.sprintf
              "local assignment not completed: typ: %s, expr: %s"
              (Utils.string_of_type t)
              (Utils.string_of_sexpr e)))
  in
  StringMap.add var local vars
;;

let add_global_val typ var vars expr the_module =
  let etyp = fst expr in
  sanitize_types etyp typ;

  let global =
    match typ, snd expr with
    | A.Int, SLiteral i ->
      let init = L.const_int l_int i in
      L.define_global var init the_module
    | A.Bool, SBoolLit b ->
      let init = L.const_int l_bool (if b then 1 else 0) in
      L.define_global var init the_module
    | A.Float, SFloatLit f ->
      let init = L.const_float l_float f in
      L.define_global var init the_module
    | A.Char, SCharLit c ->
      let init = L.const_stringz context (String.make 1 c) in
      L.define_global var init the_module
    | t, e ->
      raise
        (Failure
           (Printf.sprintf
              "global assignment not completed: typ: %s, expr: %s"
              (Utils.string_of_type t)
              (Utils.string_of_sexpr e)))
  in
  StringMap.add var global vars
;;

let add_terminal builder instr =
  match L.block_terminator (L.insertion_block builder) with
  | Some _ -> ()
  | None -> ignore (instr builder)
;;

let translate blocks =
  let the_module = L.create_module context "Fly" in
  let local_vars = StringMap.empty in

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

  (* Receives all func blocks after all functions have been declared and fills each func blocks' body *)
  let rec process_func_blocks func_blocks vars =
    match func_blocks with
    | [] -> ()
    | _blk :: rst ->
      process_func_block _blk vars;
      process_func_blocks rst vars
  and process_func_block (func_block : L.llvalue * A.formal list * sblock list) vars =
    let lfunc, _, blocks = func_block in
    let curr_func = Some lfunc in
    let builder = L.builder_at_end context (L.entry_block lfunc) in
    process_blocks blocks vars curr_func [] (Some builder)
  and process_block block vars (curr_func : L.llvalue option) func_blocks builder =
    match block with
    | SDeclTyped (id, typ, expr) ->
      if Option.is_some curr_func
      then
        ( add_local_val typ id vars expr (Option.get builder)
        , curr_func
        , func_blocks
        , builder )
      else add_global_val typ id vars expr the_module, curr_func, func_blocks, builder
    | SFunctionDefinition (typ, id, formals, body) ->
      let u_func_blocks = declare_function typ id formals body func_blocks in
      vars, curr_func, u_func_blocks, builder
    | SReturnUnit ->
      ignore (L.build_ret_void (Option.get builder));
      vars, curr_func, func_blocks, builder
    | SReturnVal expr ->
      let ret = build_expr (snd expr) vars the_module (Option.get builder) in
      ignore (L.build_ret ret (Option.get builder));
      vars, curr_func, func_blocks, builder
    | SExpr expr ->
      ignore (build_expr (snd expr) vars the_module (Option.get builder));
      vars, curr_func, func_blocks, builder
    | SIfEnd (expr, blks) ->
      (* expression should be bool *)
      sanitize_types (fst expr) A.Bool;
      let bool_val = build_expr (snd expr) vars the_module (Option.get builder) in

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
      (* expression should be bool *)
      sanitize_types (fst expr) A.Bool;

      let bool_val = build_expr (snd expr) vars the_module (Option.get builder) in

      let then_bb = L.append_block context "then" (Option.get curr_func) in
      let then_builder = Some (L.builder_at_end context then_bb) in
      ignore (process_blocks blks vars curr_func func_blocks then_builder);

      let end_bb = L.append_block context "if_end" (Option.get curr_func) in

      (* We won't deal with this "if_end" basic block here, 
         either ElseEnd or ElifEnd will have to process it *)
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
    | b ->
      raise
        (Failure
           (Printf.sprintf "expression not implemented: %s" (Utils.string_of_sblock b)))
  and process_blocks blocks vars (curr_func : L.llvalue option) func_blocks builder =
    match blocks with
    (* We've declared all objects, lets fill in all function bodies *)
    | [] -> process_func_blocks func_blocks vars
    | block :: rest ->
      let updated_vars, updated_curr_func, u_func_blocks, u_builder =
        process_block block vars curr_func func_blocks builder
      in
      process_blocks rest updated_vars updated_curr_func u_func_blocks u_builder
  and process_elseifs vars block end_bb curr_func func_blocks builder =
    match block with
    | SElseEnd blks ->
      ignore (process_blocks blks vars curr_func func_blocks builder);

      (* TODO: Throw an error or warning if the code is unreachable? *)
      let u_builder = Some (L.builder_at_end context end_bb) in
      u_builder
    | SElifEnd (expr, blks) ->
      sanitize_types (fst expr) A.Bool;

      let bool_val = build_expr (snd expr) vars the_module (Option.get builder) in

      let then_bb = L.append_block context "then" (Option.get curr_func) in
      let then_builder = Some (L.builder_at_end context then_bb) in
      ignore (process_blocks blks vars curr_func func_blocks then_builder);

      let build_br_end = L.build_br end_bb in
      add_terminal (L.builder_at_end context then_bb) build_br_end;

      ignore (L.build_cond_br bool_val then_bb end_bb (Option.get builder));

      let u_builder = Some (L.builder_at_end context end_bb) in
      u_builder
    | SElifNonEnd (expr, blks, else_blk) ->
      sanitize_types (fst expr) A.Bool;

      let bool_val = build_expr (snd expr) vars the_module (Option.get builder) in

      let then_bb = L.append_block context "then" (Option.get curr_func) in
      let then_builder = Some (L.builder_at_end context then_bb) in
      ignore (process_blocks blks vars curr_func func_blocks then_builder);

      let build_br_end = L.build_br end_bb in
      add_terminal (L.builder_at_end context then_bb) build_br_end;

      let else_bb = L.append_block context "else" (Option.get curr_func) in
      let else_builder = Some (L.builder_at_end context else_bb) in
      ignore (L.build_cond_br bool_val then_bb else_bb (Option.get builder));

      (* We haven't reached the End - let's keep going *)
      let u_builder =
        process_elseifs vars else_blk end_bb curr_func func_blocks else_builder
      in

      u_builder
    | _ -> raise (Failure "Only SElseEnd, SElifEnd and SElifNonEnd can follow SIfNonEnd")
  in

  (* we start off in no function.. *)
  let curr_func = None in
  (* ..and have come across no functions.. *)
  let func_blocks = [] in
  (* ..and start off with no builder.. *)
  let builder = None in
  process_blocks blocks local_vars curr_func func_blocks builder;
  the_module
;;
