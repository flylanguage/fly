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

let int_format_str builder = L.build_global_stringptr "%d\n" "int_fmt" builder
let str_format_str builder = L.build_global_stringptr "%s\n" "str_fmt" builder
let float_format_str builder = L.build_global_stringptr "%f\n" "float_fmt" builder

(* Creates a binding to the llvm libc "printf" function *)
let l_printf : L.lltype = L.var_arg_function_type l_int [| l_str |]
let print_func the_module : L.llvalue = L.declare_function "printf" l_printf the_module

(* Creates a binding to the llvm libc "strlen" function *)
let l_strlen = Llvm.function_type l_int [| l_str |]
let strlen_func the_module = Llvm.declare_function "strlen" l_strlen the_module

(* Creates a binding to the llvm libc "scanf" function *)
let l_scanf : L.lltype = L.var_arg_function_type l_int [| l_str |]
let scanf_func the_module = L.declare_function "scanf" l_scanf the_module

(* declare FILE c struct *)
let file_type = L.named_struct_type context "struct._IO_FILE"
let _ = L.struct_set_body file_type [||] false
let file_ptr_type = Llvm.pointer_type file_type
let get_stdin_type = L.function_type file_ptr_type [||]
let get_stdin_fn the_module = L.declare_function "get_stdin" get_stdin_type the_module

(* Creates a binding to the llvm libc "fgets" function *)
let l_fgets : L.lltype = L.function_type l_str [| l_str; l_int; file_ptr_type |]
let fgets_func the_module = L.declare_function "fgets" l_fgets the_module

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

let rec build_expr expr (vars : variable StringMap.t) the_module builder =
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
    let llval = build_expr e vars the_module builder in
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
  | SFunctionCall func ->
    let func_name = fst func in

    if func_name = print_func_name
    then prelude_print func vars the_module builder
    else if func_name = len_func_name
    then prelude_len func vars the_module builder
    else if func_name = input_func_name
    then prelude_input func vars the_module builder
    else raise (Failure "function calls not implemented")
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
    let se1 = build_expr e1 vars the_module builder in
    let se2 = build_expr e2 vars the_module builder in
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
      | A.String -> failwith "GOT A STRING\n"
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
and prelude_print (func : sfunc) vars the_module builder =
  if List.length (snd func) != 1
  then failwith "Incorrect number of args to print: expected 1"
  else (
    let func_arg = List.hd (snd func) in
    let lexpr = build_expr func_arg vars the_module builder in
    let args =
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
    L.build_call (print_func the_module) args "call_printf" builder)

and prelude_len (func : sfunc) vars the_module builder =
  let func_arg = List.hd (snd func) in
  let lexpr = build_expr func_arg vars the_module builder in
  let args =
    match fst func_arg with
    | A.String -> [| lexpr |]
    | t ->
      failwith
        (Printf.sprintf
           "prelude_len not implemented for type: %s"
           (Utils.string_of_type t))
  in

  L.build_call (strlen_func the_module) args "call_strlen" builder

and prelude_input (_func : sfunc) _vars the_module builder =
  (* The max buffer size for reading strings *)
  let max_strlen = 100 in
  let buffer_type = L.array_type l_char max_strlen in
  let buffer = L.build_alloca buffer_type "buffer" builder in
  let buffer_ptr =
    L.build_gep buffer [| L.const_int l_int 0; L.const_int l_int 0 |] "buffer_ptr" builder
  in

  let stdin_val = L.build_call (get_stdin_fn the_module) [||] "stdin_val" builder in

  let args = [| buffer_ptr; L.const_int l_int max_strlen; stdin_val |] in
  ignore (L.build_call (fgets_func the_module) args "call_fgets" builder);

  buffer_ptr
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
  let ll_initializer_value : L.llvalue = build_expr expr vars the_module builder in

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

      (* Create a temporary builder *)
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
        ( add_local_val typ id vars expr the_module (Option.get builder)
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
      let ret = build_expr expr vars the_module (Option.get builder) in
      ignore (L.build_ret ret (Option.get builder));
      vars, curr_func, func_blocks, builder
    | SExpr expr ->
      ignore (build_expr expr vars the_module (Option.get builder));
      vars, curr_func, func_blocks, builder
    | SIfEnd (expr, blks) ->
      (* expression should be bool *)
      let bool_val = build_expr expr vars the_module (Option.get builder) in

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
      assert_types (fst expr) A.Bool;

      let bool_val = build_expr expr vars the_module (Option.get builder) in

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
      assert_types (fst expr) A.Bool;

      let bool_val = build_expr expr vars the_module (Option.get builder) in

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

      let bool_val = build_expr expr vars the_module (Option.get builder) in

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
