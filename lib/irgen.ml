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

let udt_structs : (string, L.lltype) Hashtbl.t = Hashtbl.create 10
let udt_field_indices : (string, (string * int) list) Hashtbl.t = Hashtbl.create 10
let string_consts : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10
let string_counter = ref 0

let build_entry_alloca the_function var_name var_type =
  let builder =
    L.builder_at context (L.instr_begin (L.entry_block the_function))
  in
  L.build_alloca var_type var_name builder

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
  | A.UserType name ->
    (try Hashtbl.find udt_structs name with
     | Not_found -> raise (Failure ("Unknown user type: " ^ name)))
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

let lookup_type (var_types : A.typ StringMap.t) var =
  try StringMap.find var var_types with
  | Not_found -> raise (Failure ("type lookup error: " ^ var))
;;

let lookup_value (vars : variable StringMap.t) var =
  try
    let vbl = StringMap.find var vars in
    vbl.v_value
  with
  | Not_found ->
    raise (Failure (Printf.sprintf "var lookup error: failed to find variable %s\n" var))
;;

let define_udt_type name members =
  let field_types = List.map (fun (_, t) -> ltype_of_typ t) members in
  let struct_type = L.struct_type context (Array.of_list field_types) in
  Hashtbl.add udt_structs name struct_type;
  Hashtbl.add udt_field_indices name (List.mapi (fun i (name, _) -> name, i) members)
;;

let build_udt_access typ var_name field_name vars builder =
  let struct_ptr = lookup_value vars var_name in
  let var_typ =
    match typ with
    | A.UserType name -> name
    | _ -> raise (Failure "expected user-defined type")
  in
  let field_indices = Hashtbl.find udt_field_indices var_typ in
  let idx = List.assoc field_name field_indices in
  let field_ptr =
    L.build_struct_gep struct_ptr idx (var_name ^ "_" ^ field_name) builder
  in
  let field_val = L.build_load field_ptr (field_name ^ "_val") builder in
  field_val
;;

let get_or_add_string_const s builder =
  if Hashtbl.mem string_consts s
  then Hashtbl.find string_consts s
  else (
    let name =
      if !string_counter = 0 then "str" else Printf.sprintf "str.%d" !string_counter
    in
    incr string_counter;
    let v = L.build_global_stringptr s name builder in
    Hashtbl.add string_consts s v;
    v)
;;

let rec build_expr expr (vars : variable StringMap.t) var_types the_module builder =
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
    let llval = build_expr e vars var_types the_module builder in
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
    then prelude_print func vars var_types the_module builder
    else if func_name = len_func_name
    then prelude_len func vars var_types the_module builder
    else if func_name = input_func_name
    then prelude_input func vars var_types the_module builder
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
    let se1 = build_expr e1 vars var_types the_module builder in
    let se2 = build_expr e2 vars var_types the_module builder in
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
  | SUDTInstance (typename, fields) ->
    let struct_type = Hashtbl.find udt_structs typename in
    let field_indices = Hashtbl.find udt_field_indices typename in
    let instance = L.build_alloca struct_type (typename ^ "_inst") builder in
    List.iter
      (fun (field_name, sexpr) ->
         let idx = List.assoc field_name field_indices in
         let field_ptr =
           L.build_struct_gep instance idx (typename ^ "_" ^ field_name) builder
         in
         let field_val = build_expr sexpr vars var_types the_module builder in
         ignore (L.build_store field_val field_ptr builder))
      fields;
    instance
  | SUDTAccess (id, SUDTVariable field) ->
    let struct_ptr = lookup_value vars id in
    let id_typ = lookup_type var_types id in
    let type_name =
      match id_typ with
      | A.UserType n -> n
      | _ -> raise (Failure ("Expected user type for variable: " ^ id))
    in
    let field_indices = Hashtbl.find udt_field_indices type_name in
    let idx = List.assoc field field_indices in
    let field_ptr = L.build_struct_gep struct_ptr idx (id ^ "_" ^ field) builder in
    let field_val = L.build_load field_ptr (field ^ "_val") builder in
    field_val
  | SStringLit s ->
    let str_ptr = get_or_add_string_const s builder in
    str_ptr
  | e ->
    raise (Failure (Printf.sprintf "expr not implemented: %s" (Utils.string_of_sexpr e)))

(* This is the "print" prelude function, it is a special function that exists within fly

   Preferrably this function should exist somewhere else, but it needs to be defined with build_expr
*)
and prelude_print (func : sfunc) vars var_types the_module builder =
  if List.length (snd func) != 1
  then failwith "Incorrect number of args to print: expected 1"
  else (
    let func_arg = List.hd (snd func) in
    let lexpr = build_expr func_arg vars var_types the_module builder in
    let args =
      match fst func_arg with
      | A.Int -> [| int_format_str builder; lexpr |]
      | A.Bool ->
        let lexpr = build_expr func_arg vars var_types the_module builder in

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
      | A.Float ->
        let lexpr = build_expr func_arg vars var_types the_module builder in
        [| float_format_str builder; lexpr |]
      | A.String ->
        let lexpr = build_expr func_arg vars var_types the_module builder in
        [| str_format_str builder; lexpr |]
      | _ -> failwith "print not implemented for type"
    in
    L.build_call (print_func the_module) args "call_printf" builder)

and prelude_len (func : sfunc) vars var_types the_module builder =
  let func_arg = List.hd (snd func) in
  let lexpr = build_expr func_arg vars var_types the_module builder in
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

and prelude_input (_func : sfunc) _vars _var_types the_module builder =
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

let add_local_val typ var vars var_types (expr : A.typ * Sast.sx) the_module builder =
  let expr_type = fst expr in
  assert_types expr_type typ;
  let ll_initializer_value : L.llvalue =
    build_expr expr vars var_types the_module builder
  in
  match typ with
  | A.UserType _ ->
    let vbl = { v_value = ll_initializer_value; v_type = typ; v_scope = Local } in
    StringMap.add var vbl vars
  | _ ->
    let local_var_allocation : L.llvalue =
      L.build_alloca (ltype_of_typ typ) var builder
    in
    ignore (L.build_store ll_initializer_value local_var_allocation builder);
    let vbl = { v_value = local_var_allocation; v_type = typ; v_scope = Local } in
    StringMap.add var vbl vars
;;

let add_global_val typ var (vars : variable StringMap.t) _ expr the_module =
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
      let init = get_or_add_string_const s builder in
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
  let local_vars = StringMap.empty in
  let block_map = StringMap.empty in
  let var_types = StringMap.empty in
  List.iter
    (function
      | SUDTDef (name, members) -> define_udt_type name members
      | _ -> ())
    blocks;
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
  let rec process_func_blocks func_blocks vars var_types =
    match func_blocks with
    | [] -> ()
    | _blk :: rst ->
      process_func_block _blk vars var_types;
      process_func_blocks rst vars var_types
  and process_func_block
        (func_block : L.llvalue * A.formal list * sblock list)
        vars
        var_types
    =
    let lfunc, _, blocks = func_block in
    let curr_func = Some lfunc in
    let builder = L.builder_at_end context (L.entry_block lfunc) in
    process_blocks blocks vars var_types curr_func [] (Some builder) block_map
  and process_blocks
        blocks
        vars
        var_types
        (curr_func : L.llvalue option)
        func_blocks
        (builder : L.llbuilder option)
        block_map
    =
    match blocks with
    (* We've declared all objects, lets fill in all function bodies *)
    | [] -> process_func_blocks func_blocks vars var_types
    | block :: rest ->
      let updated_vars, updated_var_types, updated_curr_func, u_func_blocks, u_builder, block_map =
        process_block block vars var_types curr_func func_blocks builder block_map
      in
      process_blocks
        rest
        updated_vars
        updated_var_types
        updated_curr_func
        u_func_blocks
        u_builder
        block_map
  and process_block
        block
        vars
        var_types
        (curr_func : L.llvalue option)
        func_blocks
        (builder : L.llbuilder option)
        block_map
    =
    match block with
    | SUDTDef (name, members) ->
      define_udt_type name members;
      vars, var_types, curr_func, func_blocks, builder, block_map
    | SDeclTyped (id, typ, expr) ->
      if Option.is_some curr_func then (
        let new_vars =
          add_local_val typ id vars var_types expr the_module (Option.get builder)
        in
        let new_var_types = StringMap.add id typ var_types in
        new_vars, new_var_types, curr_func, func_blocks, builder, block_map
        )      
      else (
        let new_vars = add_global_val typ id vars var_types expr the_module in
        let new_var_types = StringMap.add id typ var_types in
        new_vars, new_var_types, curr_func, func_blocks, builder, block_map)
    | SFunctionDefinition (typ, id, formals, body) ->
      let u_func_blocks = declare_function typ id formals body func_blocks in
      vars, var_types, curr_func, u_func_blocks, builder, block_map
    | SReturnUnit ->
      ignore (L.build_ret_void (Option.get builder));
      vars, var_types, curr_func, func_blocks, builder, block_map
    | SReturnVal expr ->
      let ret = build_expr expr vars var_types the_module (Option.get builder) in
      ignore (L.build_ret ret (Option.get builder));
      vars, var_types, curr_func, func_blocks, builder, block_map
    | SExpr expr ->
      ignore (build_expr expr vars var_types the_module (Option.get builder));
      vars, var_types, curr_func, func_blocks, builder, block_map
    | SIfEnd (expr, blks) ->
      let bool_val = build_expr expr vars var_types the_module (Option.get builder) in

      (* We require curr_func to be Some - no if-else in global scope *)
      let then_bb = L.append_block context "then" (Option.get curr_func) in
      let then_builder = Some (L.builder_at_end context then_bb) in
      ignore (process_blocks blks vars var_types curr_func func_blocks then_builder block_map);
      let end_bb = L.append_block context "if_end" (Option.get curr_func) in
      let build_br_end = L.build_br end_bb in
      add_terminal (L.builder_at_end context then_bb) build_br_end;
      ignore (L.build_cond_br bool_val then_bb end_bb (Option.get builder));
      let u_builder = Some (L.builder_at_end context end_bb) in
      vars, var_types, curr_func, func_blocks, u_builder, block_map
    | SIfNonEnd (expr, blks, else_blk) ->
      assert_types (fst expr) A.Bool;
      let bool_val = build_expr expr vars var_types the_module (Option.get builder) in

      let then_bb = L.append_block context "then" (Option.get curr_func) in
      let then_builder = Some (L.builder_at_end context then_bb) in
      ignore (process_blocks blks vars var_types curr_func func_blocks then_builder block_map);
      let end_bb = L.append_block context "if_end" (Option.get curr_func) in
      let else_bb = L.append_block context "else" (Option.get curr_func) in
      let else_builder = Some (L.builder_at_end context else_bb) in
      ignore (L.build_cond_br bool_val then_bb else_bb (Option.get builder));
      let u_builder =
        process_elseifs vars else_blk end_bb curr_func func_blocks var_types else_builder
      in
      let build_br_end = L.build_br end_bb in
      add_terminal (L.builder_at_end context then_bb) build_br_end;
      add_terminal (L.builder_at_end context else_bb) build_br_end;
      vars, var_types, curr_func, func_blocks, u_builder, block_map
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
      vars, var_types, curr_func, func_blocks, builder, block_map

    | SFor (loop_var, checked_iterable, checked_body) ->
      (* checked_itrable is a tuple of the following for: 
      (list of types of all elements in t , list of elements all of same type )*)
      let curr_func = Option.get curr_func in
      let builder = Option.get builder in
  
      (* Evaluate the iterable to get the list struct *)
      let list_val = build_expr checked_iterable vars var_types the_module builder in
  
      (* Extract length and data pointer from list struct *)
      let list_length = L.build_extractvalue list_val 0 "list_length" builder in
      let list_data_ptr = L.build_extractvalue list_val 1 "list_data" builder in
  
      (* Allocate index variable *)
      let index_alloca = build_entry_alloca curr_func "i" (L.i32_type context) in
      ignore (L.build_store (L.const_int (L.i32_type context) 0) index_alloca builder);
  
      (* Allocate loop variable (e.g. x) *)
      let loop_var_alloca = build_entry_alloca curr_func loop_var (L.i32_type context) in
      let vars = StringMap.add loop_var { v_value = loop_var_alloca; v_type = A.Int; v_scope = Local } vars in
  
      (* Create blocks *)
      let loop_cond_bb = L.append_block context "loop_cond" curr_func in
      let loop_body_bb = L.append_block context "loop_body" curr_func in
      let loop_after_bb = L.append_block context "loop_after" curr_func in
      ignore (L.build_br loop_cond_bb builder);
  
      (* Condition block *)
      let cond_builder = L.builder_at_end context loop_cond_bb in
      let curr_i = L.build_load index_alloca "i_val" cond_builder in
      let cond = L.build_icmp L.Icmp.Slt curr_i list_length "loop_cond" cond_builder in
      ignore (L.build_cond_br cond loop_body_bb loop_after_bb cond_builder);
  
      (* Body block *)
      let body_builder = L.builder_at_end context loop_body_bb in
  
      (* Get current element from list: list_data_ptr[i] *)
      let elem_ptr = L.build_gep list_data_ptr [| curr_i |] "elem_ptr" body_builder in
      let elem_val = L.build_load elem_ptr "elem_val" body_builder in
  
      (* Store current element into loop variable *)
      ignore (L.build_store elem_val loop_var_alloca body_builder);
  
      (* Run loop body *)
      let block_map = StringMap.add "break" loop_after_bb block_map in
      ignore (process_blocks checked_body vars var_types (Some curr_func) func_blocks (Some body_builder) block_map);
  
      (* Increment i and jump back to condition *)
      let updated_body_builder = L.builder_at_end context (L.insertion_block body_builder) in
      let next_i = L.build_add curr_i (L.const_int (L.i32_type context) 1) "i_plus_1" updated_body_builder in
      ignore (L.build_store next_i index_alloca updated_body_builder);
      ignore (L.build_br loop_cond_bb updated_body_builder);
  
      (* After loop *)
      let after_builder = L.builder_at_end context loop_after_bb in
      vars, var_types, Some curr_func, func_blocks, Some after_builder, block_map
  
    | SBreak ->
      let exit_bb = (try
        StringMap.find "break" block_map
      with Not_found ->
        (* this should never actually run becuase sast ensures breaks are only in loops *)
        raise (Failure "Break cannot be placed outside of a loop")) in
        (* go to the while loop exit branch *)
      ignore (L.build_br exit_bb (Option.get builder));
      vars, var_types, curr_func, func_blocks, builder, block_map
    | b ->
      raise
        (Failure
           (Printf.sprintf "expression not implemented: %s" (Utils.string_of_sblock b)))
  and process_elseifs
        vars
        block
        end_bb
        curr_func
        func_blocks
        var_types
        (builder : L.llbuilder option)
    =
    match block with
    | SElseEnd blks ->
      ignore (process_blocks blks vars var_types curr_func func_blocks builder block_map);
      let u_builder = Some (L.builder_at_end context end_bb) in
      u_builder
    | SElifEnd (expr, blks) ->
      assert_types (fst expr) A.Bool;
      let bool_val = build_expr expr vars var_types the_module (Option.get builder) in

      let then_bb = L.append_block context "then" (Option.get curr_func) in
      let then_builder = Some (L.builder_at_end context then_bb) in
      ignore (process_blocks blks vars var_types curr_func func_blocks then_builder block_map);
      let build_br_end = L.build_br end_bb in
      add_terminal (L.builder_at_end context then_bb) build_br_end;
      ignore (L.build_cond_br bool_val then_bb end_bb (Option.get builder));
      let u_builder = Some (L.builder_at_end context end_bb) in
      u_builder
    | SElifNonEnd (expr, blks, else_blk) ->
      assert_types (fst expr) A.Bool;
      let bool_val = build_expr expr vars var_types the_module (Option.get builder) in

      let then_bb = L.append_block context "then" (Option.get curr_func) in
      let then_builder = Some (L.builder_at_end context then_bb) in
      ignore (process_blocks blks vars var_types curr_func func_blocks then_builder block_map);
      let build_br_end = L.build_br end_bb in
      add_terminal (L.builder_at_end context then_bb) build_br_end;
      let else_bb = L.append_block context "else" (Option.get curr_func) in
      let else_builder = Some (L.builder_at_end context else_bb) in
      ignore (L.build_cond_br bool_val then_bb else_bb (Option.get builder));
      let u_builder =
        process_elseifs vars else_blk end_bb curr_func func_blocks var_types else_builder
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
  process_blocks blocks local_vars var_types curr_func func_blocks builder block_map;
  the_module
  
;;
