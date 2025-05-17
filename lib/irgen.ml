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
  ; v_type : resolved_typ
  }

let udt_structs : (string, L.lltype) Hashtbl.t = Hashtbl.create 10
let udt_field_indices : (string, (string * int) list) Hashtbl.t = Hashtbl.create 10

let function_signatures : (resolved_formal list * resolved_typ) StringMap.t ref =
  ref StringMap.empty
;;

let string_literal_cache : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10

let l_int = L.i32_type context
and l_bool = L.i1_type context
and l_char = L.i8_type context
and l_unit = L.void_type context
and l_float = L.float_type context

let l_str = L.pointer_type l_char
let l_strcat : L.lltype = L.function_type l_str [| l_str; l_str |]
let strcat_func the_module = L.declare_function "strcat" l_strcat the_module
let l_strcmp : L.lltype = L.function_type l_int [| l_str; l_str |]
let strcmp_func the_module = L.declare_function "strcmp" l_strcmp the_module

let rec ltype_of_typ = function
  | RInt -> l_int
  | RBool -> l_bool
  | RFloat -> l_float
  (* | A.Char -> l_char *)
  | RUnit -> l_unit
  | RString -> l_str
  | REnumType _ -> l_int
  | RList typ -> L.pointer_type (ltype_of_typ typ)
  | RUserType name ->
    (try Hashtbl.find udt_structs name with
     | Not_found -> raise (Failure ("Unknown user type: " ^ name)))
  | t ->
    failwith (Printf.sprintf "type not implemented: %s" (Utils.string_of_resolved_type t))
;;

let int_format_str builder = L.build_global_stringptr "%d\n" "int_fmt" builder
let str_format_str builder = L.build_global_stringptr "%s\n" "str_fmt" builder
let float_format_str builder = L.build_global_stringptr "%f\n" "float_fmt" builder

let rec extract_id_from_sexpr (sexpr : sexpr) : string =
  match snd sexpr with
  | SId id -> id
  | SUDTAccess (base, _) -> extract_id_from_sexpr base
  | SIndex (base, _) -> extract_id_from_sexpr base
  | _ -> raise (Failure "Expected an identifier or access expression")
;;

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
let l_fgets : L.lltype = L.function_type l_str [| l_str; l_int; file_ptr_type |]
let fgets_func the_module = L.declare_function "fgets" l_fgets the_module

let get_lformals_arr (formals : resolved_formal list) =
  let lformal_list = List.map ltype_of_typ (List.map snd formals) in
  Array.of_list lformal_list
;;

let lookup (vars : variable StringMap.t) var =
  try StringMap.find var vars with
  | Not_found ->
    raise (Failure (Printf.sprintf "var lookup error: failed to find variable %s\n" var))
;;

let lookup_type (var_types : resolved_typ StringMap.t) var =
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

let assert_types typ1 typ2 =
  if typ1 <> typ2
  then
    failwith
      (Printf.sprintf
         "Type mismatch. SAST should catch this! (%s vs %s)"
         (Utils.string_of_resolved_type typ1)
         (Utils.string_of_resolved_type typ2))
;;

let define_udt_type name members =
  let struct_ll_type = L.named_struct_type context name in
  Hashtbl.add udt_structs name struct_ll_type;
  Hashtbl.add
    udt_field_indices
    name
    (List.mapi (fun i (member_name, _) -> member_name, i) members);

  let field_ll_types_list =
    List.map (fun (_, sast_member_typ) -> ltype_of_typ sast_member_typ) members
  in
  let field_ll_types_array = Array.of_list field_ll_types_list in
  L.struct_set_body struct_ll_type field_ll_types_array false
;;

(* let define_udt_type name members = *)
(*   let field_types = List.map (fun (_, t) -> ltype_of_typ t) members in *)
(*   let struct_type = L.struct_type context (Array.of_list field_types) in *)
(*   Hashtbl.add udt_structs name struct_type; *)
(*   Hashtbl.add udt_field_indices name (List.mapi (fun i (name, _) -> name, i) members) *)
(* ;; *)

let build_udt_access typ var_name field_name vars builder =
  let struct_ptr = lookup_value vars var_name in
  let var_typ =
    match typ with
    | RUserType name -> name
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

let rec build_expr expr (vars : variable StringMap.t) var_types the_module builder =
  let sx = snd expr in
  match sx with
  | SLiteral l -> L.const_int l_int l
  | SBoolLit b -> L.const_int l_bool (if b then 1 else 0)
  | SFloatLit f -> L.const_float l_float f
  | SId var ->
    let vbl = lookup vars var in
    if vbl.v_scope == Global && vbl.v_type == RString
    then vbl.v_value
    else if
      vbl.v_type
      |> function
      | RUserType _ -> true
      | _ -> false
    then vbl.v_value (* For structs, return the pointer, not a load *)
    else L.build_load vbl.v_value var builder
  | SUnop (e, op) ->
    let llval = build_expr e vars var_types the_module builder in
    let typ = fst e in
    (match op with
     | A.Not ->
       (match typ with
        | RBool -> L.build_not llval "tmp_not" builder
        | _ ->
          failwith
            ("Unary Not not supported for type " ^ Utils.string_of_resolved_type typ))
     | _ ->
       failwith
         ("Unary operator not supported for type " ^ Utils.string_of_resolved_type typ))
  | SUnopSideEffect (var, op) ->
    let typ = fst expr in
    let ll_ptr = lookup_value vars var in
    let ll_original_val = L.build_load ll_ptr var builder in
    let ll_one =
      match typ with
      | RInt -> L.const_int l_int 1
      | RFloat -> L.const_float l_float 1.0
      | _ ->
        failwith
          (Utils.string_of_resolved_type typ ^ " does not support unary incr or decr")
    in
    let ll_new_val =
      match op with
      | A.Postincr | A.Preincr ->
        (match typ with
         | RInt -> L.build_add ll_original_val ll_one "incr_val" builder
         | RFloat -> L.build_fadd ll_original_val ll_one "incr_val" builder
         | _ -> failwith "Post/Preincr failed")
      | A.Postdecr | A.Predecr ->
        (match typ with
         | RInt -> L.build_sub ll_original_val ll_one "incr_val" builder
         | RFloat -> L.build_fsub ll_original_val ll_one "incr_val" builder
         | _ -> failwith "Post/Predecr failed")
      | _ -> failwith ("Operand " ^ Utils.string_of_op op ^ " Not found")
    in

    ignore (L.build_store ll_new_val ll_ptr builder);

    (match op with
     | A.Postincr | A.Postdecr -> ll_original_val
     | A.Preincr | A.Predecr -> ll_new_val
     | _ -> failwith "Could apply incr/decr to variable")
  | SFunctionCall (func_name, actual_s_exprs_list) ->
    if func_name = print_func_name
    then prelude_print (func_name, actual_s_exprs_list) vars var_types the_module builder
    else if func_name = len_func_name
    then prelude_len (func_name, actual_s_exprs_list) vars var_types the_module builder
    else if func_name = input_func_name
    then prelude_input (func_name, actual_s_exprs_list) vars var_types the_module builder
    else (
      let callee_lfunc : L.llvalue =
        match L.lookup_function func_name the_module with
        | Some f -> f
        | None ->
          failwith
            (Printf.sprintf
               "IRgen build_expr: Function '%s' not found in LLVM module. (Was it \
                declared in Pass 1?)"
               func_name)
      in
      let exp_formals, exp_ret_typ =
        try StringMap.find func_name !function_signatures with
        | Not_found ->
          failwith
            (Printf.sprintf
               "IRgen build_expr: SAST signature for function '%s' not found."
               func_name)
      in
      if List.length exp_formals <> List.length actual_s_exprs_list
      then
        failwith
          (Printf.sprintf
             "IRgen build_expr: Arity mismatch for function '%s'. Expected %d args, got \
              %d."
             func_name
             (List.length exp_formals)
             (List.length actual_s_exprs_list));

      let evaluated_ll_args_list : L.llvalue list =
        List.map2
          (fun (sexpr_arg : sexpr) (_formal_name, resolved_type) ->
             let act_type = fst sexpr_arg in
             assert_types act_type resolved_type;
             build_expr sexpr_arg vars var_types the_module builder)
          actual_s_exprs_list
          exp_formals
      in
      let evaluated_ll_args_array : L.llvalue array =
        Array.of_list evaluated_ll_args_list
      in
      let ret_typ = fst expr in
      assert_types ret_typ exp_ret_typ;
      let call_result_name = if exp_ret_typ = RUnit then "" else func_name ^ "_result" in
      L.build_call callee_lfunc evaluated_ll_args_array call_result_name builder)
  | SEnumAccess (enum_name, variant_name) ->
    let key = extract_id_from_sexpr enum_name ^ "::" ^ variant_name in
    let vbl =
      try StringMap.find key vars with
      | Not_found ->
        failwith
          (Printf.sprintf
             "IRgen: Enum variant %s::%s not found in vars map during SEnumAccess"
             (extract_id_from_sexpr enum_name)
             variant_name)
    in
    if L.type_of vbl.v_value <> L.pointer_type l_int
    then
      failwith
        (Printf.sprintf
           "IRgen SEnumAccess: Expected global %s to be i32*, but got %s"
           key
           (L.string_of_lltype (L.type_of vbl.v_value)));
    L.build_load vbl.v_value (key ^ "_load") builder
  | SBinop (e1, op, e2) ->
    let typ = fst e1 in
    let se1 = build_expr e1 vars var_types the_module builder in
    let se2 = build_expr e2 vars var_types the_module builder in
    (match typ with
     | RString ->
       (match op with
        | A.Add ->
          let len1 = L.build_call (strlen_func the_module) [| se1 |] "strlen1" builder in
          let len2 = L.build_call (strlen_func the_module) [| se2 |] "strlen2" builder in
          let total_len = L.build_add len1 len2 "total_len" builder in
          let total_len_plus_one =
            L.build_add total_len (L.const_int l_int 1) "total_len_plus_one" builder
          in
          let new_str =
            L.build_array_alloca l_char total_len_plus_one "new_str" builder
          in
          let new_str_ptr = L.build_pointercast new_str l_str "new_str_ptr" builder in
          ignore (L.build_store (L.const_int l_char 0) new_str_ptr builder);
          ignore
            (L.build_call
               (strcat_func the_module)
               [| new_str_ptr; se1 |]
               "strcat1"
               builder);
          ignore
            (L.build_call
               (strcat_func the_module)
               [| new_str_ptr; se2 |]
               "strcat2"
               builder);
          new_str_ptr
        | A.Equal ->
          let cmp =
            L.build_call (strcmp_func the_module) [| se1; se2 |] "strcmp" builder
          in
          L.build_icmp L.Icmp.Eq cmp (L.const_int l_int 0) "eq" builder
        | A.Neq ->
          let cmp =
            L.build_call (strcmp_func the_module) [| se1; se2 |] "strcmp" builder
          in
          L.build_icmp L.Icmp.Ne cmp (L.const_int l_int 0) "neq" builder
        | _ ->
          failwith
            (Printf.sprintf
               "String binary operator %s not yet implemented"
               (Utils.string_of_op op)))
     | _ ->
       let lval =
         match typ with
         | RInt ->
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
         | RFloat ->
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
         | RBool ->
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
         | REnumType _ ->
           (match op with
            | A.Equal -> L.build_icmp L.Icmp.Eq
            | A.Neq -> L.build_icmp L.Icmp.Ne
            | _ ->
              failwith
                (Printf.sprintf
                   "Internal Compiler Error: Operator %s on EnumType %s reached IRgen."
                   (Utils.string_of_op op)
                   (Utils.string_of_resolved_type typ)))
         | _ ->
           failwith
             (Printf.sprintf
                "Binary operator %s not yet implemented for type %s"
                (Utils.string_of_op op)
                (Utils.string_of_resolved_type typ))
       in
       lval se1 se2 "tmp" builder)
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
    let struct_ptr = build_expr id vars var_types the_module builder in
    let id_typ = fst id in
    let type_name =
      match id_typ with
      | RUserType n -> n
      | _ ->
        raise (Failure ("Expected user type for variable: " ^ extract_id_from_sexpr id))
    in

    let field_indices = Hashtbl.find udt_field_indices type_name in
    let idx = List.assoc field field_indices in
    let field_ptr =
      L.build_struct_gep struct_ptr idx (extract_id_from_sexpr id ^ "_" ^ field) builder
    in
    let field_val =
      L.build_load field_ptr (extract_id_from_sexpr id ^ "_" ^ field ^ "_val") builder
    in
    field_val
  | SList list ->
    let typ = fst (List.hd list) in
    let lval = L.const_int l_int (List.length list) in
    let llist = L.build_array_alloca (ltype_of_typ typ) lval "list" builder in
    List.iteri
      (fun idx item ->
         let litem = build_expr item vars var_types the_module builder in
         let lidx =
           L.build_in_bounds_gep llist [| L.const_int l_int idx |] "index" builder
         in
         ignore (L.build_store litem lidx builder))
      list;
    llist
  | SStringLit s -> L.build_global_stringptr s "str" builder
  | SIndex (list_expr, index_expr) ->
    let list_val = build_expr list_expr vars var_types the_module builder in
    let index_val = build_expr index_expr vars var_types the_module builder in
    let elem_ptr = L.build_gep list_val [| index_val |] "elem_ptr" builder in
    (match fst expr with
     | RInt | RFloat | RString | RBool | REnumType _ ->
       L.build_load elem_ptr "elem_val" builder
     | RUserType _ -> elem_ptr
     | _ -> failwith "Unsupported list element type for indexing")
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
      | RInt -> [| int_format_str builder; lexpr |]
      | RBool ->
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
      | RFloat -> [| float_format_str builder; lexpr |]
      | RString -> [| str_format_str builder; lexpr |]
      | _ -> failwith "print not implemented for type"
    in
    L.build_call (print_func the_module) args "call_printf" builder)

and prelude_len (func : sfunc) vars var_types the_module builder =
  let func_arg = List.hd (snd func) in
  let lexpr = build_expr func_arg vars var_types the_module builder in
  let args =
    match fst func_arg with
    | RString -> [| lexpr |]
    | t ->
      failwith
        (Printf.sprintf
           "prelude_len not implemented for type: %s"
           (Utils.string_of_resolved_type t))
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

let add_local_val
      typ
      var
      vars
      var_types
      (expr : resolved_typ * Sast.sx)
      the_module
      builder
  =
  let expr_type = fst expr in
  assert_types expr_type typ;
  let ll_initializer_value : L.llvalue =
    build_expr expr vars var_types the_module builder
  in
  match typ with
  | RUserType _ ->
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
    | RInt, SLiteral i ->
      let init = L.const_int l_int i in
      L.define_global var init the_module
    | RString, SStringLit s ->
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
              (Utils.string_of_resolved_type t)
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

let rec process_func_body
          (func_llval : L.llvalue)
          (sast_formals : resolved_formal list)
          (body_sblocks : sblock list)
          (global_vars : variable StringMap.t)
          (global_var_types : resolved_typ StringMap.t)
          (the_module : L.llmodule)
  : unit
  =
  let func_builder = L.builder_at_end context (L.entry_block func_llval) in
  let vars_in_function = ref global_vars in
  let var_types_in_function = ref global_var_types in

  let llvm_params_array = L.params func_llval in
  List.iteri
    (fun i (formal_name, formal_sast_typ) ->
       let llvm_incoming_param_val = llvm_params_array.(i) in
       L.set_value_name formal_name llvm_incoming_param_val;
       let param_alloca =
         L.build_alloca (ltype_of_typ formal_sast_typ) formal_name func_builder
       in
       ignore (L.build_store llvm_incoming_param_val param_alloca func_builder);
       let param_var_record =
         { v_value = param_alloca; v_scope = Local; v_type = formal_sast_typ }
       in
       vars_in_function := StringMap.add formal_name param_var_record !vars_in_function;
       var_types_in_function
       := StringMap.add formal_name formal_sast_typ !var_types_in_function)
    sast_formals;

  ignore
    (process_blocks
       body_sblocks
       !vars_in_function
       !var_types_in_function
       (Some func_llval)
       []
       (Some func_builder)
       the_module);

  let func_llvm_type_actual = L.element_type (L.type_of func_llval) in
  if L.return_type func_llvm_type_actual = l_unit
  then add_terminal func_builder L.build_ret_void

and process_blocks
      (blocks : sblock list)
      (vars : variable StringMap.t)
      (var_types : resolved_typ StringMap.t)
      (curr_func : L.llvalue option)
      (func_blocks : (L.llvalue * resolved_formal list * sblock list) list)
      (builder : L.llbuilder option)
      (the_module : L.llmodule)
  : variable StringMap.t
    * resolved_typ StringMap.t
    * L.llvalue option
    * (L.llvalue * resolved_formal list * sblock list) list
    * L.llbuilder option
  =
  match blocks with
  | [] -> vars, var_types, curr_func, func_blocks, builder
  | sblock_hd :: sblock_rest ->
    let ( vars_after_hd
        , vtypes_after_hd
        , fn_opt_after_hd
        , decl_acc_after_hd
        , builder_after_hd )
      =
      process_block sblock_hd vars var_types curr_func func_blocks builder the_module
    in
    process_blocks
      sblock_rest
      vars_after_hd
      vtypes_after_hd
      fn_opt_after_hd
      decl_acc_after_hd
      builder_after_hd
      the_module

and process_block
      (block : sblock)
      (vars : variable StringMap.t)
      (var_types : resolved_typ StringMap.t)
      (curr_func : L.llvalue option)
      (func_blocks : (L.llvalue * resolved_formal list * sblock list) list)
      (builder : L.llbuilder option)
      (the_module : L.llmodule)
  : variable StringMap.t
    * resolved_typ StringMap.t
    * L.llvalue option
    * (L.llvalue * resolved_formal list * sblock list) list
    * L.llbuilder option
  =
  match block with
  | SDeclTyped (id, typ, expr_init) ->
    if Option.is_none curr_func
    then
      raise
        (Failure "SDeclTyped encountered outside function context during body processing.");
    let current_builder = Option.get builder in
    let updated_vars =
      add_local_val typ id vars var_types expr_init the_module current_builder
    in
    let updated_var_types = StringMap.add id typ var_types in
    updated_vars, updated_var_types, curr_func, func_blocks, builder
  | SExpr expr_stmt ->
    let current_builder = Option.get builder in
    ignore (build_expr expr_stmt vars var_types the_module current_builder);
    vars, var_types, curr_func, func_blocks, builder
  | SReturnVal expr_ret ->
    let current_builder = Option.get builder in
    let ll_expr_val_from_build_expr =
      build_expr expr_ret vars var_types the_module current_builder
    in
    let expr_ret_sast_typ, _expr_ret_sx_detail = expr_ret in
    let ll_actual_val_to_return =
      match expr_ret_sast_typ with
      | RUserType _ ->
        let current_func_llval = Option.get curr_func in
        let func_ll_type = L.element_type (L.type_of current_func_llval) in
        let func_return_ll_type = L.return_type func_ll_type in
        if
          L.classify_type (L.type_of ll_expr_val_from_build_expr) = L.TypeKind.Pointer
          && (L.classify_type func_return_ll_type = L.TypeKind.Struct
              || L.classify_type func_return_ll_type = L.TypeKind.Array)
        then L.build_load ll_expr_val_from_build_expr "load_udt_for_ret" current_builder
        else ll_expr_val_from_build_expr
      | _ -> ll_expr_val_from_build_expr
    in
    ignore (L.build_ret ll_actual_val_to_return current_builder);
    vars, var_types, curr_func, func_blocks, builder
  | SReturnUnit ->
    let current_builder = Option.get builder in
    ignore (L.build_ret_void current_builder);
    vars, var_types, curr_func, func_blocks, builder
  | SIfEnd (cond_expr, then_sblocks) ->
    let current_builder = Option.get builder in
    let ll_cond_val = build_expr cond_expr vars var_types the_module current_builder in
    let ll_function = Option.get curr_func in
    let then_bb = L.append_block context "then" ll_function in
    let then_builder_opt = Some (L.builder_at_end context then_bb) in
    ignore
      (process_blocks
         then_sblocks
         vars
         var_types
         curr_func
         []
         then_builder_opt
         the_module);
    let end_if_bb = L.append_block context "ifcont" ll_function in
    add_terminal (Option.get then_builder_opt) (L.build_br end_if_bb);
    ignore (L.build_cond_br ll_cond_val then_bb end_if_bb current_builder);
    vars, var_types, curr_func, func_blocks, Some (L.builder_at_end context end_if_bb)
  | SIfNonEnd (cond_expr, then_sblocks, else_sblock_construct) ->
    let current_builder = Option.get builder in
    let ll_cond_val = build_expr cond_expr vars var_types the_module current_builder in
    let ll_function = Option.get curr_func in
    let then_bb = L.append_block context "then" ll_function in
    let then_builder_opt = Some (L.builder_at_end context then_bb) in
    ignore
      (process_blocks
         then_sblocks
         vars
         var_types
         curr_func
         []
         then_builder_opt
         the_module);
    let else_bb = L.append_block context "else" ll_function in
    let else_builder_opt = Some (L.builder_at_end context else_bb) in
    let merge_bb = L.append_block context "ifcont" ll_function in
    add_terminal (Option.get then_builder_opt) (L.build_br merge_bb);
    ignore (L.build_cond_br ll_cond_val then_bb else_bb current_builder);
    let final_builder_opt =
      process_elseifs
        vars
        else_sblock_construct
        merge_bb
        curr_func
        var_types
        else_builder_opt
        the_module
        ll_function
    in
    vars, var_types, curr_func, func_blocks, final_builder_opt
  | SFor (loop_var_name, iterable_sexpr, body_sblocks) ->
    let current_builder = Option.get builder in
    let ll_function = Option.get curr_func in
    let ll_list_data_ptr =
      build_expr iterable_sexpr vars var_types the_module current_builder
    in
    let iterable_sast_typ, iterable_sx_detail = iterable_sexpr in
    let list_llvm_length, element_sast_typ =
      match iterable_sast_typ with
      | RList inner_typ ->
        (match iterable_sx_detail with
         | SList concrete_list_items ->
           L.const_int l_int (List.length concrete_list_items), inner_typ
         | SId list_var_name ->
           Printf.eprintf
             "Warning: SFor over SId list '%s'. Placeholder length used. Implement \
              proper length tracking.\n"
             list_var_name;
           L.const_int l_int 3, inner_typ (* !!! PLACEHOLDER / TODO !!! *)
         | _ ->
           failwith
             ("SFor: Iterable RList expression not SList or SId: "
              ^ Utils.string_of_sexpr iterable_sx_detail))
      | _ ->
        failwith
          ("SFor: Iterable expression not RList. Got: "
           ^ Utils.string_of_resolved_type iterable_sast_typ)
    in
    let llvm_element_typ = ltype_of_typ element_sast_typ in

    let pred_bb = L.insertion_block current_builder in
    let cond_bb = L.append_block context (loop_var_name ^ ".cond") ll_function in
    let body_bb = L.append_block context (loop_var_name ^ ".body") ll_function in
    let inc_bb = L.append_block context (loop_var_name ^ ".inc") ll_function in
    let exit_bb = L.append_block context (loop_var_name ^ ".exit") ll_function in

    ignore (L.build_br cond_bb current_builder);

    L.position_at_end cond_bb current_builder;
    let index_phi =
      L.build_phi
        [ L.const_int l_int 0, pred_bb ]
        (loop_var_name ^ "_idx")
        current_builder
    in
    let loop_condition =
      L.build_icmp L.Icmp.Slt index_phi list_llvm_length "loopcond" current_builder
    in
    ignore (L.build_cond_br loop_condition body_bb exit_bb current_builder);

    L.position_at_end body_bb current_builder;
    let element_ptr =
      L.build_in_bounds_gep
        ll_list_data_ptr
        [| index_phi |]
        (loop_var_name ^ "_elem_ptr")
        current_builder
    in
    let element_val =
      L.build_load element_ptr (loop_var_name ^ "_elem_val") current_builder
    in
    let loop_var_alloca = L.build_alloca llvm_element_typ loop_var_name current_builder in
    ignore (L.build_store element_val loop_var_alloca current_builder);

    let body_vars =
      StringMap.add
        loop_var_name
        { v_value = loop_var_alloca; v_scope = Local; v_type = element_sast_typ }
        vars
    in
    let body_var_types = StringMap.add loop_var_name element_sast_typ var_types in

    let ( _processed_body_vars
        , _processed_body_var_types
        , _processed_curr_func
        , _processed_func_blocks
        , builder_after_body_opt )
      =
      process_blocks
        body_sblocks
        body_vars
        body_var_types
        curr_func
        func_blocks
        (Some current_builder)
        the_module
    in
    if Option.is_some builder_after_body_opt
    then add_terminal (Option.get builder_after_body_opt) (L.build_br inc_bb);

    L.position_at_end inc_bb current_builder;
    let next_index_val =
      L.build_add
        index_phi
        (L.const_int l_int 1)
        (loop_var_name ^ "_next_idx")
        current_builder
    in
    ignore (L.build_br cond_bb current_builder);
    L.add_incoming (next_index_val, inc_bb) index_phi;
    L.position_at_end exit_bb current_builder;
    vars, var_types, curr_func, func_blocks, Some current_builder
  | SUDTDef _ | SEnumDeclaration _ | SFunctionDefinition _ ->
    failwith
      (Printf.sprintf
         "Structural item %s found in Pass 2 (body processing); should be Pass 1 only."
         (Utils.string_of_sblock block))
  | s ->
    failwith
      (Printf.sprintf "process_block: Unhandled sblock: %s" (Utils.string_of_sblock s))

and process_elseifs
      (vars : variable StringMap.t)
      (block : sblock)
      (end_bb : L.llbasicblock)
      (curr_func : L.llvalue option)
      (var_types : resolved_typ StringMap.t)
      (builder : L.llbuilder option)
      (the_module : L.llmodule)
      (ll_function : L.llvalue)
  : L.llbuilder option
  =
  match block with
  | SElseEnd else_sblocks ->
    let current_branch_builder = Option.get builder in
    ignore
      (process_blocks
         else_sblocks
         vars
         var_types
         curr_func
         []
         (Some current_branch_builder)
         the_module);
    add_terminal current_branch_builder (L.build_br end_bb);
    Some (L.builder_at_end context end_bb)
  | SElifEnd (elif_cond_expr, elif_then_sblocks) ->
    let current_branch_builder = Option.get builder in
    let ll_elif_cond_val =
      build_expr elif_cond_expr vars var_types the_module current_branch_builder
    in
    let elif_then_bb = L.append_block context "elifthen" ll_function in
    let elif_then_builder_opt = Some (L.builder_at_end context elif_then_bb) in
    ignore
      (process_blocks
         elif_then_sblocks
         vars
         var_types
         curr_func
         []
         elif_then_builder_opt
         the_module);
    add_terminal (Option.get elif_then_builder_opt) (L.build_br end_bb);
    ignore (L.build_cond_br ll_elif_cond_val elif_then_bb end_bb current_branch_builder);
    Some (L.builder_at_end context end_bb)
  | SElifNonEnd (elif_cond_expr, elif_then_sblocks, next_else_sblock_construct) ->
    let current_branch_builder = Option.get builder in
    let ll_elif_cond_val =
      build_expr elif_cond_expr vars var_types the_module current_branch_builder
    in
    let elif_then_bb = L.append_block context "elifthen" ll_function in
    let elif_then_builder_opt = Some (L.builder_at_end context elif_then_bb) in
    ignore
      (process_blocks
         elif_then_sblocks
         vars
         var_types
         curr_func
         []
         elif_then_builder_opt
         the_module);
    add_terminal (Option.get elif_then_builder_opt) (L.build_br end_bb);
    let elif_else_bb = L.append_block context "elifelse" ll_function in
    ignore
      (L.build_cond_br ll_elif_cond_val elif_then_bb elif_else_bb current_branch_builder);
    process_elseifs
      vars
      next_else_sblock_construct
      end_bb
      curr_func
      var_types
      (Some (L.builder_at_end context elif_else_bb))
      the_module
      ll_function
  | _ ->
    failwith
      "Invalid SElseConstruct in process_elseifs; SAST structure might be different."
;;

let translate (sast_toplevel_blocks : sblock list) : L.llmodule =
  let the_module = L.create_module context "Fly" in

  Hashtbl.clear udt_structs;
  Hashtbl.clear udt_field_indices;
  function_signatures := StringMap.empty;
  Hashtbl.clear string_literal_cache;

  let func_definitions_for_pass2
    : (L.llvalue * resolved_formal list * sblock list) list ref
    =
    ref []
  in
  let global_vars = ref StringMap.empty in
  let global_var_types = ref StringMap.empty in

  List.iter
    (fun sblock_item ->
       match sblock_item with
       | SUDTDef (name, members) -> define_udt_type name members
       | SEnumDeclaration (enum_name_str, sast_variants) ->
         let rec process_enum_variants current_vars variant_list current_int_val =
           match variant_list with
           | [] -> current_vars
           | SEnumVariantDefault variant_n :: rest ->
             let assigned_val = current_int_val in
             let ll_const = L.const_int l_int assigned_val in
             let global_name = enum_name_str ^ "::" ^ variant_n in
             let ll_global_ptr = L.define_global global_name ll_const the_module in
             L.set_global_constant true ll_global_ptr;
             let v_rec =
               { v_value = ll_global_ptr
               ; v_type = REnumType enum_name_str
               ; v_scope = Global
               }
             in
             process_enum_variants
               (StringMap.add global_name v_rec current_vars)
               rest
               (assigned_val + 1)
           | SEnumVariantExplicit (variant_n, explicit_val) :: rest ->
             let assigned_val = explicit_val in
             let ll_const = L.const_int l_int assigned_val in
             let global_name = enum_name_str ^ "::" ^ variant_n in
             let ll_global_ptr = L.define_global global_name ll_const the_module in
             L.set_global_constant true ll_global_ptr;
             let v_rec =
               { v_value = ll_global_ptr
               ; v_type = REnumType enum_name_str
               ; v_scope = Global
               }
             in
             process_enum_variants
               (StringMap.add global_name v_rec current_vars)
               rest
               (assigned_val + 1)
         in
         global_vars := process_enum_variants !global_vars sast_variants 0
       | SDeclTyped (id, typ, expr_init) ->
         global_vars
         := add_global_val typ id !global_vars !global_var_types expr_init the_module;
         global_var_types := StringMap.add id typ !global_var_types
       | SFunctionDefinition (ret_sast_typ, func_id, formals_sast, body_sblocks) ->
         function_signatures
         := StringMap.add func_id (formals_sast, ret_sast_typ) !function_signatures;

         let llvm_param_types_array = get_lformals_arr formals_sast in
         let llvm_func_type =
           L.function_type (ltype_of_typ ret_sast_typ) llvm_param_types_array
         in
         let llvm_func_llvalue = L.define_function func_id llvm_func_type the_module in

         func_definitions_for_pass2
         := (llvm_func_llvalue, formals_sast, body_sblocks) :: !func_definitions_for_pass2
       | SExpr _ ->
         Printf.eprintf
           "Warning: Top-level SExpr encountered in Pass 1. Ignoring for now.\n"
       | s_block ->
         Printf.eprintf
           "Warning: Unhandled top-level SBlock in Pass 1: %s\n"
           (Utils.string_of_sblock s_block))
    sast_toplevel_blocks;

  List.iter
    (fun (llvm_func_llvalue, formals_sast, body_sblocks) ->
       process_func_body
         llvm_func_llvalue
         formals_sast
         body_sblocks
         !global_vars
         !global_var_types
         the_module)
    (List.rev !func_definitions_for_pass2);

  the_module
;;
