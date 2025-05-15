module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make (String)

let context = L.global_context ()
let udt_structs : (string, L.lltype) Hashtbl.t = Hashtbl.create 10
let udt_field_indices : (string, (string * int) list) Hashtbl.t = Hashtbl.create 10

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

let l_printf : L.lltype = L.var_arg_function_type l_int [| L.pointer_type l_char |]
let print_func the_module : L.llvalue = L.declare_function "printf" l_printf the_module
let int_format_str builder = L.build_global_stringptr "%d\n" "int_fmt" builder
let str_format_str builder = L.build_global_stringptr "%s\n" "str_fmt" builder
let float_format_str builder = L.build_global_stringptr "%f\n" "float_fmt" builder

let get_lformals_arr (formals : A.formal list) =
  let lformal_list = List.map ltype_of_typ (List.map snd formals) in
  Array.of_list lformal_list
;;

let lookup (vars : L.llvalue StringMap.t) var =
  try StringMap.find var vars with
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
  let struct_ptr = lookup vars var_name in
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

let lookup_type (var_types : A.typ StringMap.t) var =
  try StringMap.find var var_types with
  | Not_found -> raise (Failure ("type lookup error: " ^ var))
;;

let rec build_expr expr vars var_types the_module builder =
  match expr with
  | SLiteral l -> L.const_int l_int l
  | SBoolLit b -> L.const_int l_bool (if b then 1 else 0)
  | SFloatLit f -> L.const_float l_float f
  | SId var -> L.build_load (lookup vars var) var builder
  | SFunctionCall func ->
    let func_name = fst func in

    if func_name = print_func_name
    then print func vars var_types the_module builder
    else raise (Failure "function calls not implemented")
  | SEnumAccess (enum_name, variant_name) ->
    let key = enum_name ^ "::" ^ variant_name in
    if not (StringMap.mem key vars)
    then
      raise
        (Failure
           (Printf.sprintf "Enum variant %s not found in enum %s" variant_name enum_name))
    else lookup vars key
  | SBinop (e1, op, e2) ->
    let typ = fst e1 in
    let se1 = build_expr (snd e1) vars var_types the_module builder in
    let se2 = build_expr (snd e2) vars var_types the_module builder in
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
  | SUDTInstance (typename, fields) ->
    let struct_type = Hashtbl.find udt_structs typename in
    let field_indices = Hashtbl.find udt_field_indices typename in
    let instance = L.build_alloca struct_type (typename ^ "_inst") builder in
    List.iter
      (fun (field_name, (_, sx)) ->
         let idx = List.assoc field_name field_indices in
         let field_ptr =
           L.build_struct_gep instance idx (typename ^ "_" ^ field_name) builder
         in
         let field_val = build_expr sx vars var_types the_module builder in
         ignore (L.build_store field_val field_ptr builder))
      fields;
    instance
  | SUDTAccess (id, SUDTVariable field) ->
    let struct_ptr = lookup vars id in
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
    let str_ptr =
      L.build_global_stringptr s ("str_" ^ Digest.to_hex (Digest.string s)) builder
    in
    str_ptr
  | e ->
    raise (Failure (Printf.sprintf "expr not implemented: %s" (Utils.string_of_sexpr e)))

(* This is the "print" prelude function, it is a special function that exists within fly

   Preferrably this function should exist somewhere else, but it needs to be defined with build_expr
*)
and print (func : sfunc) vars var_types the_module builder =
  if List.length (snd func) != 1
  then failwith "Incorrect number of args to print: expected 1"
  else (
    let func_arg = List.hd (snd func) in
    let arr =
      match func_arg with
      | A.Int, e ->
        let lexpr = build_expr e vars var_types the_module builder in
        [| int_format_str builder; lexpr |]
      | A.Bool, e ->
        let lexpr = build_expr e vars var_types the_module builder in

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
      | A.Float, e ->
        let lexpr = build_expr e vars var_types the_module builder in
        [| float_format_str builder; lexpr |]
      | A.String, e ->
        let lexpr = build_expr e vars var_types the_module builder in
        [| str_format_str builder; lexpr |]
      | _, _ -> failwith "print not implemented for type"
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

let add_local_val typ var vars var_types (expr : A.typ * Sast.sx) the_module builder =
  let expr_type = fst expr in
  assert_types expr_type typ;
  let sx = snd expr in
  let ll_initializer_value : L.llvalue =
    build_expr sx vars var_types the_module builder
  in
  match typ with
  | A.UserType _ -> StringMap.add var ll_initializer_value vars
  | _ ->
    let local_var_allocation : L.llvalue =
      L.build_alloca (ltype_of_typ typ) var builder
    in
    ignore (L.build_store ll_initializer_value local_var_allocation builder);
    StringMap.add var local_var_allocation vars
;;

let add_global_val typ var vars _ expr the_module =
  let etyp = fst expr in
  assert_types etyp typ;

  let global =
    match typ, snd expr with
    | A.Int, SLiteral i ->
      let init = L.const_int l_int i in
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
    process_blocks blocks vars var_types curr_func [] (Some builder)
  and process_blocks
        blocks
        vars
        var_types
        (curr_func : L.llvalue option)
        func_blocks
        (builder : L.llbuilder option)
    =
    match blocks with
    (* We've declared all objects, lets fill in all function bodies *)
    | [] -> process_func_blocks func_blocks vars var_types
    | block :: rest ->
      let updated_vars, updated_var_types, updated_curr_func, u_func_blocks, u_builder =
        process_block block vars var_types curr_func func_blocks builder
      in
      process_blocks
        rest
        updated_vars
        updated_var_types
        updated_curr_func
        u_func_blocks
        u_builder
  and process_block
        block
        vars
        var_types
        (curr_func : L.llvalue option)
        func_blocks
        (builder : L.llbuilder option)
    =
    match block with
    | SUDTDef (name, members) ->
      define_udt_type name members;
      vars, var_types, curr_func, func_blocks, builder
    | SDeclTyped (id, typ, expr) ->
      if Option.is_some curr_func
      then (
        let new_vars =
          add_local_val typ id vars var_types expr the_module (Option.get builder)
        in
        let new_var_types = StringMap.add id typ var_types in
        new_vars, new_var_types, curr_func, func_blocks, builder)
      else (
        let new_vars = add_global_val typ id vars var_types expr the_module in
        let new_var_types = StringMap.add id typ var_types in
        new_vars, new_var_types, curr_func, func_blocks, builder)
    | SFunctionDefinition (typ, id, formals, body) ->
      let u_func_blocks = declare_function typ id formals body func_blocks in
      vars, var_types, curr_func, u_func_blocks, builder
    | SReturnUnit ->
      ignore (L.build_ret_void (Option.get builder));
      vars, var_types, curr_func, func_blocks, builder
    | SReturnVal expr ->
      let ret = build_expr (snd expr) vars var_types the_module (Option.get builder) in
      ignore (L.build_ret ret (Option.get builder));
      vars, var_types, curr_func, func_blocks, builder
    | SExpr expr ->
      ignore (build_expr (snd expr) vars var_types the_module (Option.get builder));
      vars, var_types, curr_func, func_blocks, builder
    | SIfEnd (expr, blks) ->
      let bool_val =
        build_expr (snd expr) vars var_types the_module (Option.get builder)
      in
      let then_bb = L.append_block context "then" (Option.get curr_func) in
      let then_builder = Some (L.builder_at_end context then_bb) in
      ignore (process_blocks blks vars var_types curr_func func_blocks then_builder);
      let end_bb = L.append_block context "if_end" (Option.get curr_func) in
      let build_br_end = L.build_br end_bb in
      add_terminal (L.builder_at_end context then_bb) build_br_end;
      ignore (L.build_cond_br bool_val then_bb end_bb (Option.get builder));
      let u_builder = Some (L.builder_at_end context end_bb) in
      vars, var_types, curr_func, func_blocks, u_builder
    | SIfNonEnd (expr, blks, else_blk) ->
      assert_types (fst expr) A.Bool;
      let bool_val =
        build_expr (snd expr) vars var_types the_module (Option.get builder)
      in
      let then_bb = L.append_block context "then" (Option.get curr_func) in
      let then_builder = Some (L.builder_at_end context then_bb) in
      ignore (process_blocks blks vars var_types curr_func func_blocks then_builder);
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
      vars, var_types, curr_func, func_blocks, u_builder
    | SEnumDeclaration (id, variants) ->
      let enum_type = L.named_struct_type context id in
      let fields = Array.of_list (List.map (fun _ -> L.i32_type context) variants) in
      ignore (L.struct_set_body enum_type fields true);
      let rec assign_enum_values variants last_value =
        match variants with
        | [] -> []
        | SEnumVariantDefault n :: rest ->
          let curr = id ^ "::" ^ n, L.const_int l_int last_value in
          curr :: assign_enum_values rest (last_value + 1)
        | SEnumVariantExplicit (n, v) :: rest ->
          let curr = id ^ "::" ^ n, L.const_int l_int v in
          curr :: assign_enum_values rest (v + 1)
      in
      let vars =
        assign_enum_values variants 0
        |> List.fold_left (fun acc (name, value) -> StringMap.add name value acc) vars
      in
      vars, var_types, curr_func, func_blocks, builder
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
      ignore (process_blocks blks vars var_types curr_func func_blocks builder);
      let u_builder = Some (L.builder_at_end context end_bb) in
      u_builder
    | SElifEnd (expr, blks) ->
      assert_types (fst expr) A.Bool;
      let bool_val =
        build_expr (snd expr) vars var_types the_module (Option.get builder)
      in
      let then_bb = L.append_block context "then" (Option.get curr_func) in
      let then_builder = Some (L.builder_at_end context then_bb) in
      ignore (process_blocks blks vars var_types curr_func func_blocks then_builder);
      let build_br_end = L.build_br end_bb in
      add_terminal (L.builder_at_end context then_bb) build_br_end;
      ignore (L.build_cond_br bool_val then_bb end_bb (Option.get builder));
      let u_builder = Some (L.builder_at_end context end_bb) in
      u_builder
    | SElifNonEnd (expr, blks, else_blk) ->
      assert_types (fst expr) A.Bool;
      let bool_val =
        build_expr (snd expr) vars var_types the_module (Option.get builder)
      in
      let then_bb = L.append_block context "then" (Option.get curr_func) in
      let then_builder = Some (L.builder_at_end context then_bb) in
      ignore (process_blocks blks vars var_types curr_func func_blocks then_builder);
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
  process_blocks blocks local_vars var_types curr_func func_blocks builder;
  the_module
;;
