open OUnit2
open Fly_lib
module L = Llvm

let get_sast input =
  try
    let lexbuf = Lexing.from_string input in
    let ast = Fly_lib.Parser.program_rule Fly_lib.Scanner.tokenize lexbuf in
    let sast = Fly_lib.Semant.check ast.body in
    sast
  with
  | err ->
    raise
      (Failure
         (Printf.sprintf
            "Error generating sast, is your program correct?: error=%s"
            (Printexc.to_string err)))
;;

let _write_to_file text filename =
  let channel = open_out filename in
  Printf.fprintf channel "%s" text;
  close_out channel
;;

let tests =
  "testing_ir"
  >::: [ ("empty_program"
          >:: fun _ ->
          let sast = get_sast "" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected = "; ModuleID = 'Fly'\nsource_filename = \"Fly\"\n" in
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("empty_function_decl"
          >:: fun _ ->
          let sast = get_sast "fun function() {}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define void @function() {\n\
             entry:\n\
             }\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("empty_function_decl_ret_void"
          >:: fun _ ->
          let sast = get_sast "fun function() -> () {}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define void @function() {\n\
             entry:\n\
             }\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("empty_function_decl_ret_int"
          >:: fun _ ->
          let sast = get_sast "fun function() -> int {}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @function() {\n\
             entry:\n\
             }\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("empty_function_decl_ret_int_with_formals"
          >:: fun _ ->
          let sast = get_sast "fun function(num : int) -> int {}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @function(i32 %0) {\n\
             entry:\n\
             }\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("multiple_functions_decls"
          >:: fun _ ->
          let sast =
            get_sast
              "fun function(num : int) -> int {}\n fun function2(num2 : float) -> float{}"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @function(i32 %0) {\n\
             entry:\n\
             }\n\n\
             define float @function2(float %0) {\n\
             entry:\n\
             }\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("process_function_block"
          >:: fun _ ->
          let sast = get_sast "fun function(num : int) -> int {let b := 5;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @function(i32 %0) {\n\
             entry:\n\
            \  %b = alloca i32, align 4\n\
            \  store i32 5, i32* %b, align 4\n\
             }\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("process_nested_functions"
          >:: fun _ ->
          let sast = get_sast "fun function(num : int) -> int {fun nested() -> () {}}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @function(i32 %0) {\n\
             entry:\n\
             }\n\n\
             define void @nested() {\n\
             entry:\n\
             }\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("return_unit_from_main"
          >:: fun _ ->
          let sast = get_sast "fun main() -> () {return;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define void @main() {\n\
             entry:\n\
            \  ret void\n\
             }\n"
          in
          (* _write_to_file actual "test.out"; *)
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("return_int_from_main"
          >:: fun _ ->
          let sast = get_sast "fun main() -> int {return 123;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @main() {\n\
             entry:\n\
            \  ret i32 123\n\
             }\n"
          in
          (* _write_to_file actual "test.out"; *)
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("return_true_from_main"
          >:: fun _ ->
          let sast = get_sast "fun main() -> bool {return true;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i1 @main() {\n\
             entry:\n\
            \  ret i1 true\n\
             }\n"
          in
          (* _write_to_file actual "test.out"; *)
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("return_false_from_main"
          >:: fun _ ->
          let sast = get_sast "fun main() -> bool {return false;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i1 @main() {\n\
             entry:\n\
            \  ret i1 false\n\
             }\n"
          in
          (* _write_to_file actual "test.out"; *)
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("return_float_from_func"
          >:: fun _ ->
          let sast = get_sast "fun function() -> float {return 10.5;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define float @function() {\n\
             entry:\n\
            \  ret float 1.050000e+01\n\
             }\n"
          in
          (* _write_to_file actual "test.out"; *)
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("return_var_from_main"
          >:: fun _ ->
          let sast = get_sast "fun main() -> int {let a := 5; return a;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @main() {\n\
             entry:\n\
            \  %a = alloca i32, align 4\n\
            \  store i32 5, i32* %a, align 4\n\
            \  %a1 = load i32, i32* %a, align 4\n\
            \  ret i32 %a1\n\
             }\n"
          in
          (* _write_to_file actual "test.out"; *)
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("return_global_var_from_main"
          >:: fun _ ->
          let sast = get_sast "let a := 5; fun main() -> int {return a;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             @a = global i32 5\n\n\
             define i32 @main() {\n\
             entry:\n\
            \  %a = load i32, i32* @a, align 4\n\
            \  ret i32 %a\n\
             }\n"
          in
          (* _write_to_file actual "test.out"; *)
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
         (* TODO: THIS FAILS - we have to get back the outer function builder when leaving nested() *)
         (* ; ("process_nested_functions_with_locals" *)
         (*    >:: fun _ -> *)
         (*    let sast = *)
         (*      get_sast *)
         (*        "fun function(num : int) -> int {\n\ *)
       (*        \    let a := 5;\n\ *)
       (*        \    fun nested() -> () {}\n\ *)
       (*        \    let b := 1;\n\ *)
       (*         }\n" *)
         (*    in *)
         (*    let mdl = Irgen.translate sast in *)
         (*    let actual = L.string_of_llmodule mdl in *)
         (*    let expected = *)
         (*      "; ModuleID = 'Fly'\n\ *)
       (*       source_filename = \"Fly\"\n\n\ *)
       (*       define i32 @function(i32 %0) {\n\ *)
       (*       entry:\n\ *)
       (*       }\n\n\ *)
       (*       define void @nested() {\n\ *)
       (*       entry:\n\ *)
       (*       }\n" *)
         (*    in *)
         (*    assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n")) *)
       ]
;;

let _ = run_test_tt_main tests
