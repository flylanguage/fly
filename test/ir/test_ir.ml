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

let tests =
  "testing_ir"
  >::: [ ("empty_program"
          >:: fun _ ->
          let sast = get_sast "" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected = "; ModuleID = 'Fly'\nsource_filename = \"Fly\"\n" in
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("global_var"
          >:: fun _ ->
          let sast = get_sast "let a := 5;" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\nsource_filename = \"Fly\"\n\n@a = global i32 0\n"
          in
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
       ]
;;

let _ = run_test_tt_main tests
