open OUnit2
open Fly_lib
module L = Llvm

let get_sast input =
  try
    let lexbuf = Lexing.from_string input in
    let ast = Fly_lib.Parser.program_rule Fly_lib.Scanner.tokenize lexbuf in
    let sast = Fly_lib.Semant.check ast.body in
    let unbound_sast = Fly_lib.Unbind.unbind sast in
    unbound_sast
  with
  | err ->
    raise
      (Failure
         (Printf.sprintf
            "Error generating sast for test, is your program correct?: error=%s\n\
             Input:\n\
             %s"
            (Printexc.to_string err)
            input))
;;

let _write_to_file text filename =
  let channel = open_out filename in
  Printf.fprintf channel "%s" text;
  close_out channel
;;

let tests =
  "testing_ir" (* Or "test_func_def_ir" if you prefer grouping *)
  >::: [ ("empty_function_decl_ret_int"
          >:: fun _ ->
          let fly_code = "fun function() -> int { return 0; }" in
          let sast = get_sast fly_code in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @function() {\n\
             entry:\n\
            \  ret i32 0\n\
             }\n"
          in
          assert_equal expected actual ~printer:(fun s ->
            "\n---\n" ^ String.trim s ^ "\n---\n"))
       ; ("function_with_one_formal_param"
          >:: fun _ ->
          let fly_code = "fun function(num : int) -> int { return 0; }" in
          let sast = get_sast fly_code in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @function(i32 %num) {\n\
             entry:\n\
            \  %num1 = alloca i32, align 4\n\
            \  store i32 %num, i32* %num1, align 4\n\
            \  ret i32 0\n\
             }\n"
          in
          assert_equal expected actual ~printer:(fun s ->
            "\n---\n" ^ String.trim s ^ "\n---\n"))
       ; ("function_with_param_and_local_var"
          >:: fun _ ->
          let fly_code =
            "fun function(num : int) -> int {let b : int = 5; return num;}"
          in
          let sast = get_sast fly_code in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @function(i32 %num) {\n\
             entry:\n\
            \  %num1 = alloca i32, align 4\n\
            \  store i32 %num, i32* %num1, align 4\n\
            \  %b = alloca i32, align 4\n\
            \  store i32 5, i32* %b, align 4\n\
            \  %num2 = load i32, i32* %num1, align 4\n\
            \  ret i32 %num2\n\
             }\n"
          in
          assert_equal expected actual ~printer:(fun s ->
            "\n---\n" ^ String.trim s ^ "\n---\n"))
       ]
;;

let _ = run_test_tt_main tests
