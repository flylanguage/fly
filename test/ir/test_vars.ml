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

let printer = fun s -> "\n---\n" ^ s ^ "\n---\n"

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
          assert_equal expected actual ~printer)
       ; ("global_var_int"
          >:: fun _ ->
          let sast = get_sast "let a := 5;" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\nsource_filename = \"Fly\"\n\n@a = global i32 5\n"
          in
          assert_equal expected actual ~printer)
         (* ; ("global_var_bool" *)
         (*    >:: fun _ -> *)
         (*    let sast = get_sast "let a := true;" in *)
         (*    let mdl = Irgen.translate sast in *)
         (*    let actual = L.string_of_llmodule mdl in *)
         (*    let expected = *)
         (*      "; ModuleID = 'Fly'\nsource_filename = \"Fly\"\n\n@a = global i1 true\n" *)
         (*    in *)
         (*    assert_equal expected actual ~printer) *)
         (* ; ("global_var_float" *)
         (*    >:: fun _ -> *)
         (*    let sast = get_sast "let a := 10.5;" in *)
         (*    let mdl = Irgen.translate sast in *)
         (*    let actual = L.string_of_llmodule mdl in *)
         (*    let expected = *)
         (*      "; ModuleID = 'Fly'\n\ *)
       (*       source_filename = \"Fly\"\n\n\ *)
       (*       @a = global float 1.050000e+01\n" *)
         (*    in *)
         (*    assert_equal expected actual ~printer) *)
         (* ; ("global_var_char" *)
         (*    >:: fun _ -> *)
         (*    let sast = get_sast "let a := 'a';" in *)
         (*    let mdl = Irgen.translate sast in *)
         (*    let actual = L.string_of_llmodule mdl in *)
         (*    let expected = *)
         (*      "; ModuleID = 'Fly'\nsource_filename = \"Fly\"\n\n@a = global i1 true\n" *)
         (*    in *)
         (*    assert_equal expected actual ~printer) *)
         (* ; ("global_var_string" *)
         (*    >:: fun _ -> *)
         (*    let sast = get_sast "let a := \"abcd\";" in *)
         (*    let mdl = Irgen.translate sast in *)
         (*    let actual = L.string_of_llmodule mdl in *)
         (*    let expected = *)
         (*      "; ModuleID = 'Fly'\nsource_filename = \"Fly\"\n\n@a = global i1 true\n" *)
         (*    in *)
         (*    assert_equal expected actual ~printer) *)
       ; ("local_var_int"
          >:: fun _ ->
          let sast = get_sast "fun function() -> () {let b := 5;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define void @function() {\n\
             entry:\n\
            \  %b = alloca i32, align 4\n\
            \  store i32 5, i32* %b, align 4\n\
             }\n"
          in
          assert_equal expected actual ~printer)
       ; ("local_var_bool"
          >:: fun _ ->
          let sast = get_sast "fun function() -> () {let b := true;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define void @function() {\n\
             entry:\n\
            \  %b = alloca i1, align 1\n\
            \  store i1 true, i1* %b, align 1\n\
             }\n"
          in
          assert_equal expected actual ~printer)
       ; ("local_var_float"
          >:: fun _ ->
          let sast = get_sast "fun function() -> () {let a := 10.5;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define void @function() {\n\
             entry:\n\
            \  %a = alloca float, align 4\n\
            \  store float 1.050000e+01, float* %a, align 4\n\
             }\n"
          in
          assert_equal expected actual ~printer)
         (* ; ("local_var_char" *)
         (*    >:: fun _ -> *)
         (*    let sast = get_sast "fun function() -> () {let a := 'a';}" in *)
         (*    let mdl = Irgen.translate sast in *)
         (*    let actual = L.string_of_llmodule mdl in *)
         (*    let expected = *)
         (*      "; ModuleID = 'Fly'\nsource_filename = \"Fly\"\n\n@a = global i1 true\n" *)
         (*    in *)
         (*    assert_equal expected actual ~printer) *)
         (* ; ("local_var_string" *)
         (*    >:: fun _ -> *)
         (*    let sast = get_sast "fun function() -> () {let a := \"abcd\";}" in *)
         (*    let mdl = Irgen.translate sast in *)
         (*    let actual = L.string_of_llmodule mdl in *)
         (*    let expected = *)
         (*      "; ModuleID = 'Fly'\nsource_filename = \"Fly\"\n\n@a = global i1 true\n" *)
         (*    in *)
         (*    assert_equal expected actual ~printer) *)
       ]
;;

let _ = run_test_tt_main tests
