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
          (* let _ = Printf.printf "\nhave:\n%s\n" actual in *)
          (* let _ = Printf.printf "\nwant:\n%s\n" expected in *)
          assert_equal expected actual)
       ; ("test1"
          >:: fun _ ->
          let sast = get_sast "let a := 5;" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected = "; ModuleID = 'Fly'\nsource_filename = \"Fly\"\n" in
          (* let _ = Printf.printf "\nhave:\n%s\n" actual in *)
          (* let _ = Printf.printf "\nwant:\n%s\n" expected in *)
          assert_equal expected actual)
       ]
;;

let _ = run_test_tt_main tests
