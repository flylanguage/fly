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
  >::: [ ("local_var_int"
          >:: fun _ ->
          let sast = get_sast "fun function() -> () {let x := [1,2,3]; }" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            ""
          in
          assert_equal expected actual ~printer)
       ]
;;

let _ = run_test_tt_main tests
