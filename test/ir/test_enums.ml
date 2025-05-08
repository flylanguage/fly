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
  "testing_enum_ir"
  >::: [ ("enum_explicit_values"
          >:: fun _ ->
          let sast =
            get_sast
              "enum HTTP{OK = 200, BadRequest = 400, InternalServerError = 500} fun \
               main() -> int { return HTTP::OK; }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          (* We can't check the exit code here, but we can check that the enum struct is defined and main returns an int *)
          assert_bool
            "enum struct defined"
            (String.contains actual "%HTTP = type { i32, i32, i32 }");
          assert_bool "main returns int" (String.contains actual "define i32 @main()"))
       ; ("enum_implicit_values"
          >:: fun _ ->
          let sast =
            get_sast
              "enum Color{Red, Green, Blue} fun main() -> int { return Color::Blue; }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          assert_bool
            "enum struct defined"
            (String.contains actual "%Color = type { i32, i32, i32 }");
          assert_bool "main returns int" (String.contains actual "define i32 @main()"))
       ; ("enum_mixed_values"
          >:: fun _ ->
          let sast =
            get_sast
              "enum Mixed{A = 1, B, C = 10, D} fun main() -> int { return Mixed::D; }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          assert_bool
            "enum struct defined"
            (String.contains actual "%Mixed = type { i32, i32, i32, i32 }");
          assert_bool "main returns int" (String.contains actual "define i32 @main()"))
       ; ("enum_usage_in_if"
          >:: fun _ ->
          let sast =
            get_sast
              "enum Color{Red, Green, Blue} fun main() -> int { if (true) { return \
               Color::Red; } return Color::Green; }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          assert_bool
            "enum struct defined"
            (String.contains actual "%Color = type { i32, i32, i32 }");
          assert_bool "main returns int" (String.contains actual "define i32 @main()"))
       ]
;;

let _ = run_test_tt_main tests
