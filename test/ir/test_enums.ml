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

let str_contains haystack needle_regex_pattern =
  try
    ignore (Str.search_forward (Str.regexp needle_regex_pattern) haystack 0);
    true
  with
  | Not_found -> false
;;

let tests =
  "testing_enum_ir"
  >::: [ ("enum_explicit_values_and_comparison"
          >:: fun _ ->
          let fly_code =
            "enum HTTP { \n\
            \  OK = 200, \n\
            \  NotFound = 404, \n\
            \  ServerError = 500 \n\
             } \n\
             fun main() -> int { \n\
            \  let status_ok : HTTP = HTTP::OK; \n\
            \  let status_404 := HTTP::NotFound; \n\
            \  if (status_ok == HTTP::OK && status_404 != HTTP::ServerError) { \n\
            \    return 1; \n\
            \  } else { \n\
            \    return 0; \n\
            \  } \n\
             }"
          in
          let sast = get_sast fly_code in
          let mdl = Irgen.translate sast in
          let actual_ir = L.string_of_llmodule mdl in
          assert_bool
            "main function returns i32"
            (str_contains actual_ir "define i32 @main()");
          assert_bool
            "HTTP::OK global is 200"
            (str_contains actual_ir "@\"HTTP::OK\" = .*constant i32 200");
          assert_bool
            "HTTP::NotFound global is 404"
            (str_contains actual_ir "@\"HTTP::NotFound\" = .*constant i32 404");
          assert_bool
            "HTTP::ServerError global is 500"
            (str_contains actual_ir "@\"HTTP::ServerError\" = .*constant i32 500");
          assert_bool
            "icmp eq i32 instruction exists"
            (str_contains actual_ir "icmp eq i32");
          assert_bool
            "icmp ne i32 instruction exists"
            (str_contains actual_ir "icmp ne i32");
          assert_bool "main returns 1" (str_contains actual_ir "ret i32 1"))
       ; ("enum_implicit_values_and_branching"
          >:: fun _ ->
          let fly_code =
            "enum Color { Red, Green, Blue } \n\
             fun main() -> int { \n\
            \  let c1 := Color::Red; \n\
            \  let c2 := Color::Green; \n\
            \  if (c1 == c2) { \n\
            \    return 10; \n\
            \  } else { \n\
            \    if (Color::Blue == Color::Blue) { \n\
            \      return 20; \n\
            \    } else { \n\
            \      return 30; \n\
            \    } \n\
            \  } \n\
             }"
          in
          let sast = get_sast fly_code in
          let mdl = Irgen.translate sast in
          let actual_ir = L.string_of_llmodule mdl in

          assert_bool
            "main function returns i32"
            (str_contains actual_ir "define i32 @main()");
          assert_bool
            "Color::Red global is 0"
            (str_contains actual_ir "@\"Color::Red\" = .*constant i32 0");
          assert_bool
            "Color::Green global is 1"
            (str_contains actual_ir "@\"Color::Green\" = .*constant i32 1");
          assert_bool
            "Color::Blue global is 2"
            (str_contains actual_ir "@\"Color::Blue\" = .*constant i32 2");
          assert_bool
            "icmp eq i32 instruction exists"
            (str_contains actual_ir "icmp eq i32");
          assert_bool "main returns 20" (str_contains actual_ir "ret i32 20"))
       ; ("enum_mixed_values_auto_increment"
          >:: fun _ ->
          let fly_code =
            "enum Sequence { \n\
            \  First = 10, \n\
            \  Second, \n\
            \  Third = 20, \n\
            \  Fourth, \n\
            \  Fifth \n\
             } \n\
             fun main() -> int { \n\
            \  if (Sequence::Second == Sequence::Fourth) { \n\
            \    return 0; \n\
            \  } else { \n\
            \    if (Sequence::Fifth == Sequence::Fifth) { return 22; } \n\
            \    return 99; \n\
            \  } \n\
             }"
          in
          let sast = get_sast fly_code in
          let mdl = Irgen.translate sast in
          let actual_ir = L.string_of_llmodule mdl in

          assert_bool
            "main function returns i32"
            (str_contains actual_ir "define i32 @main()");
          assert_bool
            "Sequence::First global is 10"
            (str_contains actual_ir "@\"Sequence::First\" = .*constant i32 10");
          assert_bool
            "Sequence::Second global is 11"
            (str_contains actual_ir "@\"Sequence::Second\" = .*constant i32 11");
          assert_bool
            "Sequence::Third global is 20"
            (str_contains actual_ir "@\"Sequence::Third\" = .*constant i32 20");
          assert_bool
            "Sequence::Fourth global is 21"
            (str_contains actual_ir "@\"Sequence::Fourth\" = .*constant i32 21");
          assert_bool
            "Sequence::Fifth global is 22"
            (str_contains actual_ir "@\"Sequence::Fifth\" = .*constant i32 22");
          assert_bool "main returns 22" (str_contains actual_ir "ret i32 22"))
       ]
;;

let _ = run_test_tt_main tests
