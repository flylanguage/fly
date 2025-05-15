open OUnit2
open Fly_lib
open Fly_lib.Utils

let tests =
  "testing_fun_args"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "print(1 + 1);\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "print(1 + 1);\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let n := 1;\nprint(n + 1);\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "let n := 1;\nprint(n + 1);\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test3"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let n := 1;\nprint(1 + n);\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "let n := 1;\nprint(1 + n);\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
