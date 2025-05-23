open OUnit2
open Fly_lib
open Fly_lib.Utils

let tests =
  "testing_loops"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let test_lst: list<int> = [-5, 0, 5, 9, 100];\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "let test_lst: list<int> = [-5, 0, 5, 9, 100];\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
         (* ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "for i, v := test_lst {}\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "for i, v := test_lst {\n\n}\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\"")) *)
       ; ("test3"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "for v := test_lst {\n\t//do work here\n}\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "for v := test_lst {\n\n}" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test4"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut i: int = 0;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "let mut i: int = 0;\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test5"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "while (i < 5) {i += 1;\n}\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "while (i < 5) {\ni += 1;\n\n}" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
