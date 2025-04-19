open OUnit2
open Fly_lib
open Print_lib.Prints


let tests =
  "testing_list"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "fun nothing() {let mut a1 := [1, 2, 3];\na1[0] = 4;\n}" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in 
          let expected =
            "fun nothing() -> () {\nlet mut a1 := [1, 2, 3];\na1[0] = 4;\n\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       (* ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "fun nothing() {let a2 := [1, 2, 3];\nlet a3 := 0 :: a2;\n}" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "fun nothing() {\nlet a2 := [1, 2, 3];\nlet a3 := 0 :: a2;\n}"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\"")) *)

       (* ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "fun nothing() {\nlet a4 := [1, 2, 3];\nlet a5 := a3 :: [4, 5];\n}"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "fun nothing() {\nlet a4 := [1, 2, 3];\nlet a5 := a3 :: [4, 5];\n\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\"")) *)

       ; ("test4"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "fun nothing() -> bool {let a6 := [1, 2, 3];\na6[1] = 5;\n}" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "fun nothing() -> bool {\nlet a6 := [1, 2, 3];\na6[1] = 5;\n\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "fun nothing() -> float {let mut a7 := [1, 2, 3];\nlet b7 := a7[2];\n}"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "fun nothing() -> float {\nlet mut a7 := [1, 2, 3];\nlet b7 := a7[2];\n\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "fun nothing() -> int {let mut a8 := [1, 2, 3];\nlet b8 := a8 == [1, 2, 3];\n\n}\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "fun nothing() -> int {\nlet mut a8 := [1, 2, 3];\nlet b8 := a8 == [1, 2, 3];\n\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "fun nothing() {let mut a9 := [1, 2, 3];\nlet b9 := \"string\" + a9;\n}"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "fun nothing() -> () {\nlet mut a9 := [1, 2, 3];\nlet b9 := \"string\" + a9;\n\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test8"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "fun nothing() {let mut a10 := [true, false];\nlet b10 := !a10[0];\n}"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "fun nothing() -> () {\nlet mut a10 := [true, false];\nlet b10 := !a10[0];\n\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test9"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "fun nothing() {let mut a11 := [1, 2, 3];\nlet b11 := a11[0] == 1;\n}"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "fun nothing() -> () {\nlet mut a11 := [1, 2, 3];\nlet b11 := a11[0] == 1;\n\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test10"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "fun nothing() {let a12: list<int> = [1, 2, 3];\n}" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "fun nothing() -> () {\nlet a12: list<int> = [1, 2, 3];\n\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test11"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "fun nothing() {\nlet a13: list<char> = ['a', 'b', 'c'];\n}" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "fun nothing() -> () {\nlet a13: list<char> = ['a', 'b', 'c'];\n\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

