open OUnit2
open Fly_lib
open Fly_lib.Prints


let tests =
  "testing_minusassign"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a1 := 10;\na1 -= 5;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut a1 := 10;\na1 -= 5;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a2 := 5.5;\na2 -= 3.2;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut a2 := 5.5;\na2 -= 3.2;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a3 := 5;\na3 -= 3.5;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut a3 := 5;\na3 -= 3.5;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a4 := 3.5;\na4 -= 2;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut a4 := 3.5;\na4 -= 2;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a5 := 5;\na5 -= \"string\";\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut a5 := 5;\na5 -= \"string\";\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test6"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a6 := \"hello\";\na6 -= 5;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut a6 := \"hello\";\na6 -= 5;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test7"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a7 := (1, 2);\na7 -= 5;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut a7 := (1, 2);\na7 -= 5;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test8"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a8 := [1, 2, 3];\na8 -= 5;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut a8 := [1, 2, 3];\na8 -= 5;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test10"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a10 := true;\na10 -= 5;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut a10 := true;\na10 -= 5;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test11"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a11 := 10;\na11 -= \"string\";\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut a11 := 10;\na11 -= \"string\";\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test12"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a12 := 5.5;\na12 -= [1, 2];\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut a12 := 5.5;\na12 -= [1, 2];\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

