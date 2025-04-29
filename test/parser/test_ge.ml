open OUnit2
open Fly_lib
open Fly_lib.Utils

let tests =
  "testing_ge"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a1 := 5;\nlet b1 := 3;\nlet result1 := a1 >= b1;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "let a1 := 5;\nlet b1 := 3;\nlet result1 := a1 >= b1;\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a2 := 5.5;\nlet b2 := 3.3;\nlet result2 := a2 >= b2;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "let a2 := 5.5;\nlet b2 := 3.3;\nlet result2 := a2 >= b2;\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a3 := 5;\nlet b3 := 3.7;\nlet result3 := a3 >= b3;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "let a3 := 5;\nlet b3 := 3.7;\nlet result3 := a3 >= b3;\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a4 := \"hello\";\nlet b4 := \"world\";\nlet result4 := a4 >= b4;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "let a4 := \"hello\";\nlet b4 := \"world\";\nlet result4 := a4 >= b4;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a5 := true;\nlet b5 := false;\nlet result5 := a5 >= b5;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "let a5 := true;\nlet b5 := false;\nlet result5 := a5 >= b5;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a6 := (1, 2);\nlet b6 := 3;\nlet result6 := a6 >= b6;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "let a6 := (1, 2);\nlet b6 := 3;\nlet result6 := a6 >= b6;\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a7 := [1, 2, 3];\nlet b7 := 2;\nlet result7 := a7 >= b7;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "let a7 := [1, 2, 3];\nlet b7 := 2;\nlet result7 := a7 >= b7;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test9"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a9 := 4;\nlet b9 := 4;\nlet result9 := a9 >= b9;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "let a9 := 4;\nlet b9 := 4;\nlet result9 := a9 >= b9;\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test10"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a10 := 4.4;\nlet b10 := 4.4;\nlet result10 := a10 >= b10;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "let a10 := 4.4;\nlet b10 := 4.4;\nlet result10 := a10 >= b10;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test11"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a11 := -2;\nlet b11 := -5;\nlet result11 := a11 >= b11;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "let a11 := -2;\nlet b11 := -5;\nlet result11 := a11 >= b11;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test12"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a12 := -2.5;\nlet b12 := -3.3;\nlet result12 := a12 >= b12;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "let a12 := -2.5;\nlet b12 := -3.3;\nlet result12 := a12 >= b12;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test13"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a13 := 2;\nlet b13 := -2;\nlet result13 := a13 >= b13;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "let a13 := 2;\nlet b13 := -2;\nlet result13 := a13 >= b13;\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test14"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a14 := 3.5;\nlet b14 := -1.1;\nlet result14 := a14 >= b14;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "let a14 := 3.5;\nlet b14 := -1.1;\nlet result14 := a14 >= b14;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test15"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a15 := \"hello\";\nlet b15 := 5;\nlet result15 := a15 >= b15;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "let a15 := \"hello\";\nlet b15 := 5;\nlet result15 := a15 >= b15;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
