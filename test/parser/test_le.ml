open OUnit2
open Fly_lib
open Print_lib.Prints


let tests =
  "testing_le"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a1 := 5;\nlet b1 := 10;\nlet result1 := a1 <= b1;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a1 := 5;\nlet b1 := 10;\nlet result1 := a1 <= b1;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a2 := 3.5;\nlet b2 := 4.2;\nlet result2 := a2 <= b2;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a2 := 3.5;\nlet b2 := 4.2;\nlet result2 := a2 <= b2;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a3 := 5;\nlet b3 := 5;\nlet result3 := a3 <= b3;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a3 := 5;\nlet b3 := 5;\nlet result3 := a3 <= b3;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a4 := 5.5;\nlet b4 := 5.5;\nlet result4 := a4 <= b4;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a4 := 5.5;\nlet b4 := 5.5;\nlet result4 := a4 <= b4;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a5 := 10;\nlet b5 := 5;\nlet result5 := a5 <= b5;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a5 := 10;\nlet b5 := 5;\nlet result5 := a5 <= b5;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a6 := \"hello\";\nlet b6 := \"world\";\nlet result6 := a6 <= b6;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a6 := \"hello\";\nlet b6 := \"world\";\nlet result6 := a6 <= b6;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a7 := true;\nlet b7 := false;\nlet result7 := a7 <= b7;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a7 := true;\nlet b7 := false;\nlet result7 := a7 <= b7;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test8"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a8 := (1, 2);\nlet b8 := (2, 3);\nlet result8 := a8 <= b8;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a8 := (1, 2);\nlet b8 := (2, 3);\nlet result8 := a8 <= b8;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test9"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a9 := [1, 2, 3];\nlet b9 := [4, 5, 6];\nlet result9 := a9 <= b9;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a9 := [1, 2, 3];\nlet b9 := [4, 5, 6];\nlet result9 := a9 <= b9;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test11"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a11 := 5;\nlet b11 := \"string\";\nlet result11 := a11 <= b11;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a11 := 5;\nlet b11 := \"string\";\nlet result11 := a11 <= b11;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test12"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a12 := \"hello\";\nlet b12 := 5;\nlet result12 := a12 <= b12;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a12 := \"hello\";\nlet b12 := 5;\nlet result12 := a12 <= b12;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test13"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a13 := (1, 2);\nlet b13 := 5;\nlet result13 := a13 <= b13;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a13 := (1, 2);\nlet b13 := 5;\nlet result13 := a13 <= b13;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test14"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a14 := [1, 2, 3];\nlet b14 := 5;\nlet result14 := a14 <= b14;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a14 := [1, 2, 3];\nlet b14 := 5;\nlet result14 := a14 <= b14;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test16"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a16 := true;\nlet b16 := 5;\nlet result16 := a16 <= b16;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a16 := true;\nlet b16 := 5;\nlet result16 := a16 <= b16;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test17"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a17 := false;\nlet b17 := \"world\";\nlet result17 := a17 <= b17;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a17 := false;\nlet b17 := \"world\";\nlet result17 := a17 <= b17;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

