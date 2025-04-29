open OUnit2
open Fly_lib
open Fly_lib.Prints

let tests =
  "testing_neq"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a := 5;\nlet b := 3;\nlet result := a != b;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a := 5;\nlet b := 3;\nlet result := a != b;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a1 := 5.0;\nlet b1 := 3.0;\nlet result1 := a1 != b1;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a1 := 5.;\nlet b1 := 3.;\nlet result1 := a1 != b1;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a2 := 5;\nlet b2 := 5.0;\nlet result2 := a2 != b2;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a2 := 5;\nlet b2 := 5.;\nlet result2 := a2 != b2;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a3 := true;\nlet b3 := false;\nlet result3 := a3 != b3;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a3 := true;\nlet b3 := false;\nlet result3 := a3 != b3;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a4 := \"hello\";\nlet b4 := \"world\";\nlet result4 := a4 != b4;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a4 := \"hello\";\nlet b4 := \"world\";\nlet result4 := a4 != b4;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a5 := 'a';\nlet b5 := 'b';\nlet result5 := a5 != b5;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a5 := 'a';\nlet b5 := 'b';\nlet result5 := a5 != b5;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a6 := 10;\nlet b6 := 10;\nlet result6 := a6 != b6;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a6 := 10;\nlet b6 := 10;\nlet result6 := a6 != b6;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test8"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a7 := 5;\nlet b7 := \"hello\";\nlet result7 := a7 != b7;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a7 := 5;\nlet b7 := \"hello\";\nlet result7 := a7 != b7;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test9"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a8 := true;\nlet b8 := true;\nlet result8 := a8 != b8;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a8 := true;\nlet b8 := true;\nlet result8 := a8 != b8;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test10"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a9 := \"\";\nlet b9 := \"\";\nlet result9 := a9 != b9;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a9 := \"\";\nlet b9 := \"\";\nlet result9 := a9 != b9;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test11"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a10 := [1, 2, 3];\nlet b10 := [1, 2, 3];\nlet result10 := a10 != b10;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a10 := [1, 2, 3];\nlet b10 := [1, 2, 3];\nlet result10 := a10 != b10;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests

