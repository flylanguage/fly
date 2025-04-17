open OUnit2
open Fly_lib
open Print_lib.Prints

let tests =
  "testing_divide"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a1 := 10;\nlet b1 := 2;\return a1 / b1;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "let a1 := 10;\nlet b1 := 2;\nreturn a1 / b1;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a2 := 5.5;\nlet b2 := 2.0;\nreturn a2 / b2;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a2 := 5.5;\nlet b2 := 2.0;\nreturn a2 / b2;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a3 := 7;\nlet b3 := 3;\nreturn a3 / b3;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a3 := 7;\nlet b3 := 3;\nreturn a3 / b3;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a4 := 10;\nlet b4 := 0;\nreturn a4 / b4;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a4 := 10;\nlet b4 := 0;\nreturn a4 / b4;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a5 := 10;\nlet b5 := 2.5;\nreturn a5 / b5;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a5 := 10;\nlet b5 := 2.5;\nreturn a5 / b5;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test6"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a6 := 7.5;\nlet b6 := 2;\nreturn a6 / b6;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a6 := 7.5;\nlet b6 := 2;\nreturn a6 / b6;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test7"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a7 := (5, 3);\nlet b7 := 2;\na7 / b7;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a7 := (5, 3);\nlet b7 := 2;\nreturn a7 / b7;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test8"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a8 := 10;\nlet b8 := 2;\nlet c8 := 2;\nreturn a8/(b8 - c8);\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
             "let a8 := 10;\nlet b8 := 2;\nlet c8 := 2;\nreturn a8 / b8 - c8;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

