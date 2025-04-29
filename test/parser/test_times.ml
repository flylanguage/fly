open OUnit2
open Fly_lib
open Fly_lib.Prints

let tests =
  "testing_times"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let x1 := 5;\nlet x2 := 4;\nreturn x1 * x2;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let x1 := 5;\nlet x2 := 4;\nreturn x1 * x2;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let x3 := 5.5;\nlet x4 := 2.0;\nreturn x3 * x4;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let x3 := 5.5;\nlet x4 := 2.;\nreturn x3 * x4;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let x5 := 5;\nlet x6 := 2.5;\nreturn x5 * x6;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let x5 := 5;\nlet x6 := 2.5;\nreturn x5 * x6;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let x7 := 5;\nlet x8 := \"Hello\";\nreturn x7 * x8;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let x7 := 5;\nlet x8 := \"Hello\";\nreturn x7 * x8;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let x9 := true;\nlet x10 := 2;\nreturn x9 * x10;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let x9 := true;\nlet x10 := 2;\nreturn x9 * x10;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let x11 := (1, 2);\nlet x12 := 4;\nreturn x11 * x12;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let x11 := (1, 2);\nlet x12 := 4;\nreturn x11 * x12;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let x13 := [1, 2, 3];\nlet x14 := 2;\nreturn x13 * x14;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let x13 := [1, 2, 3];\nlet x14 := 2;\nreturn x13 * x14;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

