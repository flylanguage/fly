open OUnit2
open Fly_lib
open Print_lib.Prints

let tests =
  "testing_and"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a1 := true;\nlet b1 := false;\na1 && b1;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "let a1 := true;\nlet b1 := false;\na1 && b1;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a2 := true;\nlet b2 := true;\na2 && b2;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "let a2 := true;\nlet b2 := true;\na2 && b2;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a3 := false;\nlet b3 := false;\na3 && b3;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "let a3 := false;\nlet b3 := false;\na3 && b3;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a4 := true;\nlet b4 := 5;\na4 && b4;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "let a4 := true;\nlet b4 := 5;\na4 && b4;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a5 := false;\nlet b5 := \"hello\";\na5 && b5;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "let a5 := false;\nlet b5 := \"hello\";\na5 && b5;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test6"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "true && false;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "true && false;\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a6 := true;\nlet b6 := false;\n!(a6 && b6);\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "let a6 := true;\nlet b6 := false;\n!a6 && b6;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test8"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a7 := 5;\nlet b7 := 10;\n(a7 < b7) && (b7 > 5);"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "let a7 := 5;\nlet b7 := 10;\na7 < b7 && b7 > 5"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

