open OUnit2
open Fly_lib
open Print_lib.Prints

let tests =
  "testing_exp"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a1 := 2;\nlet b1 := 3;\nlet result1 := a1 ** b1;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a1) WALRUS LITERAL(2) SEMI LET ID(b1) WALRUS LITERAL(3) SEMI LET \
             ID(result1) WALRUS ID(a1) EXPONENT ID(b1) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a2 := 2.5;\nlet b2 := 2;\nlet result2 := a2 ** b2;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a2) WALRUS FLIT(2.500000) SEMI LET ID(b2) WALRUS LITERAL(2) SEMI LET \
             ID(result2) WALRUS ID(a2) EXPONENT ID(b2) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a3 := 2;\nlet b3 := 3.5;\nlet result3 := a3 ** b3;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a3) WALRUS LITERAL(2) SEMI LET ID(b3) WALRUS FLIT(3.500000) SEMI LET \
             ID(result3) WALRUS ID(a3) EXPONENT ID(b3) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a4 := 2;\nlet b4 := \"3\";\nlet result4 := a4 ** b4;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a4) WALRUS LITERAL(2) SEMI LET ID(b4) WALRUS SLIT(3) SEMI LET \
             ID(result4) WALRUS ID(a4) EXPONENT ID(b4) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a5 := 2;\nlet b5 := true;\nlet result5 := a5 ** b5;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a5) WALRUS LITERAL(2) SEMI LET ID(b5) WALRUS BLIT(true) SEMI LET \
             ID(result5) WALRUS ID(a5) EXPONENT ID(b5) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a6 := (1, 2);\nlet b6 := 2;\nlet result6 := a6 ** b6;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a6) WALRUS LPAREN LITERAL(1) COMMA LITERAL(2) RPAREN SEMI LET ID(b6) \
             WALRUS LITERAL(2) SEMI LET ID(result6) WALRUS ID(a6) EXPONENT ID(b6) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a7 := [1, 2, 3];\nlet b7 := 3;\nlet result7 := a7 ** b7;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a7) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACKET SEMI LET ID(b7) WALRUS LITERAL(3) SEMI LET ID(result7) WALRUS \
             ID(a7) EXPONENT ID(b7) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test8"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a8 := {1, 2, 3};\nlet b8 := 2;\nlet result8 := a8 ** b8;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a8) WALRUS LBRACE LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACE SEMI LET ID(b8) WALRUS LITERAL(2) SEMI LET ID(result8) WALRUS ID(a8) \
             EXPONENT ID(b8) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test9"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a9 := 0;\nlet b9 := 5;\nlet result9 := a9 ** b9;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a9) WALRUS LITERAL(0) SEMI LET ID(b9) WALRUS LITERAL(5) SEMI LET \
             ID(result9) WALRUS ID(a9) EXPONENT ID(b9) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test10"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a10 := 4;\nlet b10 := -2;\nlet result10 := a10 ** b10;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a10) WALRUS LITERAL(4) SEMI LET ID(b10) WALRUS LITERAL(-2) SEMI LET \
             ID(result10) WALRUS ID(a10) EXPONENT ID(b10) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test11"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a11 := 5.0;\nlet b11 := -3;\nlet result11 := a11 ** b11;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a11) WALRUS FLIT(5.000000) SEMI LET ID(b11) WALRUS LITERAL(-3) SEMI \
             LET ID(result11) WALRUS ID(a11) EXPONENT ID(b11) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test12"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a12 := 1;\nlet b12 := 1000;\nlet result12 := a12 ** b12;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a12) WALRUS LITERAL(1) SEMI LET ID(b12) WALRUS LITERAL(1000) SEMI \
             LET ID(result12) WALRUS ID(a12) EXPONENT ID(b12) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test13"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a13 := -2;\nlet b13 := 3;\nlet result13 := a13 ** b13;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a13) WALRUS LITERAL(-2) SEMI LET ID(b13) WALRUS LITERAL(3) SEMI LET \
             ID(result13) WALRUS ID(a13) EXPONENT ID(b13) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test14"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a14 := -2.5;\nlet b14 := 2;\nlet result14 := a14 ** b14;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a14) WALRUS FLIT(-2.500000) SEMI LET ID(b14) WALRUS LITERAL(2) SEMI \
             LET ID(result14) WALRUS ID(a14) EXPONENT ID(b14) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

