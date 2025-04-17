open OUnit2
open Fly_lib
open Print_lib.Prints


let tests =
  "testing_list"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a1 := [1, 2, 3];\na1[0] = 4;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in 
          let expected =
            "LET MUT ID(a1) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACKET SEMI ID(a1) LBRACKET LITERAL(0) RBRACKET EQUAL LITERAL(4) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a2 := [1, 2, 3];\nlet a3 := 0 :: a2;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a2) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACKET SEMI LET ID(a3) WALRUS LITERAL(0) DCOLON ID(a2) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a4 := [1, 2, 3];\nlet a5 := a3 :: [4, 5];\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a4) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACKET SEMI LET ID(a5) WALRUS ID(a3) DCOLON LBRACKET LITERAL(4) COMMA \
             LITERAL(5) RBRACKET SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a6 := [1, 2, 3];\na6[1] := 5;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a6) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACKET SEMI ID(a6) LBRACKET LITERAL(1) RBRACKET WALRUS LITERAL(5) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let mut a7 := [1, 2, 3];\nlet b7 := a7[2];\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET MUT ID(a7) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACKET SEMI LET ID(b7) WALRUS ID(a7) LBRACKET LITERAL(2) RBRACKET SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let mut a8 := [1, 2, 3];\nlet b8 := a8 == [1, 2, 3];\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET MUT ID(a8) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACKET SEMI LET ID(b8) WALRUS ID(a8) BEQ LBRACKET LITERAL(1) COMMA \
             LITERAL(2) COMMA LITERAL(3) RBRACKET SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let mut a9 := [1, 2, 3];\nlet b9 := \"string\" :: a9;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET MUT ID(a9) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACKET SEMI LET ID(b9) WALRUS SLIT(string) DCOLON ID(a9) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test8"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let mut a10 := [true, false];\nlet b10 := !a10[0];\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET MUT ID(a10) WALRUS LBRACKET BLIT(true) COMMA BLIT(false) RBRACKET SEMI \
             LET ID(b10) WALRUS NOT ID(a10) LBRACKET LITERAL(0) RBRACKET SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test9"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let mut a11 := [1, 2, 3];\nlet b11 := a11[0] == 1;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET MUT ID(a11) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA \
             LITERAL(3) RBRACKET SEMI LET ID(b11) WALRUS ID(a11) LBRACKET LITERAL(0) \
             RBRACKET BEQ LITERAL(1) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test10"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a12: list<int> = [1, 2, 3];" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a12) COLON LIST LT INT GT EQUAL LBRACKET LITERAL(1) COMMA LITERAL(2) \
             COMMA LITERAL(3) RBRACKET SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test11"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a13: list<char> = ['a', 'b', 'c'];" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a13) COLON LIST LT CHAR GT EQUAL LBRACKET CLIT(a) COMMA CLIT(b) \
             COMMA CLIT(c) RBRACKET SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

