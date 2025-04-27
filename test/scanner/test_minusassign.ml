open OUnit2
open Fly_lib
open Print_lib.Prints

let rec to_list lexbuf =
  let tk = Scanner.tokenize lexbuf in
  match tk with
  | Fly_lib.Parser.EOF -> []
  | t -> t :: to_list lexbuf
;;

let tests =
  "testing_minusassign"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a1 := 10;\na1 -= 5;\n" in
          let actual = string_of_tokens (to_list lexbuf) in 
          let expected =
            "LET MUT ID(a1) WALRUS LITERAL(10) SEMI ID(a1) MINUS_ASSIGN LITERAL(5) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a2 := 5.5;\na2 -= 3.2;\n" in
          let actual = string_of_tokens (to_list lexbuf) in 
          let expected =
            "LET MUT ID(a2) WALRUS FLIT(5.500000) SEMI ID(a2) MINUS_ASSIGN \
             FLIT(3.200000) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test3"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a3 := 5;\na3 -= 3.5;\n" in
          let actual = string_of_tokens (to_list lexbuf) in 
          let expected =
            "LET MUT ID(a3) WALRUS LITERAL(5) SEMI ID(a3) MINUS_ASSIGN FLIT(3.500000) \
             SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test4"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a4 := 3.5;\na4 -= 2;\n" in
          let actual = string_of_tokens (to_list lexbuf) in 
          let expected =
            "LET MUT ID(a4) WALRUS FLIT(3.500000) SEMI ID(a4) MINUS_ASSIGN LITERAL(2) \
             SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test5"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a5 := 5;\na5 -= \"string\";\n" in
          let actual = string_of_tokens (to_list lexbuf) in 
          let expected =
            "LET MUT ID(a5) WALRUS LITERAL(5) SEMI ID(a5) MINUS_ASSIGN SLIT(string) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test6"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a6 := \"hello\";\na6 -= 5;\n" in
          let actual = string_of_tokens (to_list lexbuf) in 
          let expected =
            "LET MUT ID(a6) WALRUS SLIT(hello) SEMI ID(a6) MINUS_ASSIGN LITERAL(5) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test7"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a7 := (1, 2);\na7 -= 5;\n" in
          let actual = string_of_tokens (to_list lexbuf) in 
          let expected =
            "LET MUT ID(a7) WALRUS LPAREN LITERAL(1) COMMA LITERAL(2) RPAREN SEMI ID(a7) \
             MINUS_ASSIGN LITERAL(5) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test8"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a8 := [1, 2, 3];\na8 -= 5;\n" in
          let actual = string_of_tokens (to_list lexbuf) in 
          let expected =
            "LET MUT ID(a8) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACKET SEMI ID(a8) MINUS_ASSIGN LITERAL(5) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test9"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a9 := {1, 2, 3};\na9 -= 5;\n" in
          let actual = string_of_tokens (to_list lexbuf) in 
          let expected =
            "LET MUT ID(a9) WALRUS LBRACE LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACE SEMI ID(a9) MINUS_ASSIGN LITERAL(5) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test10"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a10 := true;\na10 -= 5;\n" in
          let actual = string_of_tokens (to_list lexbuf) in 
          let expected =
            "LET MUT ID(a10) WALRUS BLIT(true) SEMI ID(a10) MINUS_ASSIGN LITERAL(5) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test11"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a11 := 10;\na11 -= \"string\";\n" in
          let actual = string_of_tokens (to_list lexbuf) in 
          let expected =
            "LET MUT ID(a11) WALRUS LITERAL(10) SEMI ID(a11) MINUS_ASSIGN SLIT(string) \
             SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test12"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut a12 := 5.5;\na12 -= [1, 2];\n" in
          let actual = string_of_tokens (to_list lexbuf) in 
          let expected =
            "LET MUT ID(a12) WALRUS FLIT(5.500000) SEMI ID(a12) MINUS_ASSIGN LBRACKET \
             LITERAL(1) COMMA LITERAL(2) RBRACKET SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
