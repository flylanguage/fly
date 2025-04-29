open OUnit2
open Fly_lib
open Fly_lib.Utils

let rec to_list lexbuf =
  let tk = Scanner.tokenize lexbuf in
  match tk with
  | Fly_lib.Parser.EOF -> []
  | t -> t :: to_list lexbuf
;;

let tests =
  "testing_divide"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a1 := 10;\nlet b1 := 2;\na1 / b1;\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a1) WALRUS LITERAL(10) SEMI LET ID(b1) WALRUS LITERAL(2) SEMI ID(a1) \
             DIVIDE ID(b1) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a2 := 5.5;\nlet b2 := 2.0;\na2 / b2;\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a2) WALRUS FLIT(5.500000) SEMI LET ID(b2) WALRUS FLIT(2.000000) SEMI \
             ID(a2) DIVIDE ID(b2) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test3"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a3 := 7;\nlet b3 := 3;\na3 / b3;\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a3) WALRUS LITERAL(7) SEMI LET ID(b3) WALRUS LITERAL(3) SEMI ID(a3) \
             DIVIDE ID(b3) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test4"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a4 := 10;\nlet b4 := 0;\na4 / b4;\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a4) WALRUS LITERAL(10) SEMI LET ID(b4) WALRUS LITERAL(0) SEMI ID(a4) \
             DIVIDE ID(b4) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test5"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a5 := 10;\nlet b5 := 2.5;\na5 / b5;\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a5) WALRUS LITERAL(10) SEMI LET ID(b5) WALRUS FLIT(2.500000) SEMI \
             ID(a5) DIVIDE ID(b5) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test6"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a6 := 7.5;\nlet b6 := 2;\na6 / b6;\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a6) WALRUS FLIT(7.500000) SEMI LET ID(b6) WALRUS LITERAL(2) SEMI \
             ID(a6) DIVIDE ID(b6) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test7"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a7 := (5, 3);\nlet b7 := 2;\na7 / b7;\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a7) WALRUS LPAREN LITERAL(5) COMMA LITERAL(3) RPAREN SEMI LET ID(b7) \
             WALRUS LITERAL(2) SEMI ID(a7) DIVIDE ID(b7) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test8"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a8 := 10;\nlet b8 := 2;\nlet c8 := 2;\na8/(b8 - c8);\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a8) WALRUS LITERAL(10) SEMI LET ID(b8) WALRUS LITERAL(2) SEMI LET \
             ID(c8) WALRUS LITERAL(2) SEMI ID(a8) DIVIDE LPAREN ID(b8) MINUS ID(c8) \
             RPAREN SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
