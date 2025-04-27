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
  "testing_eq"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a1 := 5;\nlet b1 := 5;\nif (a1 == b1) {\n} else {\n}\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in 
          let expected =
            "LET ID(a1) WALRUS LITERAL(5) SEMI LET ID(b1) WALRUS LITERAL(5) SEMI IF \
             LPAREN ID(a1) BEQ ID(b1) RPAREN LBRACE RBRACE ELSE LBRACE RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let c1 := 3.14;\nlet d1 := 3.14;\nif (c1 == d1) {\n} else {\n}\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(c1) WALRUS FLIT(3.140000) SEMI LET ID(d1) WALRUS FLIT(3.140000) SEMI \
             IF LPAREN ID(c1) BEQ ID(d1) RPAREN LBRACE RBRACE ELSE LBRACE RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let e1 := \"Hello\";\nlet f1 := \"Hello\";\nif (e1 == f1) {\n} else {\n}\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(e1) WALRUS SLIT(Hello) SEMI LET ID(f1) WALRUS SLIT(Hello) SEMI IF \
             LPAREN ID(e1) BEQ ID(f1) RPAREN LBRACE RBRACE ELSE LBRACE RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let g1 := true;\nlet h1 := true;\nif (g1 == h1) {\n} else {\n}\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(g1) WALRUS BLIT(true) SEMI LET ID(h1) WALRUS BLIT(true) SEMI IF \
             LPAREN ID(g1) BEQ ID(h1) RPAREN LBRACE RBRACE ELSE LBRACE RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a2 := 5;\nlet b2 := 10;\nif (a2 == b2) {\n} else {\n}\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a2) WALRUS LITERAL(5) SEMI LET ID(b2) WALRUS LITERAL(10) SEMI IF \
             LPAREN ID(a2) BEQ ID(b2) RPAREN LBRACE RBRACE ELSE LBRACE RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let c2 := 3.14;\nlet d2 := 2.71;\nif (c2 == d2) {\n} else {\n}\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(c2) WALRUS FLIT(3.140000) SEMI LET ID(d2) WALRUS FLIT(2.710000) SEMI \
             IF LPAREN ID(c2) BEQ ID(d2) RPAREN LBRACE RBRACE ELSE LBRACE RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let e2 := \"Hello\";\nlet f2 := \"World\";\nif (e2 == f2) {\n} else {\n}\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(e2) WALRUS SLIT(Hello) SEMI LET ID(f2) WALRUS SLIT(World) SEMI IF \
             LPAREN ID(e2) BEQ ID(f2) RPAREN LBRACE RBRACE ELSE LBRACE RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test8"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let g2 := true;\nlet h2 := false;\nif (g2 == h2) {\n} else {\n}\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(g2) WALRUS BLIT(true) SEMI LET ID(h2) WALRUS BLIT(false) SEMI IF \
             LPAREN ID(g2) BEQ ID(h2) RPAREN LBRACE RBRACE ELSE LBRACE RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test9"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a3 := 5;\nlet b3 := \"5\";\nif (a3 == b3) {\n} else {\n}\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a3) WALRUS LITERAL(5) SEMI LET ID(b3) WALRUS SLIT(5) SEMI IF LPAREN \
             ID(a3) BEQ ID(b3) RPAREN LBRACE RBRACE ELSE LBRACE RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
