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
  "testing_or"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a1 := true;\nlet b1 := false;\nlet result1 := a1 | b1;\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a1) WALRUS BLIT(true) SEMI LET ID(b1) WALRUS BLIT(false) SEMI LET \
             ID(result1) WALRUS ID(a1) OR ID(b1) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a2 := false;\nlet b2 := false;\nlet result2 := a2 | b2;\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a2) WALRUS BLIT(false) SEMI LET ID(b2) WALRUS BLIT(false) SEMI LET \
             ID(result2) WALRUS ID(a2) OR ID(b2) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a3 := true;\nif (a3 | false) {\n\tlet result3 := \"executed\";\n}\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a3) WALRUS BLIT(true) SEMI IF LPAREN ID(a3) OR BLIT(false) RPAREN \
             LBRACE LET ID(result3) WALRUS SLIT(executed) SEMI RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let b3 := false;\nif (b3 | false) {\n\tlet result4 := \"executed\";\n}\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(b3) WALRUS BLIT(false) SEMI IF LPAREN ID(b3) OR BLIT(false) RPAREN \
             LBRACE LET ID(result4) WALRUS SLIT(executed) SEMI RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a4 := true;\nlet b4 := false;\nlet result5 := (a4 | b4) | false;\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a4) WALRUS BLIT(true) SEMI LET ID(b4) WALRUS BLIT(false) SEMI LET \
             ID(result5) WALRUS LPAREN ID(a4) OR ID(b4) RPAREN OR BLIT(false) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test6"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a5 := 5;\nlet result6 := a5 | true;\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a5) WALRUS LITERAL(5) SEMI LET ID(result6) WALRUS ID(a5) OR \
             BLIT(true) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let b5 := false;\n\
               while (b5 | true) {\n\
               \tlet result7 := \"looping\";\n\
               \tbreak;\n\
               }\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(b5) WALRUS BLIT(false) SEMI WHILE LPAREN ID(b5) OR BLIT(true) RPAREN \
             LBRACE LET ID(result7) WALRUS SLIT(looping) SEMI BREAK SEMI RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test8"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let b6 := true;\nlet result9 := b6 | b6;\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(b6) WALRUS BLIT(true) SEMI LET ID(result9) WALRUS ID(b6) OR ID(b6) \
             SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test9"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a7 := false;\nlet b7 := true;\nlet result10 := (a7 | b7) | false;\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a7) WALRUS BLIT(false) SEMI LET ID(b7) WALRUS BLIT(true) SEMI LET \
             ID(result10) WALRUS LPAREN ID(a7) OR ID(b7) RPAREN OR BLIT(false) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
