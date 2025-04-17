open OUnit2
open Fly_lib
open Print_lib.Prints


let tests =
  "testing_ifelse"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let x := 10;\nif (x > 5) {\n}\nelse if (x > 4) {\n}\nelse {\n}\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "LET ID(x) WALRUS LITERAL(10) SEMI IF LPAREN ID(x) GT LITERAL(5) RPAREN \
             LBRACE RBRACE ELSE IF LPAREN ID(x) GT LITERAL(4) RPAREN LBRACE RBRACE ELSE \
             LBRACE RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

