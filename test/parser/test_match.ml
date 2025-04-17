open OUnit2
open Fly_lib
open Print_lib.Prints


let tests =
  "testing_match"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a1: int = 5;\n\
              \ match (a1) \n\
               {5 -> let res := \"ok\"\n\
              \ _ -> let res := \"not ok\"};"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(a1) COLON INT EQUAL LITERAL(5) SEMI MATCH LPAREN ID(a1) RPAREN \
             LBRACE LITERAL(5) ARROW LET ID(res) WALRUS SLIT(ok) UNDERSCORE ARROW LET \
             ID(res) WALRUS SLIT(not ok) RBRACE SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

