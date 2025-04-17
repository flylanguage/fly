open OUnit2
open Fly_lib
open Print_lib.Prints

let tests =
  "testing_func_def_and_call1"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "fun countdown(timer: int) {\n\
               \twhile (timer > 0) {\n\
               \t\ttimer -= 1;\n\
               \t}\n\
               }\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "FUN ID(countdown) LPAREN ID(timer) COLON INT RPAREN LBRACE WHILE LPAREN \
             ID(timer) GT LITERAL(0) RPAREN LBRACE ID(timer) MINUS_ASSIGN LITERAL(1) \
             SEMI RBRACE RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "countdown(10);\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "countdown(10);" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

