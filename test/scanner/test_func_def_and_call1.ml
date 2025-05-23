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
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "FUN ID(countdown) LPAREN ID(timer) COLON INT RPAREN LBRACE WHILE LPAREN \
             ID(timer) GT LITERAL(0) RPAREN LBRACE ID(timer) MINUS_ASSIGN LITERAL(1) \
             SEMI RBRACE RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "countdown(10);\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected = "ID(countdown) LPAREN LITERAL(10) RPAREN SEMI" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
