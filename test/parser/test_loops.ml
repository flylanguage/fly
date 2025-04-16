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
  "testing_loops"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let test_lst: list<int> = [-5, 0, 5, 9, 100];\n"
          in
          let actual = Parser.program Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected =
            "LET ID(test_lst) COLON LIST LT INT GT EQUAL LBRACKET LITERAL(-5) COMMA \
             LITERAL(0) COMMA LITERAL(5) COMMA LITERAL(9) COMMA LITERAL(100) RBRACKET \
             SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "for i, v := test_lst {\n\t//do work here\n}\n"
          in
          let actual = Parser.program Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected = "for i, v := test_lst {
	//do work here
}
"
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "for v := test_lst {\n\t//do work here\n}\n" in
          let actual = Parser.program Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected = "for v := test_lst {
	//do work here
}
"
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut i: int = 0;\n" in
          let actual = Parser.program Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected = "let mut i: int = 0;
"
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "while (i < 5) {\n\t//do work here\n\ti += 1\n}\n"
          in
          let actual = Parser.program Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected =
            "WHILE LPAREN ID(i) LT LITERAL(5) RPAREN LBRACE ID(i) PLUS_ASSIGN LITERAL(1) \
             RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

