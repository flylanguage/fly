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
  "testing_times"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let x1 := 5;\nlet x2 := 4;\nx1 * x2;\n" in
          let actual = Parser.program Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected =
            "LET ID(x1) WALRUS LITERAL(5) SEMI LET ID(x2) WALRUS LITERAL(4) SEMI ID(x1) \
             TIMES ID(x2) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let x3 := 5.5;\nlet x4 := 2.0;\nx3 * x4;\n" in
          let actual = Parser.program Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected =
            "LET ID(x3) WALRUS FLIT(5.500000) SEMI LET ID(x4) WALRUS FLIT(2.000000) SEMI \
             ID(x3) TIMES ID(x4) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let x5 := 5;\nlet x6 := 2.5;\nx5 * x6;\n" in
          let actual = Parser.program Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected =
            "LET ID(x5) WALRUS LITERAL(5) SEMI LET ID(x6) WALRUS FLIT(2.500000) SEMI \
             ID(x5) TIMES ID(x6) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let x7 := 5;\nlet x8 := \"Hello\";\nx7 * x8;\n"
          in
          let actual = Parser.program Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected =
            "LET ID(x7) WALRUS LITERAL(5) SEMI LET ID(x8) WALRUS SLIT(Hello) SEMI ID(x7) \
             TIMES ID(x8) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let x9 := true;\nlet x10 := 2;\nx9 * x10;\n" in
          let actual = Parser.program Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected =
            "LET ID(x9) WALRUS BLIT(true) SEMI LET ID(x10) WALRUS LITERAL(2) SEMI ID(x9) \
             TIMES ID(x10) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let x11 := (1, 2);\nlet x12 := 4;\nx11 * x12;\n"
          in
          let actual = Parser.program Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected =
            "LET ID(x11) WALRUS LPAREN LITERAL(1) COMMA LITERAL(2) RPAREN SEMI LET \
             ID(x12) WALRUS LITERAL(4) SEMI ID(x11) TIMES ID(x12) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let x13 := [1, 2, 3];\nlet x14 := 2;\nx13 * x14;\n"
          in
          let actual = Parser.program Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected =
            "LET ID(x13) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACKET SEMI LET ID(x14) WALRUS LITERAL(2) SEMI ID(x13) TIMES ID(x14) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test8"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let x15 := {1, 2};\nlet x16 := 3;\nx15 * x16;\n"
          in
          let actual = Parser.program Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected =
            "LET ID(x15) WALRUS LBRACE LITERAL(1) COMMA LITERAL(2) RBRACE SEMI LET \
             ID(x16) WALRUS LITERAL(3) SEMI ID(x15) TIMES ID(x16) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

