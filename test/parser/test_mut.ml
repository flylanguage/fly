open OUnit2
open Fly_lib
open Print_lib.Prints


let tests =
  "testing_mut"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let x := 5;\nx -= 4; //fail\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(x) WALRUS LITERAL(5) SEMI ID(x) MINUS_ASSIGN LITERAL(4) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut y := 4;\ny += 8;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET MUT ID(y) WALRUS LITERAL(4) SEMI ID(y) PLUS_ASSIGN LITERAL(8) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let mut lst := [1, 2, 3];\n\
               lst[0] -= 1;  // Should pass: list is mutable, can modify element\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET MUT ID(lst) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA \
             LITERAL(3) RBRACKET SEMI ID(lst) LBRACKET LITERAL(0) RBRACKET MINUS_ASSIGN \
             LITERAL(1) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

