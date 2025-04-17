open OUnit2
open Fly_lib
open Print_lib.Prints


let tests =
  "testing_func_def_and_call2"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "fun map(f: fun a -> b) -> fun list<a> -> list<b> {\n
               \treturn match l {\n\
               \t\t[] -> [],\n\
               \t\thead :: tail -> map(f)(tail),\n\
               \t}\n\
               }\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "fun map(f: fun a -> b) -> fun list<a> -> list<b> {\n
               return match l {\n\
                [] -> [],\n\
               \t\thead :: tail -> map(f)(tail),\n\
               \t}\n\
               }\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let lst: list<int> = [0,1,2,3,4];\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let lst: list<int> = [0, 1, 2, 3, 4];\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "fun typecast(x: int) -> float {\n\treturn float(x);\n}\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "FUN ID(typecast) LPAREN ID(x) COLON INT RPAREN ARROW FLOAT LBRACE RETURN \
             FLOAT LPAREN ID(x) RPAREN SEMI RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let float_lst := map(lst)(typecast);\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "LET ID(float_lst) WALRUS ID(map) LPAREN ID(lst) RPAREN LPAREN ID(typecast) \
             RPAREN SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

