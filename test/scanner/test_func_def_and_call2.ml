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
  "testing_func_def_and_call2"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "fun map(f: fun a -> b) -> fun list<a> -> list<b> {\n\
               \treturn match l {\n\
               \t\t[] -> [],\n\
               \t\thead :: tail -> map(f)(tail),\n\
               \t}\n\
               }\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "FUN ID(map) LPAREN ID(f) COLON FUN ID(a) ARROW ID(b) RPAREN ARROW FUN LIST \
             LT ID(a) GT ARROW LIST LT ID(b) GT LBRACE RETURN MATCH ID(l) LBRACE \
             LBRACKET RBRACKET ARROW LBRACKET RBRACKET COMMA ID(head) DCOLON ID(tail) \
             ARROW ID(map) LPAREN ID(f) RPAREN LPAREN ID(tail) RPAREN COMMA RBRACE \
             RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let lst: list<int> = [0,1,2,3,4]; //map creates a new list so we don't \
               need mut here.\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(lst) COLON LIST LT INT GT EQUAL LBRACKET LITERAL(0) COMMA LITERAL(1) \
             COMMA LITERAL(2) COMMA LITERAL(3) COMMA LITERAL(4) RBRACKET SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "fun typecast(x: int) -> float {\n\treturn float(x);\n}\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "FUN ID(typecast) LPAREN ID(x) COLON INT RPAREN ARROW FLOAT LBRACE RETURN \
             FLOAT LPAREN ID(x) RPAREN SEMI RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test5"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let float_lst := map(lst)(typecast);\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(float_lst) WALRUS ID(map) LPAREN ID(lst) RPAREN LPAREN ID(typecast) \
             RPAREN SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
