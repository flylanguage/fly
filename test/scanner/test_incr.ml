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
  "testing_and"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a1 := 2; a1++;" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected = "LET ID(a1) WALRUS LITERAL(2) SEMI ID(a1) INCR SEMI" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a2 := 2; ++a2;" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected = "LET ID(a2) WALRUS LITERAL(2) SEMI INCR ID(a2) SEMI" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
