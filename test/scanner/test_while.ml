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
  "testing_loops"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "while {break}\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected = "WHILE LBRACE BREAK RBRACE" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
