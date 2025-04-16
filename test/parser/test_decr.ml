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
  "testing_and"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a1 := 2; a1--;" in
          let actual = Parser.program Scanner.tokenize lexbuf in print_endline (string_of_program program) in
          let expected = "let a1 := 2; a1--;"
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a2 := 2; --a2;" in
          let actual = Parser.program Scanner.tokenize lexbuf in print_endline (string_of_program program) in
          let expected = "let a2 := 2; --a2;"
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

