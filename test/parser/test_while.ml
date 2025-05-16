open OUnit2
open Fly_lib
open Fly_lib.Utils

let tests =
  "testing_loops"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "fun main() -> () {while { break; } }" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "fun main() -> () {\nwhile {\nbreak;\n}\n}\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
