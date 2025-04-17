open OUnit2
open Fly_lib
open Print_lib.Prints


let tests =
  "testing_match"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a1: int = 5;\nmatch (a1) {\n5 -> let res := \"ok\";\n_ -> let res := \"not ok\";}"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a1: int = 5;\nmatch a1 {\n5 -> let res := \"ok\";\n_ -> let res := \"not ok\";\n\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

