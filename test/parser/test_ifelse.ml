open OUnit2
open Fly_lib
open Fly_lib.Prints


let tests =
  "testing_ifelse"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let x := 10;\nif (x > 5) {\n}\nelse if (x > 4) {\n}\nelse {\n}\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "let x := 10;\nif (x > 5) {\n\n} else if (x > 4) {\n\n} else {\n\n}"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

