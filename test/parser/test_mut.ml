open OUnit2
open Fly_lib
open Print_lib.Prints


let tests =
  "testing_mut"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let x := 5;\nx -= 4;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let x := 5;\nx -= 4;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut y := 4;\ny += 8;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut y := 4;\ny += 8;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let mut lst := [1, 2, 3];\nfun update_lst(lst: list<int>) {\nlst[0] -= 1;\n}"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut lst := [1, 2, 3];\nfun update_lst(lst: list<int>, ) -> () {\nlst[0] -= 1;\n\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests

