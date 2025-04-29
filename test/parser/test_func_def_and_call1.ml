open OUnit2
open Fly_lib
open Print_lib.Prints

let tests =
  "testing_func_def_and_call1"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "fun countdown(timer: int) {\n\
               \twhile (timer > 0) {\n\
               \t\ttimer -= 1;\n\
               \t}\n\
               }\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "fun countdown(timer: int, ) -> () {\n\
             while (timer > 0) {\n\
             timer -= 1;\n\n\
             }\n\
             }\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let x := countdown(10);\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "let x := countdown(10);\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
