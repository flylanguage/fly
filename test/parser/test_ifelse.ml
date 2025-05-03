open OUnit2
open Fly_lib
open Fly_lib.Utils

let printer = fun s -> "\n" ^ s ^ "\""

let tests =
  "testing_ifelse"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let x := 10;\nif (x > 5) {\n}\nelse if (x > 4) {\n}\nelse {\n}\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in

          let act_sym = List.map string_of_block_name program.body in
          let exp_sym =
            [ "DeclInfer (10)"; "IfNonEnd (x > 5) -> ElifNonEnd (x > 4) -> ElseEnd" ]
          in

          let act_str = string_of_program program in
          let expect_str =
            "let x := 10;\nif (x > 5) {\n\n} else if (x > 4) {\n\n} else {\n\n}"
          in

          List.iter2 (fun es act -> assert_equal es act ~printer) exp_sym act_sym;
          assert_equal expect_str act_str ~printer)
       ]
;;

let _ = run_test_tt_main tests
