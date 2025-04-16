open OUnit2
open Fly_lib
open Print_lib.Prints

let tests =
  "testing_eq"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a1 := 5;\nlet b1 := 5;\nif (a1 == b1) {\n} else {\n}\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf 
          let actual = string_of_program program in
          let expected =
            "let a1 := 5;\nlet b1 := 5;\nif (a1 == b1) {\n} else {\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let c1 := 3.14;\nlet d1 := 3.14;\nif (c1 == d1) {\n} else {\n}\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
             "let c1 := 3.14;\nlet d1 := 3.14;\nif (c1 == d1) {\n} else {\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let e1 := \"Hello\";\nlet f1 := \"Hello\";\nif (e1 == f1) {\n} else {\n}\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let e1 := \"Hello\";\nlet f1 := \"Hello\";\nif (e1 == f1) {\n} else {\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let g1 := true;\nlet h1 := true;\nif (g1 == h1) {\n} else {\n}\n"
          in
          let actual = Parser.program_rule Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected =
            "let g1 := true;\nlet h1 := true;\nif (g1 == h1) {\n} else {\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a2 := 5;\nlet b2 := 10;\nif (a2 == b2) {\n} else {\n}\n"
          in
          let actual = Parser.program_rule Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected =
              "let a2 := 5;\nlet b2 := 10;\nif (a2 == b2) {\n} else {\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let c2 := 3.14;\nlet d2 := 2.71;\nif (c2 == d2) {\n} else {\n}\n"
          in
          let actual = Parser.program_rule Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected =
            "let c2 := 3.14;\nlet d2 := 2.71;\nif (c2 == d2) {\n} else {\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let e2 := \"Hello\";\nlet f2 := \"World\";\nif (e2 == f2) {\n} else {\n}\n"
          in
          let actual = Parser.program_rule Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected =
            "let e2 := \"Hello\";\nlet f2 := \"World\";\nif (e2 == f2) {\n} else {\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test8"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let g2 := true;\nlet h2 := false;\nif (g2 == h2) {\n} else {\n}\n"
          in
          let actual = Parser.program_rule Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected =
            "let g2 := true;\nlet h2 := false;\nif (g2 == h2) {\n} else {\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test9"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a3 := 5;\nlet b3 := \"5\";\nif (a3 == b3) {\n} else {\n}\n"
          in
          let actual = Parser.program_rule Scanner.tokenize lexbuf in print_endline (string_of_program program)
          let expected =
            "let a3 := 5;\nlet b3 := \"5\";\nif (a3 == b3) {\n} else {\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

