open OUnit2
open Fly_lib
open Fly_lib.Prints

let tests =
  "testing_plusassign"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut x1 := 5;\nx1 += 3;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut x1 := 5;\nx1 += 3;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut x2 := 5.5;\nx2 += 3.5;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut x2 := 5.5;\nx2 += 3.5;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut x3 := 5;\nx3 += 3.5;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut x3 := 5;\nx3 += 3.5;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let x4 := 5.5;\nx4 += 3;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let x4 := 5.5;\nx4 += 3;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut x6 := true;\nx6 += false;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut x6 := true;\nx6 += false;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let mut x7 := \"Hello\";\nx7 += \" World\";\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut x7 := \"Hello\";\nx7 += \" World\";\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test7"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut x8 := [1, 2, 3];\nx8 += [4, 5];\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut x8 := [1, 2, 3];\nx8 += [4, 5];\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test9"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut x10 := 'a';\nx10 += 'b';\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut x10 := 'a';\nx10 += 'b';\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test10"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut x11 := (1, 2);\nx11 += (3, 4);\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let mut x11 := (1, 2);\nx11 += (3, 4);\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

