open OUnit2
open Fly_lib
open Print_lib.Prints


let tests =
  "testing_not"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a1 := true;\nlet result1 := !a1;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a1 := true;\nlet result1 := !a1;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let b1 := false;\nlet result2 := !b1;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let b1 := false;\nlet result2 := !b1;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a2 := true;\nif (!a2) {\n\tlet result3 := \"should not execute\";\n\n}\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a2 := true;\nif (!a2) {\nlet result3 := \"should not execute\";\n\n}"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let b2 := false;\nif (!b2) {\n\tlet result4 := \"executed\";\n}\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let b2 := false;\nif (!b2) {\nlet result4 := \"executed\";\n\n}"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a3 := true;\nlet b3 := false;\nlet result5 := !(a3 && b3);\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a3 := true;\nlet b3 := false;\nlet result5 := !a3 && b3;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test6"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a4 := 5;\nlet result6 := !a4;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a4 := 5;\nlet result6 := !a4;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let b4 := false;\n\
               while (!b4) {\n\
               \tlet result7 := \"looping\";\n\
               \tbreak;\n\
               }\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let b4 := false;\n\
               while (!b4) {\n\
               let result7 := \"looping\";\n\
               break;\n\
               }\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test8"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let b5 := true;\nlet result8 := !!b5;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let b5 := true;\nlet result8 := !!b5;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test9"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a6 := true;\nlet b6 := false;\nlet result10 := !(a6 != b6);\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a6 := true;\nlet b6 := false;\nlet result10 := !a6 != b6;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

