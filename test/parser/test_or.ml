open OUnit2
open Fly_lib
open Fly_lib.Prints


let tests =
  "testing_or"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a1 := true;\nlet b1 := false;\nlet result1 := a1 | b1;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a1 := true;\nlet b1 := false;\nlet result1 := a1 | b1;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a2 := false;\nlet b2 := false;\nlet result2 := a2 | b2;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a2 := false;\nlet b2 := false;\nlet result2 := a2 | b2;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a3 := true;\nif (a3 | false) {\n\tlet result3 := \"executed\";\n\n}\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a3 := true;\nif (a3 | false) {\nlet result3 := \"executed\";\n\n}"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let b3 := false;\nif (b3 | false) {\n\tlet result4 := \"executed\";\n\n}\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let b3 := false;\nif (b3 | false) {\nlet result4 := \"executed\";\n\n}"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a4 := true;\nlet b4 := false;\nlet result5 := (a4 | b4) | false;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a4 := true;\nlet b4 := false;\nlet result5 := a4 | b4 | false;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test6"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a5 := 5;\nlet result6 := a5 | true;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a5 := 5;\nlet result6 := a5 | true;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let b5 := false;\n\
               while (b5 | true) {\n\
               \tlet result7 := \"looping\";\n\
               \tbreak;\n\
               }\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let b5 := false;\n\
               while (b5 | true) {\n\
               let result7 := \"looping\";\n\
               break;\n\
               }"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test8"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let b6 := true;\nlet result9 := b6 | b6;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let b6 := true;\nlet result9 := b6 | b6;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test9"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a7 := false;\nlet b7 := true;\nlet result10 := (a7 | b7) | false;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "let a7 := false;\nlet b7 := true;\nlet result10 := a7 | b7 | false;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

