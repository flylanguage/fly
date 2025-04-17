open OUnit2
open Fly_lib
open Print_lib.Prints

let tests =
  "testing_enums"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "enum Color {\n\tRed,\n\tGreen,\n\tBlue\n}\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "enum Color {\nRed,\nGreen,\nBlue\n}"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a1 := Color.Red;\nlet b1 := Color.Green;\nlet c1 := Color.Blue;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "let a1 := Color.Red;\nlet b1 := Color.Green;\nlet c1 := Color.Blue;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "if (a1 == Color.Red) {\n} else {\n}\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "if (a1 == Color.Red) {\n\n} else {\n\n}"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a2 := Color.Yellow;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = 
               "let a2 := Color.Yellow;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "if (b1 == Color.Green) {\n} else {\n}\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "if (b1 == Color.Green) {\n\n} else {\n\n}"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "match (c1) {\nColor.Red -> {\n
              \  }\n\
              \  Color.Green -> { \n\
              \  }\n\
              \  Color.Blue -> { \n\
              \  }\n\
              \  _ -> {\n\
              \  }\n\
               }\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "MATCH LPAREN ID(c1) RPAREN LBRACE ID(Color) DOT ID(Red) ARROW LBRACE RBRACE \
             ID(Color) DOT ID(Green) ARROW LBRACE RBRACE ID(Color) DOT ID(Blue) ARROW \
             LBRACE RBRACE UNDERSCORE ARROW LBRACE RBRACE RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test7"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a3 := 5;\na3 := Color.Red;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "let a3 := 5;\na3 := Color.Red;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test8"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a4 := Color.Red + Color.Green;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "let a4 := Color.Red + Color.Green;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test9"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a5 := !Color.Red;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = 
            "let a5 := !Color.Red;\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ]
;;

let _ = run_test_tt_main tests

