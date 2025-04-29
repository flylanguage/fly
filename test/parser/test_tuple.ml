open OUnit2
open Fly_lib
open Fly_lib.Utils

let tests =
  "testing_tuple"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "fun nothing() -> () {\nlet x1 := (1, 2, 3);\nreturn x1;\n}"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "fun nothing() -> () {\nlet x1 := (1, 2, 3);\nreturn x1;\n\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "fun sometuple() -> tuple<int, float, string> {let x2 := (1, 2.5, \
               \"hello\");\n\
               return x2;\n\
               }"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "fun sometuple() -> tuple<int, float, string> {\n\
             let x2 := (1, 2.5, \"hello\");\n\
             return x2;\n\n\
             }\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "fun bool_tup() -> tuple<bool, bool> {\n\
               let x3 := (true, false);\n\
               return x3;\n\
               }"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "fun bool_tup() -> tuple<bool, bool> {\n\
             let x3 := (true, false);\n\
             return x3;\n\n\
             }\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "fun idx_tuple() {\n\
               let x4 := (1, 2, 3);\n\
               let y:= x4[0];\n\
               let z := x4[1];\n\
               return x4[2];\n\n\
               }\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "fun idx_tuple() -> () {\n\
             let x4 := (1, 2, 3);\n\
             let y := x4[0];\n\
             let z := x4[1];\n\
             return x4[2];\n\n\
             }\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "fun tup_arg(x: tuple<int, int, int>) {\n\
               let x5 := (1, 2, 3);\n\
               x5[1] = 4;\n\n\
               }\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "fun tup_arg(x: tuple<int, int, int>, ) -> () {\n\
             let x5 := (1, 2, 3);\n\
             x5[1] = 4;\n\n\
             }\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let x6 := (1, 2, 3);\n\
               let x7 := (4, 5, 6);\n\
               fun illegal_add_tuples(left: tuple<int, int, int>, right: \
               tuple<int,int,int>) {return left + right;\n\
               }"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected =
            "let x6 := (1, 2, 3);\n\
             let x7 := (4, 5, 6);\n\
             fun illegal_add_tuples(left: tuple<int, int, int>, right: tuple<int, int, \
             int>, ) -> () {\n\
             return left + right;\n\n\
             }\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let x8 := (\"hello\", \"world\");\nreturn x8;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "let x8 := (\"hello\", \"world\");\nreturn x8;\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test9"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let x10 := (1, 2);\nlet x11 := 3;\nreturn x10 + x11;\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "let x10 := (1, 2);\nlet x11 := 3;\nreturn x10 + x11;\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test10"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let x12 := (1, [2, 3], 4);\nreturn x12;\n" in
          let program = Parser.program_rule Scanner.tokenize lexbuf in
          let actual = string_of_program program in
          let expected = "let x12 := (1, [2, 3], 4);\nreturn x12;\n" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
