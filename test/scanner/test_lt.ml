open OUnit2
open Fly_lib
open Print_lib.Prints

let rec to_list lexbuf =
  let tk = Scanner.tokenize lexbuf in
  match tk with
  | Fly_lib.Parser.EOF -> []
  | t -> t :: to_list lexbuf
;;

let tests =
  "testing_lt"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a1 := 5;\nlet b1 := 10;\nlet result1 := a1 < b1;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a1) WALRUS LITERAL(5) SEMI LET ID(b1) WALRUS LITERAL(10) SEMI LET \
             ID(result1) WALRUS ID(a1) LT ID(b1) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a2 := 3.5;\nlet b2 := 4.2;\nlet result2 := a2 < b2;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a2) WALRUS FLIT(3.500000) SEMI LET ID(b2) WALRUS FLIT(4.200000) SEMI \
             LET ID(result2) WALRUS ID(a2) LT ID(b2) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a3 := 5;\nlet b3 := 5;\nlet result3 := a3 < b3;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a3) WALRUS LITERAL(5) SEMI LET ID(b3) WALRUS LITERAL(5) SEMI LET \
             ID(result3) WALRUS ID(a3) LT ID(b3) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a4 := 5.5;\nlet b4 := 5.5;\nlet result4 := a4 < b4;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a4) WALRUS FLIT(5.500000) SEMI LET ID(b4) WALRUS FLIT(5.500000) SEMI \
             LET ID(result4) WALRUS ID(a4) LT ID(b4) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a5 := 10;\nlet b5 := 5;\nlet result5 := a5 < b5;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a5) WALRUS LITERAL(10) SEMI LET ID(b5) WALRUS LITERAL(5) SEMI LET \
             ID(result5) WALRUS ID(a5) LT ID(b5) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a6 := \"hello\";\nlet b6 := \"world\";\nlet result6 := a6 < b6;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a6) WALRUS SLIT(hello) SEMI LET ID(b6) WALRUS SLIT(world) SEMI LET \
             ID(result6) WALRUS ID(a6) LT ID(b6) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a7 := true;\nlet b7 := false;\nlet result7 := a7 < b7;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a7) WALRUS BLIT(true) SEMI LET ID(b7) WALRUS BLIT(false) SEMI LET \
             ID(result7) WALRUS ID(a7) LT ID(b7) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test8"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a8 := (1, 2);\nlet b8 := (2, 3);\nlet result8 := a8 < b8;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a8) WALRUS LPAREN LITERAL(1) COMMA LITERAL(2) RPAREN SEMI LET ID(b8) \
             WALRUS LPAREN LITERAL(2) COMMA LITERAL(3) RPAREN SEMI LET ID(result8) \
             WALRUS ID(a8) LT ID(b8) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test9"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a9 := [1, 2, 3];\nlet b9 := [4, 5, 6];\nlet result9 := a9 < b9;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a9) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACKET SEMI LET ID(b9) WALRUS LBRACKET LITERAL(4) COMMA LITERAL(5) COMMA \
             LITERAL(6) RBRACKET SEMI LET ID(result9) WALRUS ID(a9) LT ID(b9) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test10"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a10 := {1, 2, 3};\nlet b10 := {4, 5, 6};\nlet result10 := a10 < b10;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a10) WALRUS LBRACE LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACE SEMI LET ID(b10) WALRUS LBRACE LITERAL(4) COMMA LITERAL(5) COMMA \
             LITERAL(6) RBRACE SEMI LET ID(result10) WALRUS ID(a10) LT ID(b10) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test11"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a11 := 5;\nlet b11 := \"string\";\nlet result11 := a11 < b11;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a11) WALRUS LITERAL(5) SEMI LET ID(b11) WALRUS SLIT(string) SEMI LET \
             ID(result11) WALRUS ID(a11) LT ID(b11) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test12"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a12 := \"hello\";\nlet b12 := 5;\nlet result12 := a12 < b12;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a12) WALRUS SLIT(hello) SEMI LET ID(b12) WALRUS LITERAL(5) SEMI LET \
             ID(result12) WALRUS ID(a12) LT ID(b12) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test13"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a13 := (1, 2);\nlet b13 := 5;\nlet result13 := a13 < b13;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a13) WALRUS LPAREN LITERAL(1) COMMA LITERAL(2) RPAREN SEMI LET \
             ID(b13) WALRUS LITERAL(5) SEMI LET ID(result13) WALRUS ID(a13) LT ID(b13) \
             SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test14"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a14 := [1, 2, 3];\nlet b14 := 5;\nlet result14 := a14 < b14;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a14) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACKET SEMI LET ID(b14) WALRUS LITERAL(5) SEMI LET ID(result14) WALRUS \
             ID(a14) LT ID(b14) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test15"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a15 := {1, 2, 3};\nlet b15 := 5;\nlet result15 := a15 < b15;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a15) WALRUS LBRACE LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACE SEMI LET ID(b15) WALRUS LITERAL(5) SEMI LET ID(result15) WALRUS \
             ID(a15) LT ID(b15) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test16"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a16 := true;\nlet b16 := 5;\nlet result16 := a16 < b16;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a16) WALRUS BLIT(true) SEMI LET ID(b16) WALRUS LITERAL(5) SEMI LET \
             ID(result16) WALRUS ID(a16) LT ID(b16) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test17"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a17 := false;\nlet b17 := \"world\";\nlet result17 := a17 < b17;\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a17) WALRUS BLIT(false) SEMI LET ID(b17) WALRUS SLIT(world) SEMI LET \
             ID(result17) WALRUS ID(a17) LT ID(b17) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
