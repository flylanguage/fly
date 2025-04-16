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
  "testing_ge"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a1 := 5;\nlet b1 := 3;\nlet result1 := a1 >= b1;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a1) WALRUS LITERAL(5) SEMI LET ID(b1) WALRUS LITERAL(3) SEMI LET \
             ID(result1) WALRUS ID(a1) GEQ ID(b1) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a2 := 5.5;\nlet b2 := 3.3;\nlet result2 := a2 >= b2;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a2) WALRUS FLIT(5.500000) SEMI LET ID(b2) WALRUS FLIT(3.300000) SEMI \
             LET ID(result2) WALRUS ID(a2) GEQ ID(b2) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a3 := 5;\nlet b3 := 3.7;\nlet result3 := a3 >= b3;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a3) WALRUS LITERAL(5) SEMI LET ID(b3) WALRUS FLIT(3.700000) SEMI LET \
             ID(result3) WALRUS ID(a3) GEQ ID(b3) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a4 := \"hello\";\nlet b4 := \"world\";\nlet result4 := a4 >= b4;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a4) WALRUS SLIT(hello) SEMI LET ID(b4) WALRUS SLIT(world) SEMI LET \
             ID(result4) WALRUS ID(a4) GEQ ID(b4) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a5 := true;\nlet b5 := false;\nlet result5 := a5 >= b5;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a5) WALRUS BLIT(true) SEMI LET ID(b5) WALRUS BLIT(false) SEMI LET \
             ID(result5) WALRUS ID(a5) GEQ ID(b5) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a6 := (1, 2);\nlet b6 := 3;\nlet result6 := a6 >= b6;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a6) WALRUS LPAREN LITERAL(1) COMMA LITERAL(2) RPAREN SEMI LET ID(b6) \
             WALRUS LITERAL(3) SEMI LET ID(result6) WALRUS ID(a6) GEQ ID(b6) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a7 := [1, 2, 3];\nlet b7 := 2;\nlet result7 := a7 >= b7;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a7) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACKET SEMI LET ID(b7) WALRUS LITERAL(2) SEMI LET ID(result7) WALRUS \
             ID(a7) GEQ ID(b7) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test8"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a8 := {1, 2, 3};\nlet b8 := 2;\nlet result8 := a8 >= b8;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a8) WALRUS LBRACE LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACE SEMI LET ID(b8) WALRUS LITERAL(2) SEMI LET ID(result8) WALRUS ID(a8) \
             GEQ ID(b8) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test9"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a9 := 4;\nlet b9 := 4;\nlet result9 := a9 >= b9;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a9) WALRUS LITERAL(4) SEMI LET ID(b9) WALRUS LITERAL(4) SEMI LET \
             ID(result9) WALRUS ID(a9) GEQ ID(b9) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test10"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a10 := 4.4;\nlet b10 := 4.4;\nlet result10 := a10 >= b10;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a10) WALRUS FLIT(4.400000) SEMI LET ID(b10) WALRUS FLIT(4.400000) \
             SEMI LET ID(result10) WALRUS ID(a10) GEQ ID(b10) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test11"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a11 := -2;\nlet b11 := -5;\nlet result11 := a11 >= b11;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a11) WALRUS LITERAL(-2) SEMI LET ID(b11) WALRUS LITERAL(-5) SEMI LET \
             ID(result11) WALRUS ID(a11) GEQ ID(b11) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test12"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a12 := -2.5;\nlet b12 := -3.3;\nlet result12 := a12 >= b12;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a12) WALRUS FLIT(-2.500000) SEMI LET ID(b12) WALRUS FLIT(-3.300000) \
             SEMI LET ID(result12) WALRUS ID(a12) GEQ ID(b12) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test13"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a13 := 2;\nlet b13 := -2;\nlet result13 := a13 >= b13;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a13) WALRUS LITERAL(2) SEMI LET ID(b13) WALRUS LITERAL(-2) SEMI LET \
             ID(result13) WALRUS ID(a13) GEQ ID(b13) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test14"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a14 := 3.5;\nlet b14 := -1.1;\nlet result14 := a14 >= b14;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a14) WALRUS FLIT(3.500000) SEMI LET ID(b14) WALRUS FLIT(-1.100000) \
             SEMI LET ID(result14) WALRUS ID(a14) GEQ ID(b14) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test15"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a15 := \"hello\";\nlet b15 := 5;\nlet result15 := a15 >= b15;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a15) WALRUS SLIT(hello) SEMI LET ID(b15) WALRUS LITERAL(5) SEMI LET \
             ID(result15) WALRUS ID(a15) GEQ ID(b15) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
