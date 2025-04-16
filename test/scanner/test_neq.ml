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
  "testing_neq"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a := 5;\nlet b := 3;\nlet result := a != b;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a) WALRUS LITERAL(5) SEMI LET ID(b) WALRUS LITERAL(3) SEMI LET \
             ID(result) WALRUS ID(a) NEQ ID(b) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a1 := 5.0;\nlet b1 := 3.0;\nlet result1 := a1 != b1;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a1) WALRUS FLIT(5.000000) SEMI LET ID(b1) WALRUS FLIT(3.000000) SEMI \
             LET ID(result1) WALRUS ID(a1) NEQ ID(b1) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a2 := 5;\nlet b2 := 5.0;\nlet result2 := a2 != b2;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a2) WALRUS LITERAL(5) SEMI LET ID(b2) WALRUS FLIT(5.000000) SEMI LET \
             ID(result2) WALRUS ID(a2) NEQ ID(b2) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a3 := true;\nlet b3 := false;\nlet result3 := a3 != b3;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a3) WALRUS BLIT(true) SEMI LET ID(b3) WALRUS BLIT(false) SEMI LET \
             ID(result3) WALRUS ID(a3) NEQ ID(b3) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a4 := \"hello\";\nlet b4 := \"world\";\nlet result4 := a4 != b4;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a4) WALRUS SLIT(hello) SEMI LET ID(b4) WALRUS SLIT(world) SEMI LET \
             ID(result4) WALRUS ID(a4) NEQ ID(b4) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a5 := 'a';\nlet b5 := 'b';\nlet result5 := a5 != b5;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a5) WALRUS CLIT(a) SEMI LET ID(b5) WALRUS CLIT(b) SEMI LET \
             ID(result5) WALRUS ID(a5) NEQ ID(b5) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a6 := 10;\nlet b6 := 10;\nlet result6 := a6 != b6;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a6) WALRUS LITERAL(10) SEMI LET ID(b6) WALRUS LITERAL(10) SEMI LET \
             ID(result6) WALRUS ID(a6) NEQ ID(b6) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test8"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a7 := 5;\nlet b7 := \"hello\";\nlet result7 := a7 != b7;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a7) WALRUS LITERAL(5) SEMI LET ID(b7) WALRUS SLIT(hello) SEMI LET \
             ID(result7) WALRUS ID(a7) NEQ ID(b7) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test9"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a8 := true;\nlet b8 := true;\nlet result8 := a8 != b8;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a8) WALRUS BLIT(true) SEMI LET ID(b8) WALRUS BLIT(true) SEMI LET \
             ID(result8) WALRUS ID(a8) NEQ ID(b8) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test10"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a9 := \"\";\nlet b9 := \"\";\nlet result9 := a9 != b9;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a9) WALRUS SLIT() SEMI LET ID(b9) WALRUS SLIT() SEMI LET ID(result9) \
             WALRUS ID(a9) NEQ ID(b9) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test11"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a10 := [1, 2, 3];\n\
               let b10 := [1, 2, 3];\n\
               let result10 := a10 != b10;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a10) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACKET SEMI LET ID(b10) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA \
             LITERAL(3) RBRACKET SEMI LET ID(result10) WALRUS ID(a10) NEQ ID(b10) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test12"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a11 := {1, 2, 3};\n\
               let b11 := {3, 2, 1};\n\
               let result11 := a11 != b11;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a11) WALRUS LBRACE LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACE SEMI LET ID(b11) WALRUS LBRACE LITERAL(3) COMMA LITERAL(2) COMMA \
             LITERAL(1) RBRACE SEMI LET ID(result11) WALRUS ID(a11) NEQ ID(b11) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
