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
  "testing_and"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a1 := true;\nlet b1 := false;\na1 && b1;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a1) WALRUS BLIT(true) SEMI LET ID(b1) WALRUS BLIT(false) SEMI ID(a1) \
             AND ID(b1) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a2 := true;\nlet b2 := true;\na2 && b2;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a2) WALRUS BLIT(true) SEMI LET ID(b2) WALRUS BLIT(true) SEMI ID(a2) \
             AND ID(b2) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a3 := false;\nlet b3 := false;\na3 && b3;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a3) WALRUS BLIT(false) SEMI LET ID(b3) WALRUS BLIT(false) SEMI \
             ID(a3) AND ID(b3) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test4"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a4 := true;\nlet b4 := 5;\na4 && b4;\n" in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a4) WALRUS BLIT(true) SEMI LET ID(b4) WALRUS LITERAL(5) SEMI ID(a4) \
             AND ID(b4) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a5 := false;\nlet b5 := \"hello\";\na5 && b5;\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a5) WALRUS BLIT(false) SEMI LET ID(b5) WALRUS SLIT(hello) SEMI \
             ID(a5) AND ID(b5) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test6"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "true && false;\n" in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected = "BLIT(true) AND BLIT(false) SEMI" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a6 := true;\nlet b6 := false;\n!(a6 && b6);\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a6) WALRUS BLIT(true) SEMI LET ID(b6) WALRUS BLIT(false) SEMI NOT \
             LPAREN ID(a6) AND ID(b6) RPAREN SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test8"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let a7 := 5;\nlet b7 := 10;\n(a7 < b7) && (b7 > 5);"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a7) WALRUS LITERAL(5) SEMI LET ID(b7) WALRUS LITERAL(10) SEMI LPAREN \
             ID(a7) LT ID(b7) RPAREN AND LPAREN ID(b7) GT LITERAL(5) RPAREN SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
