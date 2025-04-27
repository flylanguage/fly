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
  "testing_not"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a1 := true;\nlet result1 := !a1;\n" in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a1) WALRUS BLIT(true) SEMI LET ID(result1) WALRUS NOT ID(a1) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let b1 := false;\nlet result2 := !b1;\n" in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(b1) WALRUS BLIT(false) SEMI LET ID(result2) WALRUS NOT ID(b1) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a2 := true;\nif (!a2) {\n\tlet result3 := \"should not execute\";\n}\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a2) WALRUS BLIT(true) SEMI IF LPAREN NOT ID(a2) RPAREN LBRACE LET \
             ID(result3) WALRUS SLIT(should not execute) SEMI RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let b2 := false;\nif (!b2) {\n\tlet result4 := \"executed\";\n}\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(b2) WALRUS BLIT(false) SEMI IF LPAREN NOT ID(b2) RPAREN LBRACE LET \
             ID(result4) WALRUS SLIT(executed) SEMI RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a3 := true;\nlet b3 := false;\nlet result5 := !(a3 && b3);\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a3) WALRUS BLIT(true) SEMI LET ID(b3) WALRUS BLIT(false) SEMI LET \
             ID(result5) WALRUS NOT LPAREN ID(a3) AND ID(b3) RPAREN SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test6"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a4 := 5;\nlet result6 := !a4;\n" in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a4) WALRUS LITERAL(5) SEMI LET ID(result6) WALRUS NOT ID(a4) SEMI"
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
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(b4) WALRUS BLIT(false) SEMI WHILE LPAREN NOT ID(b4) RPAREN LBRACE \
             LET ID(result7) WALRUS SLIT(looping) SEMI BREAK SEMI RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test8"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let b5 := true;\nlet result8 := !!b5;\n" in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(b5) WALRUS BLIT(true) SEMI LET ID(result8) WALRUS NOT NOT ID(b5) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test9"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a6 := true;\nlet b6 := false;\nlet result10 := !(a6 != b6);\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(a6) WALRUS BLIT(true) SEMI LET ID(b6) WALRUS BLIT(false) SEMI LET \
             ID(result10) WALRUS NOT LPAREN ID(a6) NEQ ID(b6) RPAREN SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
