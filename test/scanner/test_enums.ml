open OUnit2
open Fly_lib
open Fly_lib.Utils

let rec to_list lexbuf =
  let tk = Scanner.tokenize lexbuf in
  match tk with
  | Fly_lib.Parser.EOF -> []
  | t -> t :: to_list lexbuf
;;

let tests =
  "testing_enums"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "enum Color {\n\tRed,\n\tGreen,\n\tBlue\n}\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "ENUM ID(Color) LBRACE ID(Red) COMMA ID(Green) COMMA ID(Blue) RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let a1 := Color::Red;\nlet b1 := Color::Green;\nlet c1 := Color::Blue;\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a1) WALRUS ID(Color) DCOLON ID(Red) SEMI LET ID(b1) WALRUS ID(Color) \
             DCOLON ID(Green) SEMI LET ID(c1) WALRUS ID(Color) DCOLON ID(Blue) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test3"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "if (a1 == Color::Red) {\n} else {\n}\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "IF LPAREN ID(a1) BEQ ID(Color) DCOLON ID(Red) RPAREN LBRACE RBRACE ELSE \
             LBRACE RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test4"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a2 := Color::Yellow;\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected = "LET ID(a2) WALRUS ID(Color) DCOLON ID(Yellow) SEMI" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test5"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "if (b1 == Color::Green) {\n} else {\n}\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "IF LPAREN ID(b1) BEQ ID(Color) DCOLON ID(Green) RPAREN LBRACE RBRACE ELSE \
             LBRACE RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "match (c1) {\n\
              \  Color::Red -> { \n\
              \  }\n\
              \  Color::Green -> { \n\
              \  }\n\
              \  Color::Blue -> { \n\
              \  }\n\
              \  _ -> {\n\
              \  }\n\
               }\n"
          in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "MATCH LPAREN ID(c1) RPAREN LBRACE ID(Color) DCOLON ID(Red) ARROW LBRACE \
             RBRACE ID(Color) DCOLON ID(Green) ARROW LBRACE RBRACE ID(Color) DCOLON \
             ID(Blue) ARROW LBRACE RBRACE UNDERSCORE ARROW LBRACE RBRACE RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test7"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a3 := 5;\na3 := Color::Red;\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a3) WALRUS LITERAL(5) SEMI ID(a3) WALRUS ID(Color) DCOLON ID(Red) \
             SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test8"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a4 := Color::Red + Color::Green;\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected =
            "LET ID(a4) WALRUS ID(Color) DCOLON ID(Red) PLUS ID(Color) DCOLON ID(Green) \
             SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test9"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let a5 := !Color::Red;\n" in
          let actual = string_of_tokens (to_list lexbuf) in
          let expected = "LET ID(a5) WALRUS NOT ID(Color) DCOLON ID(Red) SEMI" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
