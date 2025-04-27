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
  "testing_func_bind"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "type Person {\n\tname: string,\n\tage: int\n}\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "TYPE ID(Person) LBRACE ID(name) COLON STRING COMMA ID(age) COLON INT RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let p1: Person = Person {name: \"John\", age: 12};\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(p1) COLON ID(Person) EQUAL ID(Person) LBRACE ID(name) COLON \
             SLIT(John) COMMA ID(age) COLON LITERAL(12) RBRACE SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "bind new<Person>(name: string, age: int) -> Person {\n\
               \treturn Person {name: name, age: age};\n\
               }\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "BIND ID(new) LT ID(Person) GT LPAREN ID(name) COLON STRING COMMA ID(age) \
             COLON INT RPAREN ARROW ID(Person) LBRACE RETURN ID(Person) LBRACE ID(name) \
             COLON ID(name) COMMA ID(age) COLON ID(age) RBRACE SEMI RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "bind info<Person>(self) -> string {\n\
               \treturn self.name + \" \" + self.age;\n\
               }\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "BIND ID(info) LT ID(Person) GT LPAREN SELF RPAREN ARROW STRING LBRACE \
             RETURN SELF DOT ID(name) PLUS SLIT( ) PLUS SELF DOT ID(age) SEMI RBRACE"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let p2 := Person::new(\"John\", 12);\n\
               let info: string = p.info(); // self references the object itself\n"
          in
          let actual = List.map string_of_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(p2) WALRUS ID(Person) DCOLON ID(new) LPAREN SLIT(John) COMMA \
             LITERAL(12) RPAREN SEMI LET ID(info) COLON STRING EQUAL ID(p) DOT ID(info) \
             LPAREN RPAREN SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
