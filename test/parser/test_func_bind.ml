open OUnit2
open Fly_lib
open Print_lib.Prints

let tests =
  "testing_func_bind"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "type Person {\n\tname: string,\n\tage: int\n}\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected =
            "type Person{\nname: string, age: int, \n}"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test2"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let p1: Person = Person {name: \"John\", age: 12};\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "let p1: Person = Person{name: \"John\", age: 12, };\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test3"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "bind new<Person>(name: string, age: int) -> Person {\n\
               \treturn Person{name: name, age: age};\n\
               }\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "bind new<Person>(name: string, age: int, ) -> Person {\n\
               return Person{name: name, age: age, };\n\
               \n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test4"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "bind info<Person>(self) -> string {\n\
               \treturn self.name + self.age;\n\
               }\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "bind info<Person>(self: Person, ) -> string {\n\
               return self.name + self.age;\n\
               \n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

       ; ("test5"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "let p2 := Person::new(\"John\", 12);\np.info();\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "let p2 := Person::new(\"John\", 12);\np.info();\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
        ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "bind increase_age<Person>(self) {\n\
               \tself.age += 1;\n\
               }\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "bind increase_age<Person>(self: Person, ) -> () {\n\
                          self.age += 1;\n\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
        ; ("test7"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string
              "bind change_age<Person>(self, new_age: int) {\n\
               \tself.age = new_age;\n\
               }\n"
          in
          let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
          let expected = "bind change_age<Person>(self: Person, new_age: int, ) -> () {\n\
                          self.age = new_age;\n\n}\n"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests

