open OUnit2
open Fly_lib

let check_program source_code =
  let lexbuf = Lexing.from_string source_code in
  let ast = Parser.program_rule Scanner.tokenize lexbuf in
  try
    let _ = Semant.check ast.body in
    ""
  with
  | Failure msg -> msg
;;

let tests =
  "testing_and"
  >::: [ ("bool_and_bool"
          >:: fun _ ->
          let actual =
            check_program "fun main() {let a := true;\nlet b := false;\n a && b;}"
          in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("bool_and_int"
          >:: fun _ ->
          let actual =
            check_program "fun main() {let a := true;\nlet b := 5;\n a && b;}"
          in
          let expected = "Expression: 'a && b' LHS: bool, RHS: int" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("bool_and_float"
          >:: fun _ ->
          let actual =
            check_program "fun main() {let a := true;\nlet b := 5.0;\n a && b;}"
          in
          let expected = "Expression: 'a && b' LHS: bool, RHS: float" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("bool_and_string"
          >:: fun _ ->
          let actual =
            check_program "fun main() {let a := true;\nlet b := \"nonsense\";\n a && b;}"
          in
          let expected = "Expression: 'a && b' LHS: bool, RHS: string" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("bool_and_char"
          >:: fun _ ->
          let actual =
            check_program "fun main() {let a := true;\nlet b := \'a\';\n a && b;}"
          in
          let expected = "Expression: 'a && b' LHS: bool, RHS: char" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("bool_and_char"
          >:: fun _ ->
          let actual =
            check_program
              "type Person {name: string, age: int} fun main() {let a := true;\n\
               let b := Person {name: \"John\", age: 25};\n\
              \ a && b;}"
          in
          let expected = "Expression: 'a && b' LHS: bool, RHS: Person" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
