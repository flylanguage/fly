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
  "testing_divide"
  >::: [ ("int_div_int"
          >:: fun _ ->
          let actual =
            check_program "fun main() -> int {let a := 5;\nlet b := 1;\nreturn a/b;}"
          in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("float_div_float"
          >:: fun _ ->
          let actual =
            check_program "fun main() -> float {let a := 5.0;\nlet b := 1.0;\nreturn a/b;}"
          in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("int_div_float"
          >:: fun _ ->
          let actual =
            check_program "fun main() -> int {let a := 5;\nlet b := 1.0;\na/b;}"
          in
          let expected = "Expression: 'a / b' LHS: int, RHS: float" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("float_div_int"
          >:: fun _ ->
          let actual =
            check_program "fun main() -> float {let a := 5.0;\nlet b := 1;\na/b;}"
          in
          let expected = "Expression: 'a / b' LHS: float, RHS: int" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("bool_div_int"
          >:: fun _ ->
          let actual =
            check_program "fun main() -> int {let a := true;\nlet b := 5;\na/b;}"
          in
          let expected = "Expression: 'a / b' LHS: bool, RHS: int" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("char_div_int"
          >:: fun _ ->
          let actual = check_program "fun main() {let a := \'a\';\nlet b := 5;\n a/b;}" in
          let expected = "Expression: 'a / b' LHS: char, RHS: int" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("string_div_int"
          >:: fun _ ->
          let actual =
            check_program "fun main() {let a := \"nonsense\";\nlet b := 5;\n a/b;}"
          in
          let expected = "Expression: 'a / b' LHS: string, RHS: int" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("udt_div_int"
          >:: fun _ ->
          let actual =
            check_program
              "type Person {name: string, age: int} fun main() {let a := Person {name: \
               \"John\", age: 25};\n\
               let b := 5;\n\
              \ a/b;}"
          in
          let expected = "Expression: 'a / b' LHS: Person, RHS: int" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
