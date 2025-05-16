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
  "testing_return"
  >::: [ ("correct_return"
          >:: fun _ ->
          let actual = check_program "fun main() -> int { return 0; }" in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("missing_return"
          >:: fun _ ->
          let actual = check_program "fun main() -> int { let x := 5; }" in
          let expected = "Missing return statement in main" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("incorrect_return"
          >:: fun _ ->
          let actual = check_program "fun main() -> int { return \"c\"; }" in
          let expected = "Expression '\"c\"' has type string but expected int" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("append_missing_return_unit"
          >:: fun _ ->
          let actual = check_program "fun main() -> () {}" in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
      
      ; ("if_else_ok"
          >:: fun _ ->
          let actual = check_program "fun main() -> int {let x := 5; if (x > 2) { return 0; } else { return 1; } }" in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
      
      ; ("if_elif_notok"
          >:: fun _ ->
          let actual = check_program "fun main() -> int {let x := 5; if (x > 2) { return 0; } else if (x > 1) { return 1; } }" in
          let expected = "Missing return statement in main" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
      
      ; ("if_elif_else_ok"
          >:: fun _ ->
          let actual = check_program "fun main() -> int {let x := 5; if (x > 2) { return 0; } else if (x > 1) { return 1; } else { return 2; } }" in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
