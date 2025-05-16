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
  >::: [ ("normal_function"
          >:: fun _ ->
          let actual = check_program "fun main() -> int { return 0; }" in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
        ; ("recursive_function"
          >:: fun _ ->
          let actual = check_program "fun function(n: int) -> int { if (n == 0) { return 1; } return function(n - 1); } fun main() -> int { return function(5); }" in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
        
       ; ("call_nested_function"
          >:: fun _ ->
          let actual = check_program "fun function() -> int { fun nested() -> () { return; } return 0; } fun main() -> int { return nested(); }" in
          let expected = "Undefined function nested" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))

      ; ("more_recursion"
          >:: fun _ ->
          let actual = check_program "fun outer(m: int) -> int { fun nested(n: int) -> int { if (n == 0) {return 1;} return 1 + nested(n - 1); } return nested(m); } fun main() -> int { return outer(5); }" in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))]
;;

let _ = run_test_tt_main tests