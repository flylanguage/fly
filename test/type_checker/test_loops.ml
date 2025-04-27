open OUnit2
open Fly_lib

let check_program source_code =
  let lexbuf = Lexing.from_string source_code in
  let ast = Parser.program_rule Scanner.tokenize lexbuf in
  try
    let _ = Semant.check ast.body in
    ""
  with Failure msg -> msg

let tests =
  "testing_loops"
  >::: [
    ("correct_list" >:: fun _ ->
      let actual = check_program "let x := [1,2,3,4,5]; for i := x {let a := i;}" in
      let expected = "" in
      assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""));
    
    ("correct_tuple" >:: fun _ ->
      let actual = check_program "let x := (1,2,3,4,5); for i := x {let a := i;}" in
      let expected = "" in
      assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""));
    
    ("loop_var_shadowing" >:: fun _ ->
      let actual = check_program "let i := 0; let x := [1,2,3,4,5]; for i := x {}" in
      let expected =  "Loop variable i previously defined" in
      assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""));
    
    ("non_iterable" >:: fun _ ->
      let actual = check_program "let x := 5; for i := x {let a := i;}" in
      let expected = "Expression 'x' has type int and is not iterable" in
      assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""));
  ]
;;

let _ = run_test_tt_main tests