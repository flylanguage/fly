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
  "testing_loops"
  >::: [ ("correct_list"
          >:: fun _ -> 
          let actual = check_program "while { break; }" in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))      
       ]
;;

let _ = run_test_tt_main tests
