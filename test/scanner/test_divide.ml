open OUnit2
open Fly_lib
open Print_lib.Prints

let rec to_list lexbuf = 
  let tk = Scanner.tokenize lexbuf in
  match tk with
  | Fly_lib.Parser.EOF -> []
  | t -> t :: to_list lexbuf

let tests = "testing_divide" >::: [
  "test1" >:: (fun _ ->
    let lexbuf = Lexing.from_string "a1 / b1;" in
    let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
    let expected = "ID(a1) DIVIDE ID(b1) SEMI" in
    assert_equal
    expected
    actual
    ~printer:(fun s -> "\"" ^ s ^ "\""));
  "test2" >:: (fun _ ->
    let lexbuf = Lexing.from_string "10 / 2;" in
    let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
    let expected = "INT(10) DIVIDE INT(2) SEMI" in
    assert_equal
    expected
    actual
    ~printer:(fun s -> "\"" ^ s ^ "\""));
  "test3" >:: (fun _ ->
    let lexbuf = Lexing.from_string "let a1 := 10;\na1 / 2;" in
    let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
    let expected = "LET ID(a1) WALRUS INT(10) SEMI ID(a1) DIVIDE INT(2) SEMI" in
    assert_equal
    expected
    actual
    ~printer:(fun s -> "\"" ^ s ^ "\""));
  "test4" >:: (fun _ ->
    let lexbuf = Lexing.from_string "let a1 := 10;\n20/ a1;" in
    let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
    let expected = "LET ID(a1) WALRUS INT(10) SEMI INT(20) DIVIDE ID(a1) SEMI" in
    assert_equal
    expected
    actual
    ~printer:(fun s -> "\"" ^ s ^ "\""));
]

let _ = run_test_tt_main tests