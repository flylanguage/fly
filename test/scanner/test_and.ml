open OUnit2
open Fly_lib
open Print_lib.Prints

let rec to_list lexbuf = 
  let tk = Scanner.tokenize lexbuf in
  match tk with
  | Fly_lib.Parser.EOF -> []
  | t -> t :: to_list lexbuf

let tests = "testing_and" >::: [
  "test1" >:: (fun _ ->
    let lexbuf = Lexing.from_string "a1 && b1;" in
    let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
    let expected = "ID(a1) AND ID(b1) SEMI" in
    assert_equal 
    expected
    actual
    ~printer:(fun s -> "\"" ^ s ^ "\""));
]

let _ = run_test_tt_main tests