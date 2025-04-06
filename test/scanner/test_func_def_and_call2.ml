open OUnit2
open Fly_lib
open Print_lib.Prints

let rec to_list lexbuf = 
    let tk = Scanner.tokenize lexbuf in
    match tk with
    | Fly_lib.Parser.EOF -> []
    | t -> t :: to_list lexbuf

let tests = "testing_func_def_and_call2" >::: [
    
        "test1" >:: (fun _ ->
        let lexbuf = Lexing.from_string "" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test2" >:: (fun _ ->
        let lexbuf = Lexing.from_string "fun map(f: fun a -> b) -> fun list<a> -> list<b> {\n	return match l {\n		[] -> [],\n		head :: tail -> map(f)(tail),\n	}\n}\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test3" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let lst: list<int> = [0,1,2,3,4]; //map creates a new list so we don't need mut here.\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test4" >:: (fun _ ->
        let lexbuf = Lexing.from_string "fun typecast(x: int) -> float {\n	return float(x);\n}\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test5" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let float_lst := map(lst)(typecast);\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

]

let _ = run_test_tt_main tests

