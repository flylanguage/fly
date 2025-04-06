open OUnit2
open Fly_lib
open Print_lib.Prints

let rec to_list lexbuf = 
    let tk = Scanner.tokenize lexbuf in
    match tk with
    | Fly_lib.Parser.EOF -> []
    | t -> t :: to_list lexbuf

let tests = "testing_list" >::: [
    
        "test1" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut a1 := [1, 2, 3];\na1[0] = 4;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test2" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let a2 := [1, 2, 3];\nlet a3 := 0 :: a2;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test3" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let a4 := [1, 2, 3];\nlet a5 := a3 :: [4, 5];\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test4" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let a6 := [1, 2, 3];\na6[1] := 5;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test5" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut a7 := [1, 2, 3];\nlet b7 := a7[2];\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test6" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut a8 := [1, 2, 3];\nlet b8 := a8 == [1, 2, 3];\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test7" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut a9 := [1, 2, 3];\nlet b9 := \"string\" :: a9;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test8" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut a10 := [true, false];\nlet b10 := !a10[0];\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test9" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut a11 := [1, 2, 3];\nlet b11 := a11[0] == 1;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

]

let _ = run_test_tt_main tests

