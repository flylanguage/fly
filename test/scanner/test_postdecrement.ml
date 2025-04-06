open OUnit2
open Fly_lib
open Print_lib.Prints

let rec to_list lexbuf = 
    let tk = Scanner.tokenize lexbuf in
    match tk with
    | Fly_lib.Parser.EOF -> []
    | t -> t :: to_list lexbuf

let tests = "testing_postdecrement" >::: [
    
        "test1" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut x1 := 5;\nx1--;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test2" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut x2 := 5.5;\nx2--;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test3" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut x3 := true;\nx3--;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test4" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut x4 := \"Hello\";\nx4--;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test5" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut x5 := [1, 2, 3];\nx5--;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test6" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut x6 := {1, 2, 3};\nx6--;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test7" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut x7 := 'a';\nx7--;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test8" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut x8 := (1, 2);\nx8--;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test9" >:: (fun _ ->
        let lexbuf = Lexing.from_string "/*\nProbably also want a test case to check if variable to postdecrement has been declared with mut\n*/\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test10" >:: (fun _ ->
        let lexbuf = Lexing.from_string "" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

]

let _ = run_test_tt_main tests

