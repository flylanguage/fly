open OUnit2
open Fly_lib
open Print_lib.Prints

let rec to_list lexbuf = 
    let tk = Scanner.tokenize lexbuf in
    match tk with
    | Fly_lib.Parser.EOF -> []
    | t -> t :: to_list lexbuf

let tests = "testing_or" >::: [
    
        "test1" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let a1 := true;\nlet b1 := false;\nlet result1 := a1 | b1;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test2" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let a2 := false;\nlet b2 := false;\nlet result2 := a2 | b2;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test3" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let a3 := true;\nif (a3 | false) {\n	let result3 := \"executed\";\n}\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test4" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let b3 := false;\nif (b3 | false) {\n	let result4 := \"executed\";\n}\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test5" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let a4 := true;\nlet b4 := false;\nlet result5 := (a4 | b4) | false;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test6" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let a5 := 5;\nlet result6 := a5 | true;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test7" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let b5 := false;\nwhile (b5 | true) {\n	let result7 := \"looping\";\n	break;\n}\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test8" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let b6 := true;\nlet result9 := b6 | b6;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test9" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let a7 := false;\nlet b7 := true;\nlet result10 := (a7 | b7) | false;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

]

let _ = run_test_tt_main tests

