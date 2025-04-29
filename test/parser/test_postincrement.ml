open OUnit2
open Fly_lib
open Fly_lib.Prints

let tests = "testing_postincrement" >::: [
    
        "test1" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut a1 := 10;\nreturn a1++;\n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let mut a1 := 10;\nreturn a1++;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test2" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut a2 := 5;\nreturn a2++;\n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let mut a2 := 5;\nreturn a2++;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test3" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut a3 := 3.5;\nreturn a3++; \n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let mut a3 := 3.5;\nreturn a3++;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test4" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut a4 := \"hello\";\nreturn a4++;\n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let mut a4 := \"hello\";\nreturn a4++;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test5" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut a5 := (1, 2);\nreturn a5++;\n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let mut a5 := (1, 2);\nreturn a5++;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test6" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut a6 := [1, 2, 3];\nreturn a6++;\n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let mut a6 := [1, 2, 3];\nreturn a6++;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test8" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut a8 := true;\nreturn a8++;\n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let mut a8 := true;\nreturn a8++;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test9" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let a9 := 5;\nreturn a9++;\n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let a9 := 5;\nreturn a9++;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test10" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let a10 := 10;\nreturn a10++;\n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let a10 := 10;\nreturn a10++;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test11" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let a11 := 5.5;\nreturn a11++;\n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let a11 := 5.5;\nreturn a11++;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

]

let _ = run_test_tt_main tests

