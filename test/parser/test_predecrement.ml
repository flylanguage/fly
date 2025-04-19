open OUnit2
open Fly_lib
open Print_lib.Prints

let tests = "testing_predecrement" >::: [
    
        "test1" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut x1 := 5;\nreturn --x1;\n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let mut x1 := 5;\nreturn --x1;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test2" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut x2 := 5.5;\nreturn --x2;\n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let mut x2 := 5.5;\nreturn --x2;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test3" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut x3 := true;\nreturn --x3;\n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let mut x3 := true;\nreturn --x3;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test4" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut x4 := \"Hello\";\nreturn --x4;\n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let mut x4 := \"Hello\";\nreturn --x4;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test5" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut x5 := [1, 2, 3];\nreturn --x5;\n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let mut x5 := [1, 2, 3];\nreturn --x5;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));


        "test7" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut x7 := 'a';\nreturn --x7;\n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let mut x7 := 'a';\nreturn --x7;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test8" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let mut x8 := (1, 2);\nreturn --x8;\n" in
        let program = Parser.program_rule Scanner.tokenize lexbuf in 
          let actual = string_of_program program in
        let expected = "let mut x8 := (1, 2);\nreturn --x8;\n" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

]

let _ = run_test_tt_main tests

