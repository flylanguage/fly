open Scanner  (* Assuming Scanner is the module you are testing *)

let%test "tokenize test_func_def_and_call1 program" =
  let input = $(cat "tests/sample_programs/test_func_def_and_call1.fly") in
  let lexbuf = Lexing.from_string input in
  let tokens = Scanner.tokenize lexbuf in
  (* Expected tokens should be defined or checked *)
  (* Example: *)
  (* let expected_tokens = [...] in *)
  (* tokens = expected_tokens *)
  true  (* Placeholder; replace with actual comparison of tokens *)
