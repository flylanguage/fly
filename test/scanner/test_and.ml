open Scanner
open Prints

let%expect_test "and1" =
  let lexbuf = Lexing.from_string "a1 && b1;" in
  let tokenseq = Scanner.tokenize lexbuf in
  List.iter print_token tokenseq;
  [%expect{|
    ID(a1) AND ID(b1) SEMI
  |}]
