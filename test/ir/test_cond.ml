open OUnit2
open Fly_lib (* Assuming this brings in Sast, Irgen, Parser, Scanner, Semant, Utils *)
module L = Llvm

let get_sast input =
  try
    let lexbuf = Lexing.from_string input in
    let ast = Fly_lib.Parser.program_rule Fly_lib.Scanner.tokenize lexbuf in
    let sast = Fly_lib.Semant.check ast.body in
    let unbound_sast = Fly_lib.Unbind.unbind sast in
    unbound_sast
  with
  | err ->
    failwith
      (Printf.sprintf
         "Error generating sast for test, is your program correct?: error=%s\nInput:\n%s"
         (Printexc.to_string err)
         input)
;;

let assert_ir_equal ~ctxt expected actual =
  let _ = ctxt in
  assert_equal
    (String.trim expected)
    (String.trim actual)
    ~printer:(fun s -> "\n" ^ s)
    ~pp_diff:(fun fmt (expected_trimmed, actual_trimmed) ->
      Format.fprintf fmt "EXPECTED:\n%s\nBUT GOT:\n%s\n" expected_trimmed actual_trimmed)
;;

let tests =
  "test_cond_ir"
  >::: [ ("simple_if"
          >:: fun ctxt ->
          let fly_code =
            "fun main() -> int {\n  if (true) {\n    return 1;\n  }\n  return 0;\n}"
          in
          let sast = get_sast fly_code in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @main() {\n\
             entry:\n\
            \  br i1 true, label %then, label %ifcont\n\n\
             then:                                             ; preds = %entry\n\
            \  ret i32 1\n\n\
             ifcont:                                           ; preds = %entry\n\
            \  ret i32 0\n\
             }\n"
          in
          assert_ir_equal ~ctxt expected actual)
       ]
;;

let _ = run_test_tt_main tests
