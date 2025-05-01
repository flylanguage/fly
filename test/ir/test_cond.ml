open OUnit2
open Fly_lib
module L = Llvm

let get_sast input =
  try
    let lexbuf = Lexing.from_string input in
    let ast = Fly_lib.Parser.program_rule Fly_lib.Scanner.tokenize lexbuf in
    let sast = Fly_lib.Semant.check ast.body in
    sast
  with
  | err ->
    raise
      (Failure
         (Printf.sprintf
            "Error generating sast, is your program correct?: error=%s"
            (Printexc.to_string err)))
;;

let _write_to_file text filename =
  let channel = open_out filename in
  Printf.fprintf channel "%s" text;
  close_out channel
;;

let tests =
  "testing_ir"
  >::: [ ("simple_if"
          >:: fun _ ->
          let sast = get_sast "fun main() -> int {if (true) {return 1;} return 0;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @main() {\n\
             entry:\n\
            \  br i1 true, label %then, label %if_end\n\n\
             then:                                             ; preds = %entry\n\
            \  ret i32 1\n\n\
             if_end:                                           ; preds = %entry\n\
            \  ret i32 0\n\
             }\n"
          in
          (* _write_to_file actual "test.out"; *)
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("ifelse"
          >:: fun _ ->
          let sast =
            get_sast
              "fun main() -> int {if (true) {return 1;} else {return 0;} return 2;}"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @main() {\n\
             entry:\n\
            \  br i1 true, label %then, label %if_end\n\n\
             then:                                             ; preds = %entry\n\
            \  ret i32 1\n\n\
             if_end:                                           ; preds = %entry\n\
            \  ret i32 0\n\
             }\n"
          in
          (* _write_to_file actual "test.out"; *)
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ]
;;

let _ = run_test_tt_main tests
