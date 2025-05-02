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
       ; ("if-else"
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
            \  br i1 true, label %then, label %else\n\n\
             then:                                             ; preds = %entry\n\
            \  ret i32 1\n\n\
             if_end:                                           ; No predecessors!\n\
            \  ret i32 2\n\n\
             else:                                             ; preds = %entry\n\
            \  ret i32 0\n\
             }\n"
          in
          (* _write_to_file actual "test.out"; *)
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("if-elif"
          >:: fun _ ->
          let sast =
            get_sast
              "fun main() -> int {\n\
              \  if (true) {return 1;} \n\
              \  else if (true) {return 3;}\n\
              \  return 2;\n\
               }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @main() {\n\
             entry:\n\
            \  br i1 true, label %then, label %else\n\n\
             then:                                             ; preds = %entry\n\
            \  ret i32 1\n\n\
             if_end:                                           ; preds = %else\n\
            \  ret i32 2\n\n\
             else:                                             ; preds = %entry\n\
            \  br i1 true, label %then1, label %if_end\n\n\
             then1:                                            ; preds = %else\n\
            \  ret i32 3\n\
             }\n"
          in
          (* _write_to_file actual "test.out"; *)
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ; ("if-elif-else"
          >:: fun _ ->
          let sast =
            get_sast
              "fun main() -> int {\n\
              \  if (true) {return 1;} \n\
              \  else if (true) {return 3;}\n\
              \  else {return 0;}\n\
              \  return 2;\n\
               }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @main() {\n\
             entry:\n\
            \  br i1 true, label %then, label %else\n\n\
             then:                                             ; preds = %entry\n\
            \  ret i32 1\n\n\
             if_end:                                           ; No predecessors!\n\
            \  ret i32 2\n\n\
             else:                                             ; preds = %entry\n\
            \  br i1 true, label %then1, label %else2\n\n\
             then1:                                            ; preds = %else\n\
            \  ret i32 3\n\n\
             else2:                                            ; preds = %else\n\
            \  ret i32 0\n\
             }\n"
          in
          (* _write_to_file actual "test.out"; *)
          assert_equal expected actual ~printer:(fun s -> "\n---\n" ^ s ^ "\n---\n"))
       ]
;;

let _ = run_test_tt_main tests
