open OUnit2
open Fly_lib
module L = Llvm

let printer = fun s -> "\n---\n" ^ s ^ "\n---\n"

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
  "testing_prints"
  >::: [ (* ("print_nothing" *)
         (*     >:: fun _ -> *)
         (*     let sast = get_sast "fun main() -> int {print(); return 0;}" in *)
         (*     let mdl = Irgen.translate sast in *)
         (*     let actual = L.string_of_llmodule mdl in *)
         (*     let expected = *)
         (*       "; ModuleID = 'Fly'\n\ *)
      (*        source_filename = \"Fly\"\n\n\ *)
      (*        define void @main() {\n\ *)
      (*        entry:\n\ *)
      (*       \  ret void\n\ *)
      (*        }\n" *)
         (*     in *)
         (*     _write_to_file actual "test.out"; *)
         (*     assert_equal expected actual ~printer) *)
         (*  ;  *)
         ("print_int"
          >:: fun _ ->
          let sast = get_sast "fun main() -> int {print(1); return 0;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             @int_fmt = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n\n\
             define i32 @main() {\n\
             entry:\n\
            \  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x \
             i8], [4 x i8]* @int_fmt, i32 0, i32 0), i32 1)\n\
            \  ret i32 0\n\
             }\n\n\
             declare i32 @printf(i8*, ...)\n"
          in
          assert_equal expected actual ~printer)
       ; ("print_float"
          >:: fun _ ->
          let sast = get_sast "fun main() -> int {print(1.23); return 0;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             @float_fmt = private unnamed_addr constant [4 x i8] c\"%f\\0A\\00\", align \
             1\n\n\
             define i32 @main() {\n\
             entry:\n\
            \  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x \
             i8], [4 x i8]* @float_fmt, i32 0, i32 0), float 0x3FF3AE1480000000)\n\
            \  ret i32 0\n\
             }\n\n\
             declare i32 @printf(i8*, ...)\n"
          in
          assert_equal expected actual ~printer)
       ; ("print_bool"
          >:: fun _ ->
          let sast = get_sast "fun main() -> int {print(true); return 0;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in

          (* Ok this is getting absurd *)
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             @true_str = private unnamed_addr constant [5 x i8] c\"true\\00\", align 1\n\
             @false_str = private unnamed_addr constant [6 x i8] c\"false\\00\", align 1\n\
             @str_fmt = private unnamed_addr constant [4 x i8] c\"%s\\0A\\00\", align 1\n\n\
             define i32 @main() {\n\
             entry:\n\
            \  br i1 true, label %true_case, label %false_case\n\n\
             true_case:                                        ; preds = %entry\n\
            \  br label %merge\n\n\
             false_case:                                       ; preds = %entry\n\
            \  br label %merge\n\n\
             merge:                                            ; preds = %false_case, \
             %true_case\n\
            \  %bool_str = phi i8* [ getelementptr inbounds ([5 x i8], [5 x i8]* \
             @true_str, i32 0, i32 0), %true_case ], [ getelementptr inbounds ([6 x i8], \
             [6 x i8]* @false_str, i32 0, i32 0), %false_case ]\n\
            \  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x \
             i8], [4 x i8]* @str_fmt, i32 0, i32 0), i8* %bool_str)\n\
            \  ret i32 0\n\
             }\n\n\
             declare i32 @printf(i8*, ...)\n"
          in
          assert_equal expected actual ~printer)
       ; ("print_string"
          >:: fun _ ->
          let sast = get_sast "fun main() -> int {print(\"hello\"); return 0;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             @str = private unnamed_addr constant [6 x i8] c\"hello\\00\", align 1\n\
             @str_fmt = private unnamed_addr constant [4 x i8] c\"%s\\0A\\00\", align 1\n\n\
             define i32 @main() {\n\
             entry:\n\
            \  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x \
             i8], [4 x i8]* @str_fmt, i32 0, i32 0), i8* getelementptr inbounds ([6 x \
             i8], [6 x i8]* @str, i32 0, i32 0))\n\
            \  ret i32 0\n\
             }\n\n\
             declare i32 @printf(i8*, ...)\n"
          in
          _write_to_file actual "test.out";
          assert_equal expected actual ~printer)
       ]
;;

let _ = run_test_tt_main tests
