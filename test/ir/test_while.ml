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

let printer = fun s -> "\n---\n" ^ s ^ "\n---\n"

let _write_to_file text filename =
  let channel = open_out filename in
  Printf.fprintf channel "%s" text;
  close_out channel
;;

let tests =
  "testing_ir"
  >::: [ ("test_break"
          >:: fun _ ->
          let sast = get_sast "fun function() -> () {while {break;}}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define void @function() {\n\
             entry:\n\
            \  br label %loop_body\n\n\
             loop_body:                                        ; preds = %loop_body, \
             %entry\n\
            \  br label %loop_exit\n\
            \  br label %loop_body\n\n\
             loop_exit:                                        ; preds = %loop_body\n\
             }\n"
          in
          assert_equal expected actual ~printer)
       ; ("test_nested_print_and_return"
          >:: fun _ ->
          let sast =
            get_sast
              "fun function() -> int {while { let a : int = 4; print(a); return 0;}}"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             @int_fmt = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n\n\
             define i32 @function() {\n\
             entry:\n\
            \  br label %loop_body\n\n\
             loop_body:                                        ; preds = %loop_body, \
             %entry\n\
            \  %a = alloca i32, align 4\n\
            \  store i32 4, i32* %a, align 4\n\
            \  %a1 = load i32, i32* %a, align 4\n\
            \  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x \
             i8], [4 x i8]* @int_fmt, i32 0, i32 0), i32 %a1)\n\
            \  ret i32 0\n\
            \  br label %loop_body\n\n\
             loop_exit:                                        ; No predecessors!\n\
             }\n\n\
             declare i32 @printf(i8*, ...)\n"
          in
          assert_equal expected actual ~printer)
       ; ("test_nested_while_loops"
          >:: fun _ ->
          let sast =
            get_sast
              "fun function() -> int {\n\
              \               while {\n\
              \                 let x : int = 1;\n\
              \                 print(x);\n\
              \                 while {\n\
              \                   let y : int = 2;\n\
              \                   print(y);\n\
              \                   return 0;\n\
              \                 }\n\
              \               }\n\
              \             }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             @int_fmt = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n\
             @int_fmt.1 = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align \
             1\n\n\
             define i32 @function() {\n\
             entry:\n\
            \  br label %loop_body\n\n\
             loop_body:                                        ; preds = %loop_body, \
             %entry\n\
            \  %x = alloca i32, align 4\n\
            \  store i32 1, i32* %x, align 4\n\
            \  %x1 = load i32, i32* %x, align 4\n\
            \  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x \
             i8], [4 x i8]* @int_fmt, i32 0, i32 0), i32 %x1)\n\
            \  br label %loop_body2\n\
            \  br label %loop_body\n\n\
             loop_exit:                                        ; No predecessors!\n\n\
             loop_body2:                                       ; preds = %loop_body2, \
             %loop_body\n\
            \  %y = alloca i32, align 4\n\
            \  store i32 2, i32* %y, align 4\n\
            \  %y4 = load i32, i32* %y, align 4\n\
            \  %printf5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x \
             i8], [4 x i8]* @int_fmt.1, i32 0, i32 0), i32 %y4)\n\
            \  ret i32 0\n\
            \  br label %loop_body2\n\n\
             loop_exit3:                                       ; No predecessors!\n\
             }\n\n\
             declare i32 @printf(i8*, ...)\n"
          in
          assert_equal expected actual ~printer)
       ; ("test_break_inner_return_outer"
          >:: fun _ ->
          let sast =
            get_sast
              "fun function() -> int {\n\
              \               while {\n\
              \                 while {\n\
              \                   break;\n\
              \                 }\n\
              \                 return 1;\n\
              \               }\n\
              \             }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @function() {\n\
             entry:\n\
            \  br label %loop_body\n\n\
             loop_body:                                        ; preds = %loop_body, \
             %entry\n\
            \  br label %loop_body1\n\
            \  br label %loop_body\n\n\
             loop_exit:                                        ; No predecessors!\n\n\
             loop_body1:                                       ; preds = %loop_body1, \
             %loop_body\n\
            \  br label %loop_exit2\n\
            \  br label %loop_body1\n\n\
             loop_exit2:                                       ; preds = %loop_body1\n\
            \  ret i32 1\n\
             }\n"
          in
          assert_equal expected actual ~printer)
       ]
;;

let _ = run_test_tt_main tests
