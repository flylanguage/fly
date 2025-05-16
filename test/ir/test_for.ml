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
  >::: [ ("test1"
          >:: fun _ ->
          let sast =
            get_sast "fun function() -> () { for y := [10, 20, 30] { break; } }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define void @function() {\n\
             entry:\n\
            \  %y = alloca i32, align 4\n\
            \  %i = alloca i32, align 4\n\
            \  %list = alloca i32, i32 3, align 4\n\
            \  %index = getelementptr inbounds i32, i32* %list, i32 0\n\
            \  store i32 10, i32* %index, align 4\n\
            \  %index1 = getelementptr inbounds i32, i32* %list, i32 1\n\
            \  store i32 20, i32* %index1, align 4\n\
            \  %index2 = getelementptr inbounds i32, i32* %list, i32 2\n\
            \  store i32 30, i32* %index2, align 4\n\
            \  store i32 0, i32* %i, align 4\n\
            \  br label %loop_cond\n\n\
             loop_cond:                                        ; preds = %loop_body, \
             %entry\n\
            \  %i_val = load i32, i32* %i, align 4\n\
            \  %loop_cond3 = icmp slt i32 %i_val, 3\n\
            \  br i1 %loop_cond3, label %loop_body, label %loop_after\n\n\
             loop_body:                                        ; preds = %loop_cond\n\
            \  %elem_ptr = getelementptr i32, i32* %list, i32 %i_val\n\
            \  %elem_val = load i32, i32* %elem_ptr, align 4\n\
            \  store i32 %elem_val, i32* %y, align 4\n\
            \  br label %loop_after\n\
            \  %i_plus_1 = add i32 %i_val, 1\n\
            \  store i32 %i_plus_1, i32* %i, align 4\n\
            \  br label %loop_cond\n\n\
             loop_after:                                       ; preds = %loop_body, \
             %loop_cond\n\
            \  ret void\n\
             }\n"
          in
          assert_equal expected actual ~printer)
       ; ("test2"
          >:: fun _ ->
          let sast =
            get_sast "fun function() -> () { for y := [10, 20, 30] { print(y); } }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             @int_fmt = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n\n\
             define void @function() {\n\
             entry:\n\
            \  %y = alloca i32, align 4\n\
            \  %i = alloca i32, align 4\n\
            \  %list = alloca i32, i32 3, align 4\n\
            \  %index = getelementptr inbounds i32, i32* %list, i32 0\n\
            \  store i32 10, i32* %index, align 4\n\
            \  %index1 = getelementptr inbounds i32, i32* %list, i32 1\n\
            \  store i32 20, i32* %index1, align 4\n\
            \  %index2 = getelementptr inbounds i32, i32* %list, i32 2\n\
            \  store i32 30, i32* %index2, align 4\n\
            \  store i32 0, i32* %i, align 4\n\
            \  br label %loop_cond\n\n\
             loop_cond:                                        ; preds = %loop_body, \
             %entry\n\
            \  %i_val = load i32, i32* %i, align 4\n\
            \  %loop_cond3 = icmp slt i32 %i_val, 3\n\
            \  br i1 %loop_cond3, label %loop_body, label %loop_after\n\n\
             loop_body:                                        ; preds = %loop_cond\n\
            \  %elem_ptr = getelementptr i32, i32* %list, i32 %i_val\n\
            \  %elem_val = load i32, i32* %elem_ptr, align 4\n\
            \  store i32 %elem_val, i32* %y, align 4\n\
            \  %y4 = load i32, i32* %y, align 4\n\
            \  %call_printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 \
             x i8], [4 x i8]* @int_fmt, i32 0, i32 0), i32 %y4)\n\
            \  %i_plus_1 = add i32 %i_val, 1\n\
            \  store i32 %i_plus_1, i32* %i, align 4\n\
            \  br label %loop_cond\n\n\
             loop_after:                                       ; preds = %loop_cond\n\
            \  ret void\n\
             }\n\n\
             declare i32 @printf(i8*, ...)\n"
          in
          assert_equal expected actual ~printer)

        ; ("test2"
        >:: fun _ ->
        let sast =
          get_sast "fun function() -> () { let a := [1,2,3]; for y := a { print(y); } }"
        in
        let mdl = Irgen.translate sast in
        let actual = L.string_of_llmodule mdl in
        let expected =
          ""
        in
        assert_equal expected actual ~printer)
       ]
;;

let _ = run_test_tt_main tests
