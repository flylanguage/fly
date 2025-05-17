open OUnit2
open Fly_lib
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
  >::: [ ("local_string"
          >:: fun _ ->
          let sast = get_sast "fun function() -> () {let a := \"hello\";}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             @str = private unnamed_addr constant [6 x i8] c\"hello\\00\", align 1\n\n\
             define void @function() {\n\
             entry:\n\
            \  %a = alloca i8*, align 8\n\
            \  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @str, i32 0, i32 \
             0), i8** %a, align 8\n\
            \  ret void\n\
             }\n"
          in
          assert_equal expected actual ~printer)
       ; ("global_string"
          >:: fun _ ->
          let sast = get_sast "let a := \"hello\"; fun function() -> () {let b := a;}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             @str = private unnamed_addr constant [6 x i8] c\"hello\\00\", align 1\n\n\
             define void @function() {\n\
             entry:\n\
            \  %b = alloca i8*, align 8\n\
            \  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @str, i32 0, i32 \
             0), i8** %b, align 8\n\
            \  ret void\n\
             }\n"
          in
          assert_equal expected actual ~printer)
       ; ("concat_string"
          >:: fun _ ->
          let sast =
            get_sast
              "fun function() -> () {let a := \"hello\"; let b := \" World\"; a + b; }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             @str = private unnamed_addr constant [6 x i8] c\"hello\\00\", align 1\n\
             @str.1 = private unnamed_addr constant [7 x i8] c\" World\\00\", align 1\n\n\
             define void @function() {\n\
             entry:\n\
            \  %a = alloca i8*, align 8\n\
            \  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @str, i32 0, i32 \
             0), i8** %a, align 8\n\
            \  %b = alloca i8*, align 8\n\
            \  store i8* getelementptr inbounds ([7 x i8], [7 x i8]* @str.1, i32 0, i32 \
             0), i8** %b, align 8\n\
            \  %a1 = load i8*, i8** %a, align 8\n\
            \  %b2 = load i8*, i8** %b, align 8\n\
            \  %strlen1 = call i32 @strlen(i8* %a1)\n\
            \  %strlen2 = call i32 @strlen(i8* %b2)\n\
            \  %total_len = add i32 %strlen1, %strlen2\n\
            \  %total_len_plus_one = add i32 %total_len, 1\n\
            \  %new_str = alloca i8, i32 %total_len_plus_one, align 1\n\
            \  store i8 0, i8* %new_str, align 1\n\
            \  %strcat1 = call i8* @strcat(i8* %new_str, i8* %a1)\n\
            \  %strcat2 = call i8* @strcat(i8* %new_str, i8* %b2)\n\
            \  ret void\n\
             }\n\n\
             declare i32 @strlen(i8*)\n\n\
             declare i8* @strcat(i8*, i8*)\n"
          in
          assert_equal expected actual ~printer)
       ; ("print_concat_string"
          >:: fun _ ->
          let sast =
            get_sast
              "fun function() -> () {let a := \"hello\"; let b := \" World\"; print(a + \
               b); }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             @str = private unnamed_addr constant [6 x i8] c\"hello\\00\", align 1\n\
             @str.1 = private unnamed_addr constant [7 x i8] c\" World\\00\", align 1\n\
             @str_fmt = private unnamed_addr constant [4 x i8] c\"%s\\0A\\00\", align 1\n\n\
             define void @function() {\n\
             entry:\n\
            \  %a = alloca i8*, align 8\n\
            \  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @str, i32 0, i32 \
             0), i8** %a, align 8\n\
            \  %b = alloca i8*, align 8\n\
            \  store i8* getelementptr inbounds ([7 x i8], [7 x i8]* @str.1, i32 0, i32 \
             0), i8** %b, align 8\n\
            \  %a1 = load i8*, i8** %a, align 8\n\
            \  %b2 = load i8*, i8** %b, align 8\n\
            \  %strlen1 = call i32 @strlen(i8* %a1)\n\
            \  %strlen2 = call i32 @strlen(i8* %b2)\n\
            \  %total_len = add i32 %strlen1, %strlen2\n\
            \  %total_len_plus_one = add i32 %total_len, 1\n\
            \  %new_str = alloca i8, i32 %total_len_plus_one, align 1\n\
            \  store i8 0, i8* %new_str, align 1\n\
            \  %strcat1 = call i8* @strcat(i8* %new_str, i8* %a1)\n\
            \  %strcat2 = call i8* @strcat(i8* %new_str, i8* %b2)\n\
            \  %call_printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 \
             x i8], [4 x i8]* @str_fmt, i32 0, i32 0), i8* %new_str)\n\
            \  ret void\n\
             }\n\n\
             declare i32 @strlen(i8*)\n\n\
             declare i8* @strcat(i8*, i8*)\n\n\
             declare i32 @printf(i8*, ...)\n"
          in
          _write_to_file actual "actual.out";
          assert_equal expected actual ~printer)
       ]
;;

let _ = run_test_tt_main tests
