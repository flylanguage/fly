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
  "testing_lists"
  >::: [ ("local_list"
          >:: fun _ ->
          let sast = get_sast "fun function() -> () {let a := [10, 20, 30];}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define void @function() {\n\
             entry:\n\
            \  %list = alloca i32, i32 3, align 4\n\
            \  %index = getelementptr inbounds i32, i32* %list, i32 0\n\
            \  store i32 10, i32* %index, align 4\n\
            \  %index1 = getelementptr inbounds i32, i32* %list, i32 1\n\
            \  store i32 20, i32* %index1, align 4\n\
            \  %index2 = getelementptr inbounds i32, i32* %list, i32 2\n\
            \  store i32 30, i32* %index2, align 4\n\
            \  %a = alloca i32*, align 8\n\
            \  store i32* %list, i32** %a, align 8\n\
            \  ret void\n\
             }\n"
          in
          assert_equal expected actual ~printer)
       ; ("local_bool_list"
          >:: fun _ ->
          let sast = get_sast "fun function() -> () {let a := [true, false];}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define void @function() {\n\
             entry:\n\
            \  %list = alloca i1, i32 2, align 1\n\
            \  %index = getelementptr inbounds i1, i1* %list, i32 0\n\
            \  store i1 true, i1* %index, align 1\n\
            \  %index1 = getelementptr inbounds i1, i1* %list, i32 1\n\
            \  store i1 false, i1* %index1, align 1\n\
            \  %a = alloca i1*, align 8\n\
            \  store i1* %list, i1** %a, align 8\n\
            \  ret void\n\
             }\n"
          in
          _write_to_file actual "actual.out";
          assert_equal expected actual ~printer)
       ; ("local_string_list"
          >:: fun _ ->
          let sast = get_sast "fun function() -> () {let a := [\"hello\", \"world\"];}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             @str = private unnamed_addr constant [6 x i8] c\"hello\\00\", align 1\n\
             @str.1 = private unnamed_addr constant [6 x i8] c\"world\\00\", align 1\n\n\
             define void @function() {\n\
             entry:\n\
            \  %list = alloca i8*, i32 2, align 8\n\
            \  %index = getelementptr inbounds i8*, i8** %list, i32 0\n\
            \  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @str, i32 0, i32 \
             0), i8** %index, align 8\n\
            \  %index1 = getelementptr inbounds i8*, i8** %list, i32 1\n\
            \  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @str.1, i32 0, i32 \
             0), i8** %index1, align 8\n\
            \  %a = alloca i8**, align 8\n\
            \  store i8** %list, i8*** %a, align 8\n\
            \  ret void\n\
             }\n"
          in
          (* _write_to_file actual "actual.out"; *)
          assert_equal expected actual ~printer)
       ; ("local_int_list_index"
          >:: fun _ ->
          let sast = get_sast "fun function() -> int {let a := [1, 2]; return a[0];}" in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @function() {\n\
             entry:\n\
            \  %list = alloca i32, i32 2, align 4\n\
            \  %index = getelementptr inbounds i32, i32* %list, i32 0\n\
            \  store i32 1, i32* %index, align 4\n\
            \  %index1 = getelementptr inbounds i32, i32* %list, i32 1\n\
            \  store i32 2, i32* %index1, align 4\n\
            \  %a = alloca i32*, align 8\n\
            \  store i32* %list, i32** %a, align 8\n\
            \  %a2 = load i32*, i32** %a, align 8\n\
            \  %elem_ptr = getelementptr i32, i32* %a2, i32 0\n\
            \  %elem_val = load i32, i32* %elem_ptr, align 4\n\
            \  ret i32 %elem_val\n\
             }\n"
          in
          (* _write_to_file actual "actual.out"; *)
          assert_equal expected actual ~printer)
       ; ("list_of_structs_index_field"
          >:: fun _ ->
          let sast =
            get_sast
              "type Point { x:int, y:int } fun main() -> int { let p1 := Point{x:1, \
               y:2}; let p2 := Point{x:3, y:4}; let arr := [p1, p2]; return arr[1].y; }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          let expected =
            "; ModuleID = 'Fly'\n\
             source_filename = \"Fly\"\n\n\
             define i32 @main() {\n\
             entry:\n\
            \  %Point_inst = alloca { i32, i32 }, align 8\n\
            \  %Point_x = getelementptr inbounds { i32, i32 }, { i32, i32 }* \
             %Point_inst, i32 0, i32 0\n\
            \  store i32 1, i32* %Point_x, align 4\n\
            \  %Point_y = getelementptr inbounds { i32, i32 }, { i32, i32 }* \
             %Point_inst, i32 0, i32 1\n\
            \  store i32 2, i32* %Point_y, align 4\n\
            \  %Point_inst1 = alloca { i32, i32 }, align 8\n\
            \  %Point_x2 = getelementptr inbounds { i32, i32 }, { i32, i32 }* \
             %Point_inst1, i32 0, i32 0\n\
            \  store i32 3, i32* %Point_x2, align 4\n\
            \  %Point_y3 = getelementptr inbounds { i32, i32 }, { i32, i32 }* \
             %Point_inst1, i32 0, i32 1\n\
            \  store i32 4, i32* %Point_y3, align 4\n\
            \  %list = alloca { i32, i32 }, i32 2, align 8\n\
            \  %index = getelementptr inbounds { i32, i32 }, { i32, i32 }* %list, i32 0\n\
            \  store { i32, i32 }* %Point_inst, { i32, i32 }* %index, align 8\n\
            \  %index4 = getelementptr inbounds { i32, i32 }, { i32, i32 }* %list, i32 1\n\
            \  store { i32, i32 }* %Point_inst1, { i32, i32 }* %index4, align 8\n\
            \  %arr = alloca { i32, i32 }*, align 8\n\
            \  store { i32, i32 }* %list, { i32, i32 }** %arr, align 8\n\
            \  %arr5 = load { i32, i32 }*, { i32, i32 }** %arr, align 8\n\
            \  %elem_ptr = getelementptr { i32, i32 }, { i32, i32 }* %arr5, i32 1\n\
            \  %arr_y = getelementptr inbounds { i32, i32 }, { i32, i32 }* %elem_ptr, \
             i32 0, i32 1\n\
            \  %arr_y_val = load i32, i32* %arr_y, align 4\n\
            \  ret i32 %arr_y_val\n\
             }\n"
          in
          assert_equal expected actual ~printer)
       ; ("list_of_enums_index"
          >:: fun _ ->
          let sast =
            get_sast
              "enum Color { Red, Green, Blue } fun main() -> Color { let arr := \
               [Color::Red, Color::Blue]; return arr[1]; }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          assert_bool
            "alloca for enum list"
            (try
               ignore (Str.search_forward (Str.regexp "alloca i32, i32 2") actual 0);
               true
             with
             | Not_found -> false);
          assert_bool
            "return enum value"
            (try
               ignore (Str.search_forward (Str.regexp "ret i32") actual 0);
               true
             with
             | Not_found -> false))
       ]
;;

let _ = run_test_tt_main tests
