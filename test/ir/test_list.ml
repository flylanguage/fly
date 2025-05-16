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
             }\n"
          in
          assert_equal expected actual ~printer)
       ]
;;

let _ = run_test_tt_main tests
