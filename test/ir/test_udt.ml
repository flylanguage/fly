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

let str_contains haystack needle =
  match String.index_opt haystack needle.[0] with
  | None -> false
  | Some idx ->
    let len = String.length needle in
    let rec check i =
      if i + len > String.length haystack
      then false
      else if String.sub haystack i len = needle
      then true
      else check (i + 1)
    in
    check idx
;;

let tests =
  "testing_udt_ir"
  >::: [ ("udt_struct_definition"
          >:: fun _ ->
          let sast =
            get_sast
              "type Person { name : string, age : int } fun main() -> () { let dummy := \
               Person { name : \"A\", age : 1 }; }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          assert_bool "UDT struct defined" (str_contains actual "alloca { i8*, i32 }"))
       ; ("udt_instance_and_field_store"
          >:: fun _ ->
          let sast =
            get_sast
              "type Person { name : string, age : int } fun main() -> () { let p := \
               Person { name : \"Alice\", age : 30 }; }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          assert_bool
            "alloca for Person instance"
            (str_contains actual "alloca { i8*, i32 }");
          assert_bool
            "store name field"
            (str_contains
               actual
               "store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @str_");
          assert_bool "store age field" (str_contains actual "store i32 30"))
       ; ("udt_field_access"
          >:: fun _ ->
          let sast =
            get_sast
              "type Person { name : string, age : int } fun main() -> int { let p := \
               Person { name : \"Bob\", age : 42 }; return p.age; }"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          assert_bool "load age field" (str_contains actual "load i32, i32* %p_age");
          assert_bool "return age field" (str_contains actual "ret i32"))
       ; (*
            ("udt_as_function_return"
     >:: fun _ ->
      let sast = get_sast "type Point { x : int, y : int } fun make_point() -> Point { return Point { x = 1, y = 2 }; }" in
      let mdl = Irgen.translate sast in
      let actual = L.string_of_llmodule mdl in
      assert_bool "Point struct defined" (str_contains actual "%Point = type { i32, i32 }");
      assert_bool "function returns Point" (str_contains actual "define %Point* @make_point()");
    ); *)
         ("udt_as_function_argument"
          >:: fun _ ->
          let sast =
            get_sast "type Point { x : int, y : int } fun print_point(p : Point) -> () {}"
          in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          assert_bool
            "function takes Point argument"
            (str_contains actual "define void @print_point({ i32, i32 } %0)"))
       ]
;;

let _ = run_test_tt_main tests
