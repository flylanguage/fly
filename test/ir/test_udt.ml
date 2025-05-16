open OUnit2
open Fly_lib
module L = Llvm

let get_sast input =
  try
    let lexbuf = Lexing.from_string input in
    let ast = Parser.program_rule Scanner.tokenize lexbuf in
    let sast = Semant.check ast.body in
    sast
  with
  | err ->
    raise
      (Failure
         (Printf.sprintf
            "Error generating sast for test, is your program correct?: error=%s\n\
             Input:\n\
             %s"
            (Printexc.to_string err)
            input))
;;

let str_contains haystack needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with
  | Not_found -> false
;;

let tests =
  "testing_udt_ir"
  >::: [ ("udt_struct_definition"
          >:: fun _test_ctxt ->
          let fly_code =
            "type Person { name : string, age : int }\n\
             fun main() -> int {\n\
            \  let dummy : Person = Person { name : \"A\", age : 1 };\n\
            \  return 0;\n\
             }"
          in
          let sast = get_sast fly_code in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          assert_bool "TC1_TYP_DEF" (str_contains actual "%Person = type { i8*, i32 }");
          assert_bool "TC1_ALLOCA" (str_contains actual "alloca %Person"))
       ; ("udt_instance_and_field_store"
          >:: fun _test_ctxt ->
          let fly_code =
            "type Person { name : string, age : int }\n\
             fun main() -> () {\n\
            \  let p : Person = Person { name : \"Alice\", age : 30 };\n\
            \  return;\n\
             }"
          in
          let sast = get_sast fly_code in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          assert_bool "TC2_TYP_DEF" (str_contains actual "%Person = type { i8*, i32 }");
          assert_bool "TC2_ALLOCA_P" (str_contains actual "%Person_inst = alloca %Person");
          assert_bool
            "TC2_STORE_NAME_GEP"
            (str_contains
               actual
               "getelementptr inbounds %Person, %Person* %Person_inst, i32 0, i32 0");
          assert_bool
            "TC2_STORE_AGE_GEP"
            (str_contains
               actual
               "getelementptr inbounds %Person, %Person* %Person_inst, i32 0, i32 1");
          assert_bool
            "TC2_STORE_NAME_VAL"
            (str_contains
               actual
               "store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @");
          assert_bool "TC2_STORE_AGE_VAL" (str_contains actual "store i32 30, i32* "))
       ; ("udt_field_access"
          >:: fun _test_ctxt ->
          let fly_code =
            "type Person { name : string, age : int }\n\
             fun main() -> int {\n\
            \  let p : Person = Person { name : \"Bob\", age : 42 };\n\
            \  return p.age;\n\
             }"
          in
          let sast = get_sast fly_code in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          assert_bool "TC3_TYP_DEF" (str_contains actual "%Person.0 = type { i8*, i32 }");
          assert_bool
            "TC3_ALLOCA_P"
            (str_contains actual "%Person_inst = alloca %Person.0");
          assert_bool
            "TC3_GEP_AGE_FOR_RETURN"
            (str_contains
               actual
               "%p_age = getelementptr inbounds %Person.0, %Person.0* %Person_inst, i32 \
                0, i32 1");
          assert_bool
            "TC3_LOAD_AGE"
            (str_contains actual "%p_age_val = load i32, i32* %p_age");
          assert_bool "TC3_RET_AGE" (str_contains actual "ret i32 %p_age_val"))
       ; ("udt_as_function_return"
          >:: fun _test_ctxt ->
          let fly_code =
            "type Point { x : int, y : int }\n\
             fun make_point() -> Point {\n\
            \  return Point { x : 1, y : 2 };\n\
             }"
          in
          let sast = get_sast fly_code in
          let mdl = Irgen.translate sast in
          let actual = L.string_of_llmodule mdl in
          assert_bool "TC4_TYP_DEF" (str_contains actual "%Point = type { i32, i32 }");
          assert_bool
            "TC4_FUNC_SIG_VAL"
            (str_contains actual "define %Point @make_point()");
          assert_bool
            "TC4_ALLOCA_FOR_INSTANCE"
            (str_contains actual "%Point_inst = alloca %Point");
          assert_bool
            "TC4_LOAD_FROM_INSTANCE"
            (str_contains actual "%load_udt_for_ret = load %Point, %Point* %Point_inst");
          assert_bool
            "TC4_RET_LOADED_VALUE"
            (str_contains actual "ret %Point %load_udt_for_ret"))
       ]
;;

let _ = run_test_tt_main tests
