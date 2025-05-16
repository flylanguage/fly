open OUnit2
open Fly_lib

let check_program source_code =
  let lexbuf = Lexing.from_string source_code in
  let ast = Parser.program_rule Scanner.tokenize lexbuf in
  try
    let _ = Semant.check ast.body in
    ""
  with
  | Failure msg -> msg
;;

let tests =
  "testing_return"
  >::: [ ("static_bind"
          >:: fun _ ->
          let actual = check_program "type Person {name: string, age: int} bind new<Person>(name: string, age: int) -> Person { return Person {name: name, age: age}; }" in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("non_static_bind"
          >:: fun _ ->
          let actual =
            check_program
              "type Person {name: string, age: int} bind get_age<Person>(self) -> int { return self.age; }"
          in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
        
      ; ("primitive_bind"
          >:: fun _ ->
          let actual =
            check_program
              "bind add<int>(self, other: int) -> int { return self + other; }"
          in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
      
      ; ("non_primitive_bind"
          >:: fun _ ->
          let actual =
            check_program
              "bind search<list<int>>(self, target: value) -> bool \n\
              \ { \n\
                for v := self { \n\
              \   if (v == target) {
              \     return true;
              \   }
              \ }
              \ return false;
              }\n\
              
              let x := [1,2,3,4,5];
              let res := x.search(3);
              "
          in
          let expected = "" in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       
       ]
;;

let _ = run_test_tt_main tests