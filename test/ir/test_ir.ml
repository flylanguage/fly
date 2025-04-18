open OUnit2
open Fly_lib
(* (* open Print_lib.Prints *) *)
module L = Llvm

let tests =
  "testing_ir"
  >::: [ ("test1"
          >:: fun _ ->
          let mdl = Irgen.translate in
          let actual = L.string_of_llmodule mdl in
          let expected = "; ModuleID = 'Fly'\nsource_filename = \"Fly\"\n" in
          (* let _ = Printf.printf "\nhave:\n%s\n" actual in *)
          (* let _ = Printf.printf "\nwant:\n%s\n" expected in *)
          assert_equal expected actual)
       ]
;;

let _ = run_test_tt_main tests
