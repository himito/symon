(** Module containing the tests of the module Utils *)

open OUnit2
open Utils

(* auxiliar functions *)
let test_equal a b = fun _ -> assert_equal a b

(* tests for the function build_list *)
let test_build_list = [
  "Empty list" >:: test_equal [] (build_list 0 0);
  "One element list" >:: test_equal [0] (build_list 0 1);
  "Several elements List" >:: test_equal [1;2;3;4;5;6;7;8;9] (build_list 1 10);
]

(* suite of tests *)
let suite = [
  "Function build_list" >::: test_build_list;
]
