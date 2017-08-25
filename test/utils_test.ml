(** Module containing the tests of the module Utils *)

open OUnit2
open Utils
open Ntcc
open Constraint
open Utilities

(* tests for the function build_list *)
let test_build_list = [
  "Empty list" >:: test_equal_list_int [] (build_list 0 0);
  "One element list" >:: test_equal_list_int [0] (build_list 0 1);
  "Several elements List" >:: test_equal_list_int [1;2;3;4;5;6;7;8;9] (build_list 1 10);
]

(* tests for the function process_from_string *)
let test_process_from_string = [
  "Empty String" >::  test_equal_program Empty (process_from_string "");
  "Tell" >::  test_equal_program (Some (Tell (Atom_C "c"))) (process_from_string "Tell (c)");
]

(* suite of tests *)
let suite = [
  "Function build_list" >::: test_build_list;
  "Function process_from_string" >::: test_process_from_string;
]
