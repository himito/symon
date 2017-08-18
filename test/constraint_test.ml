(** Module containing the tests of the module Constraint *)

open OUnit2
open Constraint

(* auxiliar functions *)
let test_equal a b = fun _ -> assert_equal a b

(* tests for the function string_of_constraint *)
let test_string_of_constraint = [
  "Atom_C" >:: test_equal "c" (string_of_constraint (Atom_C "c"));
  "False" >:: test_equal "false" (string_of_constraint False_C);
  "True" >:: test_equal "true" (string_of_constraint True_C);
  "Conjunction" >:: test_equal "c1 ^ true" (string_of_constraint (And_C (Atom_C "c1", True_C)));
]

(* suite of tests *)
let suite = [
  "Function string_of_constraint" >::: test_string_of_constraint;
]