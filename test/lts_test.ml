(** Module containing the tests of the module Lts *)

open OUnit2
open Lts
open Constraint

(* auxiliar functions *)
let test_equal a b = fun _ -> assert_equal a b
let test_equal_state a b = fun _ -> assert_equal ~cmp: ConstraintSet.equal a b
let state_ = {positive = ConstraintSet.of_list [Atom_C "c"];
              negative = ConstraintSet.of_list [Atom_C "d"]}

(* tests for the function positive_part *)
let test_positive_part = [
  "Two element" >:: test_equal_state (ConstraintSet.of_list [Atom_C "c"]) (positive_part state_)
]

(* tests for the function negative_part *)
let test_negative_part = [
  "Two elements" >:: test_equal_state (ConstraintSet.of_list [Atom_C "d"]) (negative_part state_)
]

(* tests for the function equivalent_states *)
let test_equivalent_states = [
  "Equivalent" >:: test_equal true (equivalent_states state_ state_);
  "Not Equivalent" >:: test_equal false (equivalent_states state_ ({positive = ConstraintSet.of_list [Atom_C "d"]; negative=ConstraintSet.of_list [Atom_C "d"]}))
] 

(* tests for the function check_state_consistency *)
let test_check_state_consistency = [
  "Consistent" >:: test_equal true (check_state_consistency state_)
]

(* suite of tests *)
let suite = [
  "Function positive_part" >::: test_positive_part;
  "Function negative_part" >::: test_negative_part;
  "Function equivalent_states" >::: test_equivalent_states; 
  "Function check_state_consistency" >::: test_check_state_consistency;
]