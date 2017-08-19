(** Module containing the tests of the module Logic *)

open OUnit2
open Constraint
open Logic

(* auxiliary functions *)
let test_equal a b = fun _ -> assert_equal a b
let constraint_ c = Constraint (Atom_C c)
let conjugation_ a b = And (constraint_ a, constraint_ b)
let disjunction_ a b = Or (constraint_ a, constraint_ b)

(* tests for the function string_of_formula *)
let test_string_of_formula = [
  "Constraint" >:: test_equal "c" (string_of_formula (constraint_ "c"));
  "Not" >:: test_equal "¬c" (string_of_formula (Not (constraint_ "c")));
  "And" >:: test_equal "(c1 /\\ c2)" (string_of_formula (And (constraint_ "c1", constraint_ "c2")));
  "Or" >:: test_equal "[c1] \\/ [c2]" (string_of_formula (Or (constraint_ "c1", constraint_ "c2")));
  "Next" >:: test_equal "X(c)" (string_of_formula (X (constraint_ "c")));
]

(*  tests for the function distributivity_next *)
let test_distribute_next = [
  "Otherwise" >:: test_equal (X (constraint_ "c")) (distributivity_next (X (constraint_ "c")));
  "Conjugation" >:: test_equal (And (X (constraint_ "c1"), X (constraint_ "c2"))) (distributivity_next (X (conjugation_ "c1" "c2")));
  "Disjunction" >:: test_equal (Or (X (constraint_ "c1"), X (constraint_ "c2"))) (distributivity_next (X (disjunction_ "c1" "c2")));
]

(* tests for the function list_to_and *)
let test_list_to_and = [
  "One Element" >:: test_equal (constraint_ "c") (list_to_and [(constraint_ "c")]);
  "Two Element" >:: test_equal (And (constraint_ "c", constraint_ "d")) (list_to_and [constraint_ "c"; constraint_ "d"]);
]

(* tests for the function list_to_or *)
let test_list_to_or = [
  "One Element" >:: test_equal (constraint_ "c") (list_to_or [constraint_ "c"]);
  "Two Element" >:: test_equal (Or (constraint_ "c", constraint_ "d")) (list_to_or [constraint_ "c"; constraint_ "d"]);
]

(* suite of tests *)
let suite = [
  "Function string_of_formula" >::: test_string_of_formula;
  "Function distributivity_next" >::: test_distribute_next;
  "Function list_to_and" >::: test_list_to_and;
  "Function list_to_or" >::: test_list_to_or;
]
