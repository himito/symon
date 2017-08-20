(** Module containing the tests of the module Logic *)

open OUnit2
open Constraint
open Logic
open Utilities

(* tests for the function string_of_formula *)
let test_string_of_formula = [
  "Constraint" >:: test_equal_string "c" (string_of_formula (mk_atom "c"));
  "Not" >:: test_equal_string "¬c" (string_of_formula (Not (mk_atom "c")));
  "And" >:: test_equal_string "(c1 /\\ c2)" (string_of_formula (And (mk_atom "c1", mk_atom "c2")));
  "Or" >:: test_equal_string "(c1 \\/ c2)" (string_of_formula (Or (mk_atom "c1", mk_atom "c2")));
  "Next" >:: test_equal_string "X(c)" (string_of_formula (X (mk_atom "c")));
]

(** tests for the function mk_and *)
let test_mk_and = [
  "Basic" >:: test_equal_formula (And (mk_atom "c", mk_atom "d")) (mk_and (mk_atom "c") (mk_atom "d"));
]

(** tests for the function mk_or *)
let test_mk_or = [
  "Basic" >:: test_equal_formula (Or (mk_atom "c", mk_atom "d")) (mk_or (mk_atom "c") (mk_atom "d"));
]

(** tests for the function negate *)
let test_negate = [
  "Basic" >:: test_equal_formula (Not (mk_atom "c")) (negate (mk_atom "c"));
  "Double Negation" >:: test_equal_formula (mk_atom "c") (negate (Not (mk_atom "c")));
]

(** tests for the function negative *)
let test_negative = [
  "True" >:: test_equal_bool true (negative (Not (mk_atom "c")));
  "False" >:: test_equal_bool false (negative (mk_atom "c"))
]

(** tests for the function positive *)
let test_positive = [
  "True" >:: test_equal_bool true (positive (mk_atom "c"));
  "False" >:: test_equal_bool false (positive (Not (mk_atom "c")));
]

(** tests for the function replicate_next *)
let test_replicate_next = [
  "0 times" >:: test_equal_formula (mk_atom "c") (replicate_next (mk_atom "c") 0);
  "negative value" >:: test_equal_formula (mk_atom "c") (replicate_next (mk_atom "c") (-1));
  "positive value" >:: test_equal_formula (X(X(X(mk_atom "c")))) (replicate_next (mk_atom "c") 3);
]

(** tests for the function conjuncts *)
let test_conjuncts = [
  "One Element" >:: test_equal_list_formula [True] (conjuncts True);
  "Two Element" >:: test_equal_list_formula [mk_atom "c"; mk_atom "d"] (conjuncts (and_atoms "c" "d"));
]

(** tests for the function disjuncts *)
let test_disjuncts = [
  "One Element" >:: test_equal_list_formula [False] (disjuncts False);
  "Two Element" >:: test_equal_list_formula [mk_atom "c"; mk_atom "d"] (disjuncts (or_atoms "c" "d"));
]

(** tests for the function simplify_trivial *)
let test_simplify_trivial = [
  "Identity" >:: test_equal_formula False (simplify_trivial False);
  "Negation True" >:: test_equal_formula False (simplify_trivial (Not True));
  "Negation False" >:: test_equal_formula True (simplify_trivial (Not False));
  "Double Negation" >:: test_equal_formula True (simplify_trivial (Not (Not True)));
  "Identity for Or" >:: test_equal_formula (mk_atom "c") (simplify_trivial (mk_or False (mk_atom "c")));
  "Identity for And" >:: test_equal_formula (mk_atom "c") (simplify_trivial (mk_and True (mk_atom "c")));
  "Annihilator for Or" >:: test_equal_formula True (simplify_trivial (mk_or True (mk_atom "c")));
  "Annihilator for And" >:: test_equal_formula False (simplify_trivial (mk_and False (mk_atom "c")));
]

(** tests for the function simplify_formula *)
let test_simplify_formula = [
  "Simplification Negation" >:: test_equal_formula (mk_atom "c") (simplify_formula (Not (Not (Not (Not (mk_atom "c"))))));
  "Simplification And" >:: test_equal_formula (mk_and (mk_atom "c") (mk_atom "d")) (simplify_formula (mk_and (mk_atom "c") (mk_and True (mk_atom "d"))));
  "Simplification Or" >:: test_equal_formula (mk_or (mk_atom "c") (mk_atom "d")) (simplify_formula (mk_or (mk_atom "c") (mk_or False (mk_atom "d"))));
]

(* tests for the function list_to_and *)
let test_list_to_and = [
  "Empty List" >:: test_equal_formula True (list_to_and []);
  "One Element" >:: test_equal_formula (mk_atom "c") (list_to_and [(mk_atom "c")]);
  "Two Element" >:: test_equal_formula (And (mk_atom "c", mk_atom "d")) (list_to_and [mk_atom "c"; mk_atom "d"]);
]

(* tests for the function list_to_or *)
let test_list_to_or = [
  "Empty List" >:: test_equal_formula False (list_to_or []);
  "One Element" >:: test_equal_formula (mk_atom "c") (list_to_or [mk_atom "c"]);
  "Two Element" >:: test_equal_formula (Or (mk_atom "c", mk_atom "d")) (list_to_or [mk_atom "c"; mk_atom "d"]);
]

(** tests for the function nnf *)
let test_nnf = [
  "De Morgan law" >:: test_equal_formula (mk_or (Not (mk_atom "c")) (Not (mk_atom "d"))) (nnf (Not (mk_and (mk_atom "c") (mk_atom "d"))));
]

(* suite of tests *)
let suite = [
  "Function string_of_formula" >::: test_string_of_formula;
  "Function mk_and" >::: test_mk_and;
  "Function mk_or" >::: test_mk_or;
  "Function negate" >::: test_negate;
  "Function negative" >::: test_negative;
  "Function positive" >::: test_positive;
  "Function replicate_next" >::: test_replicate_next;
  "Function conjuncts" >::: test_conjuncts;
  "Function disjuncts" >::: test_disjuncts;
  "Function simplify_trivial" >::: test_simplify_trivial;
  "Function simplify_formula" >::: test_simplify_formula;
  "Function list_to_and" >::: test_list_to_and;
  "Function list_to_or" >::: test_list_to_or;
  "Function nnf" >::: test_nnf;
]
