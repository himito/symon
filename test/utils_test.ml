(** Module containing the tests of the module Utils *)

open OUnit2
open Utils
open Types

(* auxiliar functions *)
let test_equal a b = fun _ -> assert_equal a b
let test_equal_state a b = fun _ -> assert_equal ~cmp: StateSet.equal a b
let test_failure msg f = (fun _ -> assert_raises (Failure msg) (fun () -> f))
let constraint_ c = Constraint (Atomic c)
let conjugation_ a b = And_L (constraint_ a, constraint_ b)
let disjunction_ a b = Or_L (constraint_ a, constraint_ b)
let ntcc_process = Tell (Atomic "c")
let state_ = StateSet.of_list [Present (Atomic "c"); Absent (Atomic "d") ]

(* tests for the function build_list *)
let test_build_list = [
  "Empty list" >:: test_equal [] (build_list 0 0);
  "One element list" >:: test_equal [0] (build_list 0 1);
  "Several elements List" >:: test_equal [1;2;3;4;5;6;7;8;9] (build_list 1 10);
]

(* tests for the function string_of_constraint *)
let test_string_of_constraint = [
  "Atomic" >:: test_equal "c" (string_of_constraint (Atomic "c"));
  "False" >:: test_equal "false" (string_of_constraint False_C);
  "True" >:: test_equal "true" (string_of_constraint True_C);
  "Conjunction" >:: test_equal "c1 ^ true" (string_of_constraint (And_C (Atomic "c1", True_C)));
]

(* tests for the function string_of_formula *)
let test_string_of_formula = [
  "Constraint" >:: test_equal "c" (string_of_formula (constraint_ "c"));
  "Negation" >:: test_equal "¬c" (string_of_formula (Negation (constraint_ "c")));
  "And" >:: test_equal "(c1 ^ c2)" (string_of_formula (And_L (constraint_ "c1", constraint_ "c2")));
  "Or" >:: test_equal "[c1] v [c2]" (string_of_formula (Or_L (constraint_ "c1", constraint_ "c2")));
  "Next" >:: test_equal "o(c)" (string_of_formula (Next_L (constraint_ "c")));
]

(* tests for the function string_of_process *)
let test_string_of_process = [
  "Tell" >:: test_equal "tell(c)" (string_of_process (Tell (Atomic "c")));
  "Parallel" >:: test_equal "tell(c1) || tell(c2)" (string_of_process (Parallel (Tell (Atomic "c1"), Tell (Atomic "c2"))));
  "Next" >:: test_equal "next(tell(c))" (string_of_process (Next (Tell (Atomic "c"))));
  "Start" >:: test_equal "*(tell(c))" (string_of_process (Star (Tell (Atomic "c"))));
  "Bang" >:: test_equal "!(tell(c))" (string_of_process (Bang (Tell (Atomic "c"))));
  "Unless" >:: test_equal "unless (c) next(tell(c))" (string_of_process (Unless (Atomic "c", Next (Tell (Atomic "c")))));
  "Skip" >:: test_equal "skip" (string_of_process Skip);
  "Choice" >:: test_equal "{ when (c) do tell(c1) }" (string_of_process (Choice [(Atomic "c", Tell (Atomic "c1"))]))
]

(* tests for the function unfold_next *)
let test_unfold_next = [
  "Zero" >:: test_equal ntcc_process (unfold_next ntcc_process 0);
  "One" >:: test_equal (Next ntcc_process) (unfold_next ntcc_process 1);
  "Two" >:: test_equal (Next (Next ntcc_process)) (unfold_next ntcc_process 2)
]

(*  tests for the function distribute_next *)
let test_distribute_next = [
  "Otherwise" >:: test_equal (Next_L (constraint_ "c")) (distribute_next (constraint_ "c"));
  "Conjugation" >:: test_equal (And_L (Next_L (constraint_ "c1"), Next_L (constraint_ "c2"))) (distribute_next (conjugation_ "c1" "c2"));
  "Disjunction" >:: test_equal (Or_L (Next_L (constraint_ "c1"), Next_L (constraint_ "c2"))) (distribute_next (disjunction_ "c1" "c2"))
]

(* tests for the function value_to_status *)
let test_value_to_status = [
  "Preset" >:: test_equal Status.Present (value_to_status (Present (Atomic "c")));
  "Absent" >:: test_equal Status.Absent (value_to_status (Absent (Atomic "c")));
]

(* tests for the function positive_part *)
let test_positive_part = [
  "Two element" >:: test_equal_state (StateSet.of_list [Present (Atomic "c")]) (positive_part state_)
]

(* tests for the function negative_part *)
let test_negative_part = [
  "Two elements" >:: test_equal_state (StateSet.of_list [Absent (Atomic "d")]) (negative_part state_)
]

(* tests for the function equivalent_states *)
let test_equivalent_states = [
  "Equivalent" >:: test_equal true (equivalent_states state_ state_);
  "Not Equivalent" >:: test_equal false (equivalent_states state_ (StateSet.of_list [Absent (Atomic "d")]))
]

(* suite of tests *)
let suite = [
  "Function build_list" >::: test_build_list;
  "Function string_of_constraint" >::: test_string_of_constraint;
  "Function string_of_formula" >::: test_string_of_formula;
  "Function string_of_process" >::: test_string_of_process;
  "Function unfold_next" >::: test_unfold_next;
  "Function distribute_next" >::: test_distribute_next;
  "Function value_to_status" >::: test_value_to_status;
  "Function positive_part" >::: test_positive_part;
  "Function negative_part" >::: test_negative_part;
  "Function equivalent_states" >::: test_equivalent_states;
]
