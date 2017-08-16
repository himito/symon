(** Module containing the tests of the module Utils *)

open OUnit2
open Utils
open Types

(* auxiliar function *)
let test_equal a b = fun _ -> assert_equal a b
let test_failure msg f = (fun _ -> assert_raises (Failure msg) (fun () -> f))
let constraint_ c = Constraint (Atomic c)

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
  "Next" >:: test_equal "o(c)" (string_of_formula (Next (constraint_ "c")));
]

(* tests for the function string_of_process *)
let test_string_of_process = [
  "Tell" >:: test_equal "tell(c)" (string_of_process (Tell (Atomic "c")));
  "Parallel" >:: test_equal "tell(c1) || tell(c2)" (string_of_process (Parallel (Tell (Atomic "c1"), Tell (Atomic "c2"))));
  "Next" >:: test_equal "next(tell(c))" (string_of_process (Next (Tell (Atomic "c"))));
  "Start" >:: test_equal "*(tell(c))" (string_of_process (Star (Tell (Atomic "c"))));
  "Bang" >:: test_equal "!(tell(c))" (string_of_process (Bang (Tell (Atomic "c"))));
  "Unless" >:: test_equal "unless (c) next(tell(c))" (string_of_process (Unless (Atomic "c", Tell (Atomic "c"))));
  "Skip" >:: test_equal "skip" (string_of_process Skip);
  "Choice" >:: test_equal "{ when (c) do tell(c1) }" (string_of_process (Choice [(Atomic "c", Tell (Atomic "c1"))]))
]


(* suite of tests *)
let suite = [
  "Function build_list" >::: test_build_list;
  "Function string_of_constraint" >::: test_string_of_constraint;
  "Function string_of_formula" >::: test_string_of_formula;
  "Function string_of_process" >::: test_string_of_process;
]
