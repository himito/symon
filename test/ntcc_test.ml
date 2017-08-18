(** Module containing the tests of the module Ntcc *)

open OUnit2
open Constraint
open Ntcc

(* auxiliar functions *)
let test_equal a b = fun _ -> assert_equal a b
let ntcc_process = Tell (Atom_C "c")

(* tests for the function string_of_process *)
let test_string_of_process = [
  "Tell" >:: test_equal "tell(c)" (string_of_process (Tell (Atom_C "c")));
  "Parallel" >:: test_equal "tell(c1) || tell(c2)" (string_of_process (Parallel (Tell (Atom_C "c1"), Tell (Atom_C "c2"))));
  "Next" >:: test_equal "next(tell(c))" (string_of_process (Next (Tell (Atom_C "c"))));
  "Start" >:: test_equal "*(tell(c))" (string_of_process (Star (Tell (Atom_C "c"))));
  "Bang" >:: test_equal "!(tell(c))" (string_of_process (Bang (Tell (Atom_C "c"))));
  "Unless" >:: test_equal "unless (c) next(tell(c))" (string_of_process (Unless (Atom_C "c", Tell (Atom_C "c"))));
  "Skip" >:: test_equal "skip" (string_of_process Skip);
  "Choice" >:: test_equal "{ when (c) do tell(c1) }" (string_of_process (Choice [(Atom_C "c", Tell (Atom_C "c1"))]))
]

(* tests for the function unfold_next *)
let test_unfold_next = [
  "Zero" >:: test_equal ntcc_process (unfold_next ntcc_process 0);
  "One" >:: test_equal (Next ntcc_process) (unfold_next ntcc_process 1);
  "Two" >:: test_equal (Next (Next ntcc_process)) (unfold_next ntcc_process 2)
]

(* suite of tests *)
let suite = [
  "Function string_of_process" >::: test_string_of_process;
  "Function unfold_next" >::: test_unfold_next;
]