(** Module containing the tests of the module Symbolic *)

open OUnit2
open Constraint
open Symbolic
open Logic
open Ntcc
open Utilities

(* Auxiliary functions *)
let test_equal_model (f: formula_t) (p:ntcc_process_t) = 
  let sm = symbolic_model p in
  test_equal_formula f sm 

(* NTCC processes *)
let remark_3_1 = Parallel (Tell (Atom_C "c"), Choice [(Atom_C "c", Tell (Atom_C "d"))])
let example_3_1 = Parallel (Choice [(Atom_C "signal", Next (Tell (Atom_C "on")))], Unless (Atom_C "signal", Next (Tell (Atom_C "off"))))
let example_3_2_a = Bang (Tell (Atom_C "c"))
let example_3_2_b = Star (Tell (Atom_C "c"))
let example_3_3 = Bang (Star (Tell (Atom_C "c")))

let control_system = Bang (Parallel (Choice [(Atom_C "signal", Next (Tell (Atom_C "on")))], Unless (Atom_C "signal", Next (Tell (Atom_C "off")))))
let asynchronous_behavior = Parallel (Star (Tell (Atom_C "error")), Bang (Choice [(Atom_C "error", Bang (Tell (Atom_C "stop")))]))

let test_symbolic_model = [
  "Tell" >:: test_equal_model (Constraint (Atom_C "c")) (Tell (Atom_C "c"));
  "Next" >:: test_equal_model (X (mk_atom "c")) (Next (Tell (Atom_C "c")));
  "Parallel" >:: test_equal_model (And (mk_atom "c", mk_atom "d")) (Parallel (Tell (Atom_C "c"), Tell (Atom_C "d")));
  "Unless" >:: test_equal_model (Or (Constraint (Atom_C "c"), And (Not (Constraint (Atom_C "c")), X (Constraint (Atom_C "c"))))) (Unless (Atom_C "c", Tell (Atom_C "c")));
  "When" >::  test_equal_model (Or (And (mk_atom "c", mk_atom "d"), Not (mk_atom "c"))) (Choice [(Atom_C "c", Tell (Atom_C "d"))]);
  "Remark 3.1" >:: test_equal_model (mk_and (mk_atom "c") (mk_atom "d")) remark_3_1
]

(* suite of tests *)
let suite = [
  "Function symbolic_model" >::: test_symbolic_model
]
