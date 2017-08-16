(** Module containing the tests of the module Symbolic *)

open OUnit2
open Symbolic
open Types
open Utils

(* Auxiliary functions *)
let test_equal_model (f: formula_t) (p:ntcc_process_t) = 
  let sm = symbolic_model p in
  Printf.printf "  * Testing NTCC process: %s\n" (string_of_process p);
  fun _ -> assert_equal f sm 

(* NTCC processes *)
let remark_3_1 = Parallel (Tell (Atomic "c"), Choice [(Atomic "c", Tell (Atomic "d"))])
let example_3_1 = Parallel (Choice [(Atomic "signal", Next (Tell (Atomic "on")))], Unless (Atomic "signal", Next (Tell (Atomic "off"))))
let example_3_2_a = Bang (Tell (Atomic "c"))
let example_3_2_b = Star (Tell (Atomic "c"))
let example_3_3 = Bang (Star (Tell (Atomic "c")))

let control_system = Bang (Parallel (Choice [(Atomic "signal", Next (Tell (Atomic "on")))], Unless (Atomic "signal", Next (Tell (Atomic "off")))))
let asynchronous_behavior = Parallel (Star (Tell (Atomic "error")), Bang (Choice [(Atomic "error", Bang (Tell (Atomic "stop")))]))

let test_symbolic_model = [
  "Remark 3.1" >:: test_equal_model (Constraint (Atomic "ok")) remark_3_1;
]

(* suite of tests *)
let suite = [
  "Function symbolic_model" >::: test_symbolic_model
]
