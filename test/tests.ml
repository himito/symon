(** Module collecting all the tests of the project *)

open OUnit2

(*  suite of tests *)
let suite = "All" >::: [
  "Constraint module" >::: Constraint_test.suite;
  "Logic module" >::: Logic_test.suite;
  "LTS module" >::: Lts_test.suite;
  "NTCC module" >::: Ntcc_test.suite;
  "Symbolic module">::: Symbolic_test.suite;
  "Utils module" >::: Utils_test.suite;
  ]

(* test runner *)
let () = run_test_tt_main suite
