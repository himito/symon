(** Module collecting all the tests of the project *)

open OUnit2

(*  suite of tests *)
let suite = "All" >::: [
    "Utils module" >::: Utils_test.suite;
  ]

(* test runner *)
let () = run_test_tt_main suite
