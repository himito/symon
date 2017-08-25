(** Module containing some auxiliary functions for tests *)

open OUnit2
open Logic
open Constraint
open Ntcc

(** string functions *)
let string_of_list f l =
  Printf.sprintf "[%s]" (String.concat ";" (List.map f l))

let string_of_optional f o =
  match o with
    Empty -> "Empty"
  | Some x -> Printf.sprintf "Some (%s)" (f x)

(** test functions *)
let test_equal_formula a b = fun _ -> assert_equal ~printer:string_of_formula a b
let test_equal_bool a b = fun _ -> assert_equal ~printer:string_of_bool a b
let test_equal_string a b = fun _ -> assert_equal ~printer:(fun x -> x) a b
let test_equal_process a b = fun _ -> assert_equal ~printer:string_of_process a b
let test_equal_program a b = fun _ -> assert_equal ~printer:(fun x -> string_of_optional string_of_process x) a b
let test_equal a b = fun _ -> assert_equal a b

let test_equal_list_formula a b = fun _ -> assert_equal ~printer:(fun x -> string_of_list string_of_formula x) a b
let test_equal_list_int a b = fun _ -> assert_equal ~printer:(fun x -> string_of_list string_of_int x) a b

(** constructor functions *)
let mk_atom c = Constraint (Atom_C c)
let and_atoms a b = And (mk_atom a, mk_atom b)
let or_atoms a b = Or (mk_atom a, mk_atom b)
