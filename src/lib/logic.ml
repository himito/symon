(** Module implementing the Constraing Temporal Logic (CLTL) *)

open Constraint

(** Constraint Temporal Logic (CLTL) *)
type formula_t = Constraint of constraint_t
               | And of formula_t * formula_t
               | Or of formula_t * formula_t
               | Not of formula_t
               | Next_L of formula_t
               | True
               | False

(** Returns the string representation of a formula *)
let rec string_of_formula (f:formula_t) : string =
  match f with
    Constraint c -> string_of_constraint c
  | Not f -> "¬" ^ (string_of_formula f)
  | And (f1, f2) -> Printf.sprintf "(%s /\\ %s)" (string_of_formula f1) (string_of_formula f2)
  | Or (f1,f2) -> Printf.sprintf "[%s] \\/ [%s]" (string_of_formula f1) (string_of_formula f2)
  | Next_L f1 -> Printf.sprintf "o(%s)" (string_of_formula f1)
  | _ -> failwith "function string_of_formula does not support the constructor"


(** Propagates a [Next] operator inside a formula *)
let rec distribute_next (f:formula_t) : formula_t =
  match f with
    Or (a,b) -> Or ((distribute_next a), (distribute_next b))
  | And (a,b) -> And ((distribute_next a), (distribute_next b))
  | _ -> Next_L f

let simplify_base (f:formula_t) : formula_t =
  match f with
    Not False -> True
  | Not True -> False
  | Not(Not p) -> p
  | And(p, False) | And(False, p) -> False
  | And(p, True) | And(True, p) -> p
  | Or(p, False) | Or(False, p) -> p
  | Or(p, True) | Or(True, p) -> True
  | _ -> f

(** Simplify a formula *)
let rec simplify_formula (f:formula_t) : formula_t =
  match f with
    Not p -> simplify_base (Not (simplify_formula p))
  | And(p, q) -> simplify_base (And (simplify_formula p, simplify_formula q))
  | Or(p, q) -> simplify_base (Or (simplify_formula p, simplify_formula q))
  | _ -> f

(** And constructor function *)
let mk_and p q = And(p,q)

(** Generates a formula by concatenating each element of the list with an AND *)
let list_to_and (l:formula_t list) : formula_t =
  if l == [] then
    True
  else
    simplify_formula (List.fold_left mk_and True l)

(* Breaks down a conjunction into a list of conjuncts *)
let rec and_to_list (f:formula_t) : formula_t list =
  match f with And(p,q) -> and_to_list p @ and_to_list q | _ -> [f]

(** Or constructor function *)
let mk_or p q = Or(p,q)

(** Generate a formula by concatenating each element of the list with an OR *)
let list_to_or (l:formula_t list) : formula_t =
  if l == [] then
    False
  else
    simplify_formula (List.fold_left mk_or False l)

(* Breaks down a disjunction into a list of disjuncts *)
let rec or_to_list (f:formula_t) : formula_t list =
  match f with Or(p,q) -> or_to_list p @ and_to_list q | _ -> [f]

(** Applies distribute law to a formula*)
let rec distribute_law (f: formula_t) : formula_t =
  match f with
    And(p, (Or(q,r))) -> mk_or (distribute_law (And(p,q))) (distribute_law (And(p,r)))
  | And(Or(p,q), r) -> mk_or (distribute_law (And(p,r))) (distribute_law (And(q,r)))
  | _ -> f

(** Translates a formula into its Disjunctive Normal Form (DNF) *)
let rec dnf (f:formula_t) : formula_t =
  match f with
    And(p,q) -> distribute_law (And(dnf p, dnf q))
  | Or(p,q) -> Or(dnf p, dnf q)
  | _-> f

