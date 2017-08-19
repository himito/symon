(** Module implementing the Constraint Temporal Logic (CLTL) *)

open Constraint
open Utils

(** Constraint Temporal Logic (CLTL) formula *)
type formula_t = Constraint of constraint_t    (** a constraint [c] *)
               | And of formula_t * formula_t  (** [and] logic operator *)
               | Or of formula_t * formula_t   (** [or] logic operator *)
               | Not of formula_t              (** [not] logic operator *)
               | X of formula_t                (** [Next] temporal operator*)
               | True                          (** [true] logic constant *)
               | False                         (** [false] logic constant *)

(** Returns the string representation of a formula *)
let rec string_of_formula (f:formula_t) : string =
  match f with
    Constraint c -> string_of_constraint c
  | Not f -> "¬" ^ (string_of_formula f)
  | And (f1, f2) -> Printf.sprintf "(%s /\\ %s)" (string_of_formula f1) (string_of_formula f2)
  | Or (f1,f2) -> Printf.sprintf "[%s] \\/ [%s]" (string_of_formula f1) (string_of_formula f2)
  | X f1 -> Printf.sprintf "X(%s)" (string_of_formula f1)
  | True -> "T"
  | False -> "F"

(** Conjunction constructor function *)
let mk_and (p:formula_t) (q:formula_t) : formula_t = And(p,q)

(** Disjunctive constructor function *)
let mk_or (p:formula_t) (q:formula_t) : formula_t = Or(p,q)

(** Negates a formula *)
let negate = function (Not p ) -> p | p -> Not p

(** Returns if a formula is negative *)
let negative = function (Not p) -> true | _ -> false

(** Returns if a formula is positive *)
let positive (f:formula_t) = not (negative f) 

(* Simplify trivialities *)
let simplify_trivial (f:formula_t) : formula_t =
  match f with
    Not False -> True
  | Not True -> False
  | Not (Not p) -> p                         (* ¬¬p <=> p *)
  | And(p, False) | And(False, p) -> False
  | And(p, True) | And(True, p) -> p
  | Or(p, False) | Or(False, p) -> p
  | Or(p, True) | Or(True, p) -> True
  | _ -> f

(** Simplify a formula *)
let rec simplify_formula (f:formula_t) : formula_t =
  match f with
    Not p -> simplify_trivial (Not (simplify_formula p))
  | X p -> simplify_trivial (X (simplify_formula p))
  | And(p, q) -> simplify_trivial (And (simplify_formula p, simplify_formula q))
  | Or(p, q) -> simplify_trivial (Or (simplify_formula p, simplify_formula q))
  | _ -> f

(** Nesting [X] operator *)
let rec replicate_next (f:formula_t) (i:int) : formula_t =
  match i with
    0 -> f
  | _ -> replicate_next (X f) (i-1)

(** Returns the Negation Normal Form (NNF) of a formula *)
let rec nnf (f:formula_t) (n:int) : formula_t = 
  match f with 
  | And (p, q) -> And (nnf p n, nnf q n)
  | Or (p, q) -> Or (nnf p n, nnf q n)
  | Not (Not p) -> nnf p n                                   (* De Morgan law *)
  | Not (And (p, q)) -> Or (nnf (Not p) n, nnf (Not q) n)    (* De Morgan law *)
  | Not (Or (p, q)) -> And (nnf (Not p) n, nnf (Not q) n)    (* De Morgan law *)
  | Not (X p) -> nnf (Not p) (n+1)                           (* negation elimination*)
  | X p -> nnf p (n+1)
  | _ -> replicate_next f n

(** Returns the simplified NNF of a formula *)
let nnf (f:formula_t) : formula_t = nnf (simplify_formula f) 0 

(** A formulas set *)
module FormulaSet = Set.Make (
  struct
    let compare = Pervasives.compare
    type t = formula_t 
  end)

(** A set of formulas set *)
module SetFormulaSet = Set.Make (
  struct
    let compare = Pervasives.compare
    type t = FormulaSet.t
  end)

(** Returns the string representation of a set of formulas *)
let string_of_formulas_set (s:FormulaSet.t) : string =
  let list_formulas = List.map string_of_formula (FormulaSet.elements s) in
  Printf.sprintf "{ %s }" (String.concat ","  list_formulas)

(** Returns the string representation of a set of formula sets *)
let string_of_set_formulas_set (s:SetFormulaSet.t) : string =
  let list_formulas_set = List.map string_of_formulas_set (SetFormulaSet.elements s) in
  Printf.sprintf "{ %s }" (String.concat "," list_formulas_set)

(** Generates a formula by concatenating each element of the list with an AND *)
let list_to_and (l:formula_t list) : formula_t =
  match l with
    [] -> True
  | _ -> simplify_formula (List.fold_left mk_and True l)

(* Breaks down a conjunction into a list of conjuncts *)
let rec and_to_list (f:formula_t) : formula_t list =
  match f with And(p,q) -> and_to_list p @ and_to_list q | _ -> [f]

(** Generate a formula by concatenating each element of the list with an OR *)
let list_to_or (l:formula_t list) : formula_t =
  match l with
    [] -> False
  | _ -> simplify_formula (List.fold_left mk_or False l)

(* Breaks down a disjunctive into a list of disjuncts *)
let rec or_to_list (f:formula_t) : formula_t list =
  match f with Or(p,q) -> or_to_list p @ and_to_list q | _ -> [f]

(** Applies distribute law to a formula *)
let rec distribute_law (s1: SetFormulaSet.t) (s2: SetFormulaSet.t) : SetFormulaSet.t =
  let all_pairs = cartesian_product (SetFormulaSet.elements s1) (SetFormulaSet.elements s2) in
  SetFormulaSet.of_list (List.map (fun (e1, e2) -> FormulaSet.union e1 e2) all_pairs)

(** Returns if a formula has no complementary elements *)
let consistent_formula (f: FormulaSet.t) : bool =
  let pos, neg = FormulaSet.partition positive f in
  let double_neg = FormulaSet.of_list (List.map negate (FormulaSet.elements neg)) in
  FormulaSet.is_empty (FormulaSet.inter pos double_neg)

(** Translates a formula into its Disjunctive Normal Form (DNF) *)
let rec dnf_ (f:formula_t) : SetFormulaSet.t =
  match f with
    And(p,q) -> distribute_law (dnf_ p) (dnf_ q)
  | Or(p,q) -> SetFormulaSet.union (dnf_ p) (dnf_ q)
  | False -> SetFormulaSet.empty
  | True -> SetFormulaSet.singleton FormulaSet.empty
  | _-> SetFormulaSet.singleton (FormulaSet.singleton f)

(** Returns the simplified DNF of a formula *)
let dnf (f:formula_t) : formula_t =
  let dnf_simplified = SetFormulaSet.filter consistent_formula (dnf_ (nnf f)) in
  let dnf_list = List.map FormulaSet.elements (SetFormulaSet.elements dnf_simplified) in
  list_to_or (List.map list_to_and dnf_list)