(** Module implementing the constraint system *)

(** Atomic constraint *)
type atom_t = string

(** Constraint System *)
type constraint_t = Atom_C of atom_t
                  | And_C of constraint_t * constraint_t
                  | True_C
                  | False_C


(** Returns the string representation of a constraint *)
let rec string_of_constraint (x:constraint_t) : string =
  match x with
    Atom_C c -> c
  | And_C (c1, c2) -> Printf.sprintf "%s ^ %s" (string_of_constraint c1) (string_of_constraint c2)
  | False_C -> "false"
  | True_C -> "true"

