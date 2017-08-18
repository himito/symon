(** Module implementing the transition system (LTS) *)

open Constraint
open Graph
open Set

(** Set of constraints *)
module ConstraintSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = constraint_t
  end)

(** A formula of a state is a set of constraints *)
type state_formula_t = ConstraintSet.t

(** A LTS state is a structure with a positive and a negative part. Each part 
    consists of a set of constraints *)
type state_t = {
  positive : state_formula_t;
  negative : state_formula_t
}

(** Node of the LTS *)
module Vertex = 
    struct
      type t = state_t
      let compare = Pervasives.compare
      let hash = Hashtbl.hash
      let equal = (=)
    end

(** Definition of the LTS *)
module Graph = Graph.Imperative.Digraph.ConcreteBidirectional (Vertex)

(** Returns the positive part of a LTS state *)
let positive_part (state:state_t) : state_formula_t =
  state.positive

(** Returns the negative part of a LTS state *)
let negative_part (state:state_t) : state_formula_t =
  state.negative

(** Returns if two states are equivalent *)
let equivalent_states (s1:state_t) (s2:state_t) : bool =
  ConstraintSet.equal (positive_part s1) (positive_part s2) && 
  ConstraintSet.equal (negative_part s1) (negative_part s2)

(** Check the consistency of a state *)
let check_state_consistency (s:state_t) : bool =
  let inconsistent_c = ConstraintSet.inter (positive_part s) (negative_part s) in
  ConstraintSet.is_empty inconsistent_c
