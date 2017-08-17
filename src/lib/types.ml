(** This module contains the types for defining constraints, ntcc processes,
    logic formulas and label transition systems (LTS). *)

open Graph
open Set

(** Constraint System *)
type constraint_t = Atomic of string
                  | And_C of constraint_t * constraint_t
                  | True_C
                  | False_C

(** Constraint Temporal Logic (CLTL) *)
type formula_t = Constraint of constraint_t
               | And_L of formula_t * formula_t
               | Or_L of formula_t * formula_t
               | Negation of formula_t
               | Next_L of formula_t
               | Always of formula_t
               | Eventually of formula_t
               | True_L
               | False_L

(** NTCC Process Syntax *)
type ntcc_process_t = Tell of constraint_t
                    | Choice of (constraint_t * ntcc_process_t) list
                    | Parallel of ntcc_process_t * ntcc_process_t
                    | Next of ntcc_process_t
                    | Unless of constraint_t * ntcc_process_t
                    | Star of ntcc_process_t
                    | Bang of ntcc_process_t
                    | Skip

(** Parsed NTCC program *)
type ntcc_program = Some of ntcc_process_t | Empty

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

(*
(** pretty representation of the symbolic representation *)
type pretty_state = (state_value * int) list

module Node = struct
  type t = string
  let compare = Pervasives.compare
  let hash  = Hashtbl.hash
  let equal = (=)
end

module G = Graph.Persistent.Digraph.ConcreteBidirectional(Node)

let colors = [| 32768; 15771652; 44799; 7362495 |]

module Dot = Graph.Graphviz.Dot (struct
  include G
  let graph_attributes _ = []
  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = (if v = "" then [`Style `Invis] else [`Shape `Circle; `Style `Filled; `Fillcolor colors.(Random.int (Array.length colors))])
  let vertex_name v = "\""^v^"\""
  let default_vertex_attributes _ = []
end) *)
