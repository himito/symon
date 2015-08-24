(**
 This modules contains the definition of a constraint, a ntcc process, a logic formula and a lts
*)
open Graph

(** constraint *)
type constr = Atomic of string
              | And of constr * constr
              | True
              | False

(** ntcc process *)
type ntcc_proc = Tell of constr
                | Choice of (constr * ntcc_proc) list
                | Parallel of ntcc_proc * ntcc_proc
                | Next of ntcc_proc
                | Unless of constr * ntcc_proc
                | Star of ntcc_proc
                | Bang of ntcc_proc
                | Skip

type ntcc_program = Some of ntcc_proc | Empty

(** symbolic representation *)
type logic = Cons of constr
            | And_L of logic * logic
            | Or_L of logic * logic
            | Next_L of logic
            | Abs of constr

(** values of a lts state *)
type state_value = Cons_S of constr | Abs_S of constr

(** pretty representation of the symbolic representation *)
type pretty_state = (state_value * int) list

(** lts state *)
type state = {mutable values: state_value list;
              mutable next_transitions : int list;
              }


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
end)
