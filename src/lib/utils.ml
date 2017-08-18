(** This module contains auxiliary functions *)

(** Builds a list of integers from [i] to [n-1] *)
let rec build_list (i:int) (n:int) : 'i list =
  if i < n then
    i::(build_list (i+1) n)
  else
    []


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


(*
(* function that converts a state of a lts into a string *)
let string_of_atom s =
  match s with
    | Cons_S c -> string_of_constraint c
    | Abs_S c -> "¬"^(string_of_constraint c)

let rec string_of_state x =
  (String.concat " ^ " (List.map string_of_atom x))

(** function that prints a LTS *)
let printLTS lts =
  let n = Hashtbl.length lts in
  for i= 0 to (n-1) do
    let state = Hashtbl.find lts i in
    let f = state.values in
    let next_state = state.next_transitions in
    print_string ("state "^(string_of_int i)^" : ");
    print_string ("("^(string_of_state f)^")");
    print_string (" ; next_states: [");
    print_string (String.concat ", " (List.map string_of_int next_state));
    print_endline "]"
  done

let states2logic betterF =
  let state2ctr c =
    match c with
      | Cons_S x -> Cons x
      | Abs_S x -> Abs x
  in
  let state2and s =
    let r = List.fold_left
    (fun str (prep,n) ->
      let k = match str with
      | And (c, Cons True) -> c
      | _ -> str
      in
      And ((unfold_nextL (state2ctr(prep)) n), k)
    )
    (Cons True)
    s
    in
    match r with
    | And (c, Cons True) -> c
    | _ -> r
  in
  let r2 = List.fold_left
    (fun str lAnd ->
      let k = match str with
        | Or (c, Cons False) -> c
        | _ -> str
        in
        Or ((state2and lAnd),k)
    )
    (Cons False)
    betterF
  in
  match r2 with
    | Or (c, Cons False) -> c
    | _ -> r2

let states2logicK betterF =
  let state2ctr c =
    match c with
      | Cons_S x -> Cons x
      | Abs_S x -> Abs x
  in
  let state2and s =
    let r = List.fold_left
    (fun str (prep,n) ->
      let k = match str with
      | And (c, Cons False) -> c
      | _ -> str
      in
      And ((unfold_nextL (state2ctr(prep)) n), k)
    )
    (Cons False)
    s
    in
    r
    (* match r with *)
    (* | And (c, Cons True) -> c *)
    (* | _ -> r *)
  in


  let r2 = List.fold_left
    (fun str lAnd ->
      let k = match str with
        | Or (c, Cons False) -> c
        | _ -> str
        in
        Or ((state2and lAnd),k)
    )
    (Cons False)
    betterF
  in
  match r2 with
    | Or (c, Cons False) -> c
    | _ -> r2

let printBetter l =
  List.iter (fun l2 -> List.iter (fun (k,i) ->  print_string ("o^"^(string_of_int i)^"("^(string_of_atom k)^")")) l2; print_string "\n") l;
  print_string ("\n\n"); *)
