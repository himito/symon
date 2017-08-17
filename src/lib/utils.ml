(** This module contains auxiliary functions *)

open Types

(** Builds a list of integers from [i] to [n-1] *)
let rec build_list (i:int) (n:int) : 'i list =
  if i < n then
    i::(build_list (i+1) n)
  else
    []

(** Unfolds [Next^(i)] constructor *)
let rec unfold_next (ntcc_proc:ntcc_process_t) (i:int) : ntcc_process_t =
  match i with
  | 0 -> ntcc_proc
  | _ -> unfold_next (Next ntcc_proc) (i-1)

(** Propagates a [Next] operator inside a formula *)
let rec distribute_next (f:formula_t) : formula_t =
  match f with
  | Or_L (a,b) -> Or_L ((distribute_next a), (distribute_next b))
  | And_L (a,b) -> And_L ((distribute_next a), (distribute_next b))
  | _ -> Next_L f

(** Returns the string representation of a constraint *)
let rec string_of_constraint (x:constraint_t) : string =
  match x with
  | Atomic c -> c
  | And_C (c1, c2) -> Printf.sprintf "%s ^ %s" (string_of_constraint c1) (string_of_constraint c2)
  | False_C -> "false"
  | True_C -> "true"

(** Returns the string representation of a ntcc process *)
let rec string_of_process (p:ntcc_process_t) : string =
  let string_of_choice (c,p) =
    Printf.sprintf "when (%s) do %s" (string_of_constraint c) (string_of_process p)
  in
  match p with
  | Tell c -> Printf.sprintf "tell(%s)" (string_of_constraint c)
  | Parallel (p1, p2) -> Printf.sprintf "%s || %s" (string_of_process p1) (string_of_process p2)
  | Next p -> Printf.sprintf "next(%s)" (string_of_process p)
  | Star p -> Printf.sprintf "*(%s)" (string_of_process p)
  | Bang p -> Printf.sprintf "!(%s)" (string_of_process p)
  | Unless (c, p) -> Printf.sprintf "unless (%s) %s" (string_of_constraint c) (string_of_process p)
  | Choice l -> Printf.sprintf "{ %s }" (String.concat " + " (List.map string_of_choice l))
  | Skip -> "skip"

(** Returns the string representation of a formula *)
let rec string_of_formula (f:formula_t) : string =
  match f with
  | Constraint c -> string_of_constraint c
  | Negation f -> "¬" ^ (string_of_formula f)
  | And_L (f1, f2) -> Printf.sprintf "(%s ^ %s)" (string_of_formula f1) (string_of_formula f2)
  | Or_L (f1,f2) -> Printf.sprintf "[%s] v [%s]" (string_of_formula f1) (string_of_formula f2)
  | Next_L f1 -> Printf.sprintf "o(%s)" (string_of_formula f1)
  | _ -> failwith "function string_of_formula does not support the constructor"

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
      | And_L (c, Cons True) -> c
      | _ -> str
      in
      And_L ((unfold_nextL (state2ctr(prep)) n), k)
    )
    (Cons True)
    s
    in
    match r with
    | And_L (c, Cons True) -> c
    | _ -> r
  in
  let r2 = List.fold_left
    (fun str lAnd ->
      let k = match str with
        | Or_L (c, Cons False) -> c
        | _ -> str
        in
        Or_L ((state2and lAnd),k)
    )
    (Cons False)
    betterF
  in
  match r2 with
    | Or_L (c, Cons False) -> c
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
      | And_L (c, Cons False) -> c
      | _ -> str
      in
      And_L ((unfold_nextL (state2ctr(prep)) n), k)
    )
    (Cons False)
    s
    in
    r
    (* match r with *)
    (* | And_L (c, Cons True) -> c *)
    (* | _ -> r *)
  in


  let r2 = List.fold_left
    (fun str lAnd ->
      let k = match str with
        | Or_L (c, Cons False) -> c
        | _ -> str
        in
        Or_L ((state2and lAnd),k)
    )
    (Cons False)
    betterF
  in
  match r2 with
    | Or_L (c, Cons False) -> c
    | _ -> r2

let printBetter l =
  List.iter (fun l2 -> List.iter (fun (k,i) ->  print_string ("o^"^(string_of_int i)^"("^(string_of_atom k)^")")) l2; print_string "\n") l;
  print_string ("\n\n"); *)
