(**
 This modules contains auxiliary functions
 *)

open Types

(** function that builds a list of integers from i to n-1 *)
let rec buildList (i:int) (n:int) =
  if i < n then
    i::(buildList (i+1) n)
  else
    []

(** function that unfolds Next^(index) process *)
let rec unfoldNext (proc_ntcc:ntcc_proc) (index:int) =
  match index with
  | 0 -> proc_ntcc
  | _ -> unfoldNext (Next proc_ntcc) (index-1)

(** function that unfolds parallel *)
let unfoldParallel f l =
  List.fold_left
    (fun p i ->
      match p with
      | Parallel (Skip,b) -> Parallel (b,(f i))
      | _ -> Parallel (p,(f i))
    )
    Skip l

(** function that unfolds choice *)
let unfoldChoice c f l =
  Choice (List.map (fun i -> (c,f i)) l)

(** function that adds a next operator to a formule *)
let rec distributeNext (f:logic) =
  match f with
  | Or_L (a,b) -> Or_L ((distributeNext a), (distributeNext b))
  | And_L (a,b) -> And_L ((distributeNext a), (distributeNext b))
  | _ -> Next_L f

(** function that converts a constraint into a string *)
let rec string_of_constraint (x:constr) =
  match x with
  | Atomic c -> c
  | And (c1,c2) -> (string_of_constraint c1)^" ^ "^(string_of_constraint c2)
  | False -> "false"
  | True -> "true"

(** function that converts a ntcc process into a string *)
let rec string_of_process (p:ntcc_proc) =
  let string_of_choice (c,p) =
    "when ("^(string_of_constraint c)^") do ("^(string_of_process p)^")"
  in
  match p with
  | Tell c -> "tell ("^(string_of_constraint c)^")"
  | Parallel (p1,p2) -> (string_of_process p1)^" || "^(string_of_process p2)
  | Next p -> "next ("^(string_of_process p)^")"
  | Star p -> "* ("^(string_of_process p)^")"
  | Bang p -> "! ("^(string_of_process p)^")"
  | Unless (c,p) -> "unless ("^(string_of_constraint c)^") Next ("^(string_of_process p)^")"
  | Choice l -> "{ "^(String.concat " + " (List.map string_of_choice l))^" }"
  | Skip -> "skip"

(** function that converts a state of a lts into a string *)
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

(** function that prints the symbolic formula *)
let rec string_of_formula f =
  match f with
    | Cons c -> string_of_constraint c
    | Abs c -> "¬"^(string_of_constraint c)
    | And_L (f1,f2) -> (string_of_formula f1)^" ^ "^(string_of_formula f2)
    | Or_L (f1,f2) -> "["^(string_of_formula f1)^"]"^" v "^"["^(string_of_formula f2)^"]"
    | Next_L f1 -> "o"^(string_of_formula f1)


(** function that unfolds Next^(index) process *)
let rec unfoldNextL const index =
  match index with
  | 0 -> const
  | _ -> unfoldNextL (Next_L const) (index-1)


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
      And_L ((unfoldNextL (state2ctr(prep)) n), k)
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
      And_L ((unfoldNextL (state2ctr(prep)) n), k)
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
  print_string ("\n\n");
