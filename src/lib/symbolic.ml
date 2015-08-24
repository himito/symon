(**
 Module that contains the implementation of the symbolic representation of ntcc processes
*)

open Types
open Auxiliar

(** function that returns a new hashtbl of a specific size *)
let newLTS (size:int) =
  let lts = Hashtbl.create size in (* create new lts *)
  Hashtbl.add lts 0 {values=[]; next_transitions = []}; (* initial state 0 *)
  lts

let sortF (formula:pretty_state) =
  List.sort
  (fun (x,n1) (y,n2) ->
    if  n1 = n2 then 0
    else if n1 > n2 then 1
    else -1)
  formula


(** function that completes a formula *)
let completeFormula (f:pretty_state) =
  let fill l =
    let (_,max) = List.hd (List.rev l) in
    let r = ref [] in
    for i = 0 to (max) do
      let elements = List.filter (fun (x,k) -> k=i ) l in
      if (List.length elements) = 0 then
        r := !r@[(Cons_S True,i)]
      else
        r := !r@elements
    done;
    !r
  in
  fill (sortF f)



let completeFormula2 (f:pretty_state) =
  let fill l =
    let (_,max) = List.hd (List.rev l) in
    let r = ref [] in
    for i = 0 to (max) do
      let elements = List.filter (fun (x,k) -> k=i ) l in
      if (List.length elements) = 0 then
        r := !r@[(Cons_S (Atomic ("st_"^(string_of_int (i+1)))),i)]
      else
        r := !r@elements
    done;
    !r
  in
  fill (sortF f)

(* function that builds a LTS from the formula *)
let addFormula2LTS lts formula =

  let getPositive f =
    List.filter (fun x -> match x with Cons_S c -> true | _ -> false) f
  in

  (* Returns the state id that satisfies the constraints *)
  let whichSatisfies lts state_ids cs =
    (* Check if the state 'n' satisfies all constraints 'cs' *)
    let satisfiesState n =
      let state = Hashtbl.find lts n in
      let positive_state_values = getPositive state.values in
      let positive_cs = getPositive cs in
      if (List.length positive_cs) == (List.length positive_state_values) then
        List.for_all (fun c -> List.mem c positive_state_values) positive_cs
      else
        false
    in
    try
      List.find (fun id -> satisfiesState id) state_ids
    with Not_found -> -1
  in

  let (_,degree) = List.nth formula ((List.length formula) - 1) in  (* gets the degree of the formula *)

  let current_state_id = ref 0 in (* current id 0 *)

  for i=0 to degree do

    (* make a list of constraints from the formula of the current state *)
    let constrs = List.map (fun (c,id) -> c) (List.filter (fun (x,d) -> d==i ) formula) in

    let state_satifying_id = whichSatisfies lts (buildList 1 (Hashtbl.length lts)) constrs in (* state's id satisfying the constraints *)

    let current_state = Hashtbl.find lts !current_state_id in (* get current state *)

    if state_satifying_id < 0 then (* create a new state *)
      let new_id = Hashtbl.length lts in
      Hashtbl.add lts new_id {values = constrs; next_transitions =[]}; (* new state created *)
      current_state.next_transitions <- current_state.next_transitions@[new_id];
      current_state_id := new_id
    else if (List.mem state_satifying_id current_state.next_transitions) then (* the transition exists *)
      current_state_id := state_satifying_id (* pass to the next state *)
    else begin (* make a loop *)
      current_state.next_transitions <- current_state.next_transitions@[state_satifying_id];
      current_state_id := state_satifying_id
    end
  done


let formulasInconsistent l1 l2=
  let elementInconsistent l e=
    match e with
    | (Cons_S c, n)-> List.mem ((Abs_S c),n) l
    | (Abs_S c, n) -> List.mem ((Cons_S c),n) l
  in
  List.exists (elementInconsistent l2) l1

let unionFormulas l1 l2 =
  (List.fold_left (fun l e -> if (List.mem e l2) then l else l@[e] ) [] l1)@l2

let cross l1 l2 =
  let new_list = ref [] in
  List.iter (fun e_l1 ->
            List.iter (fun e_l2 ->
              if not(formulasInconsistent e_l1 e_l2) then new_list := !new_list@[(unionFormulas e_l1 e_l2)]
                      )
                      l2
            )
            l1;
  !new_list

let rec getBetter formula n=
  match formula with
  | Cons c -> [[(Cons_S c, n)]]
  | Abs c -> [[(Abs_S c, n)]]
  | Next_L l -> getBetter l (n+1)
  | And_L (a,b) -> cross (getBetter a n) (getBetter b n)
  | Or_L (a,b) -> (getBetter a n)@(getBetter b n)




let eliminateCon c =
  match c with
  | And_L (c1,c2) -> if c1 = Cons True then c2 else if c2 = Cons True then c1 else c
  | Or_L (c1,c2) -> if c1 = Cons False then c2 else if c2 = Cons False then c1 else c
  | _ -> c


let reduceFormula formula =
  let rec addNext c n =
    if n = 0 then c else Next_L ((addNext c (n-1)))
  in
  let r = getBetter formula 0
  in
  let reduction = List.fold_left
    (fun c1 l1 ->
      eliminateCon (Or_L ((eliminateCon c1), (eliminateCon (List.fold_left
                (fun c2 (c3,n) ->
                  match c3 with
                  | Cons_S c -> And_L ((eliminateCon c2), (addNext (Cons c) n ))
                  | Abs_S c -> And_L ((eliminateCon c2), (addNext (Abs c) n))
                )
                (Cons True)
                l1))
            ))
    )
    (Cons False)
    r
  in
  reduction

let rec labeling p_ntcc n =
  let rec dist_n const i =
    match const with
    | Atomic c -> Atomic (c^"_"^(string_of_int i))
    | And (a,b) -> And ((dist_n a i),(dist_n b i))
    | True -> True
    | False -> False
  in
  match p_ntcc with
  | Tell c -> Tell (dist_n c n)
  | Parallel (p,q) -> Parallel ((labeling p n),(labeling q n))
  | Next p -> Next (labeling p (n+1))
  | Star p -> Star (labeling p n)
  | Unless (c,p) -> Unless ((dist_n c n),(labeling p (n+1)))
  | Bang p -> Bang (labeling p n)
  | Choice l -> Choice (List.map (fun (c,p) -> ((dist_n c n),(labeling p n))) l)
  | _ -> Tell (Atomic "Error")



let deadEnd formula =
  let lts = newLTS 300 in
  let _ = List.iter (addFormula2LTS lts) (List.map completeFormula2 (getBetter formula 0)) in
  let cond = ref false in
  let i = ref 1 in
  while (not !cond) && (!i< (Hashtbl.length lts)) do
    let state = Hashtbl.find lts !i in
    cond := (List.length (state.next_transitions)) == 0;
    i := !i + 1
  done;
  !cond

let addDeadEnd formula =
  let generateF n =
    [(Cons_S True,n+1);(Cons_S True,n+2)]
  in
  let better = getBetter formula 0 in
  let newF = List.map
    (fun f ->
      (* get the n of the formula     *)
      let (_,max) = List.hd (List.rev (sortF f)) in
      (* agregar a f la nueva formula *)
      f@(generateF max)
    )
    better in
  states2logic newF




let buildSymbolicModel ntcc_program =
  let copy_t t =
    { values = t.values;
    next_transitions = t.next_transitions }
  in
  let makeCopy lts =
    let new_lts = Hashtbl.create 300 in
    Hashtbl.iter
      (fun k v ->
        Hashtbl.add new_lts k (copy_t v)
      )
      lts;
    new_lts
  in
(* --------------------------------------------------------------------------------------------------------*)
  let rec getGreatestFixPoint s y lts level=
    let new_lts = makeCopy lts in
    let new_y = reduceFormula (if y = Cons True then s else And_L (s, distributeNext y)) in
    (* let _ = print_string ("mirar aqui ->"^(printLogic new_y)^"\n") in *)
    List.iter (addFormula2LTS new_lts) (List.map completeFormula (getBetter new_y 0));
    (* print_string "Anterior\n"; *)
    (* print_endline ("Formula : \n"^(printLogic y)); *)
    (* printLTS lts; *)
    (* print_string "Nuevo\n"; *)
    (* print_endline ("Formula : \n"^(printLogic new_y)); *)
    (* printLTS new_lts; *)
    (* print_string "\n"; *)
    if (lts = new_lts) then
      if (level = 0) then
        begin
          (* print_endline ("FixPoint : \n"^(printLogic y)^"\n"); *)
          y
        end
      else
        begin
          (* print_endline ("FixPoint : \n"^(printLogic new_y)^"\n"); *)
          new_y
        end
    else
      getGreatestFixPoint s new_y new_lts level
  in
(* --------------------------------------------------------------------------------------------------------*)
  let rec getLeastFixPoint s x lts =
    let new_lts = makeCopy lts in
    let new_x = reduceFormula (if x = Cons False then s else Or_L (s, distributeNext x)) in
    List.iter (addFormula2LTS new_lts) (List.map completeFormula (getBetter new_x 0));
    if (lts = new_lts) then
      (* begin *)
        (* printLTS new_lts; *)
       x
      (* end *)
    else
      getLeastFixPoint s new_x new_lts
  in
  let rec getSymbolicModel ntcc_proc  level=
  let choiceSymbolic l level =
    let term_negative (c,p) =
      Abs c
    in
    let term_positive (c,p) =
      And_L (Cons c, (getSymbolicModel p (level+1)))
    in
    let choiceNegative w1 l =
      List.fold_left (fun f w -> And_L (f,term_negative w)) (term_negative w1) l
    in
    let choicePositive w1 l =
      List.fold_left (fun f w -> Or_L (f, term_positive w)) (term_positive w1) l
    in
    let w1 = List.hd l in
    if (List.length l) == 1 then
      Or_L (term_negative w1, term_positive w1)
    else
      let l2 = List.tl l in
      Or_L ((choiceNegative w1 l2),(choicePositive w1 l2))
  in
    match ntcc_proc with
    | Tell c -> Cons c
    | Choice l -> choiceSymbolic l level
    | Parallel (p,q) -> And_L ((getSymbolicModel p level),(getSymbolicModel q level))
    | Next p -> Next_L (getSymbolicModel p level)
    | Unless (c,p) -> Or_L((And_L (Abs c, Next_L (getSymbolicModel p level))), Cons c)
    | Star p ->
        let x0 = Cons False in
        let lts = newLTS 300 in
        let sp = reduceFormula (getSymbolicModel p level) in
        let tmp = List.map completeFormula2 (getBetter sp 0) in
        let sp1 = states2logic tmp in
        (* let _ = print_string ("\nFormula * : "^(printLogic sp1)^"\n") in *)
        let r = getLeastFixPoint sp1 x0 lts in
        (* let _ = print_endline ("\nComplete *: "^(printLogic (r)^"\n")) in *)
        r

    | Bang p ->
        let y0 = Cons True in
        let lts = newLTS 300 in
        let sp = reduceFormula (getSymbolicModel p level) in
        (* let _ = print_string ("\nFormula Antes level("^(string_of_int level)^") ! :"^(printLogic sp)^"\n") in *)
        let sp1 = match p with
                  | Star _ | Bang _ ->
                      sp
                  | _ ->
                      states2logic (List.map completeFormula2 (getBetter sp 0))
        in
        (* let _ = print_string ("\nFormula ! :"^(printLogic sp1)^"\n") in *)
        let r = getGreatestFixPoint sp1 y0 lts level in
        let complete =  states2logicK (List.map completeFormula (getBetter r 0)) in
        (* let _ = print_endline ("Complete : "^(printLogic (complete))^"\n") in *)
        complete
    | _ -> Cons (Atomic "Error")
  in
  let model =  (getSymbolicModel (labeling ntcc_program 1) 0) in
  if (deadEnd model) then addDeadEnd model else model


