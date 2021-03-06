(**
  This is the main module. It parses the ntcc program into the defined structure and generates its symbolic model.
  @author Jaime Arias (ariasalmeida@gmail.com)
  @version 1.0
 *)

open Types
open Lexer
open Auxiliar
open Symbolic

(** function that parses the ntcc process *)
let process_of_string s =
  Parser.main Lexer.lex (Lexing.from_string s)

(** function that parses the ntcc program **)
let process_of_file =
  let filename = Sys.argv.(1) in
  Parser.main Lexer.lex (Lexing.from_channel (open_in filename))

let system = match process_of_file  with Some p -> p | Empty -> print_endline "Empty file"; exit(1)


(* Generate the symbolic model *)
let ntcc_program = system
let symbolic_model = buildSymbolicModel ntcc_program

let _ = print_endline ("NTCC Process : \n"^(string_of_process ntcc_program)^"\n")
let _ = print_endline ("Formula : \n"^(printLogic symbolic_model)^"\n")

let getLatex model  =
  let getAtomicLatex a =
    let rec getConstraintLatex c =
      match c with
      | Atomic a -> let l = Str.split (Str.regexp_string "_") a  in (List.hd l)^"_{"^(List.hd (List.rev l))^"}"
      | And (a,b) -> (getConstraintLatex a)^" \\wedge "^(getConstraintLatex b)
      | True -> "\\mathtt{true}"
      | False -> "\\mathtt{false}"
    in
    let getNextLatex n =
      if n > 0 then
        "\\circ^{"^(string_of_int n)^"}"
      else
        ""
    in
    match a with
    | (Cons_S c,n) -> (getNextLatex n)^"("^(getConstraintLatex c)^")"
    | (Abs_S c,n) -> (getNextLatex n )^"(\\neg("^(getConstraintLatex c)^"))"
  in
  let getSubFormula s =
    List.fold_left (fun str a -> if str <> "" then (getAtomicLatex a)^" \\wedge "^str else (getAtomicLatex a) ) "" s
  in
  let formula = getBetter model 0 in
  List.fold_left (fun str f -> if str <> "" then "\\{"^(getSubFormula f)^"\\} \\vee "^str else "\\{"^(getSubFormula f)^"\\}" ) "" formula

let _ = print_endline (getLatex symbolic_model)


let final_lts = newLTS 300
let _ = List.iter (addFormula2LTS final_lts) (List.map completeFormula2 (getBetter symbolic_model 0)) (* ver aquí *)

(* let _ = print_endline "LTS Generated :" *)
(* let _ = printLTS final_lts *)



let _ =
let g = G.empty in
let st = Array.make (Hashtbl.length final_lts) "" in
let _ = Hashtbl.iter (fun k v -> Array.set st k (Str.global_replace (Str.regexp_string " ^ ") "\n" (string_of_state v.values))) final_lts in
let st = Array.map G.V.create st in
let g = Array.fold_left (G.add_vertex) g (Array.sub st 1 ((Array.length st)-2)) in



(* let g = Hashtbl.fold (fun k v i -> if (k > 0) then (List.fold_left (fun i1 v1 -> G.add_edge i1 st.(k) st.(v1) ) i v.next_transitions) else i ) final_lts g in *)
let g = Hashtbl.fold (fun k v i -> (List.fold_left (fun i1 v1 -> G.add_edge i1 st.(k) st.(v1) ) i v.next_transitions)) final_lts g in

let file = open_out "output-lts.dot" in
  let _ = Dot.output_graph file g in
  close_out file

let _ = Sys.command "dot -Tpdf output-lts.dot -o output-lts.pdf"
