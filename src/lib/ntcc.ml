(** Module implementing the process calculus NTCC *)

open Constraint

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

(** Returns the string representation of a ntcc process *)
let rec string_of_process (p:ntcc_process_t) : string =
  let string_of_choice (c,p) =
    Printf.sprintf "when (%s) do %s" (string_of_constraint c) (string_of_process p)
  in
  match p with
    Tell c -> Printf.sprintf "tell(%s)" (string_of_constraint c)
  | Parallel (p1, p2) -> Printf.sprintf "%s || %s" (string_of_process p1) (string_of_process p2)
  | Next p -> Printf.sprintf "next(%s)" (string_of_process p)
  | Star p -> Printf.sprintf "*(%s)" (string_of_process p)
  | Bang p -> Printf.sprintf "!(%s)" (string_of_process p)
  | Unless (c, p) -> Printf.sprintf "unless (%s) next(%s)" (string_of_constraint c) (string_of_process p)
  | Choice l -> Printf.sprintf "{ %s }" (String.concat " + " (List.map string_of_choice l))
  | Skip -> "skip"

(** Unfolds [Next^(i)] constructor *)
let rec unfold_next (ntcc_proc:ntcc_process_t) (i:int) : ntcc_process_t =
  match i with
    0 -> ntcc_proc
  | _ -> unfold_next (Next ntcc_proc) (i-1)

