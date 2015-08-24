open Types
open Auxiliar



(* Boilerplate code for calling OCaml in the worker thread. *)
let js_object = Js.Unsafe.variable "Object"
let js_handler = jsnew js_object ()
let postMessage = Js.Unsafe.variable "postMessage"

let log s = ignore (Js.Unsafe.call postMessage (Js.Unsafe.variable "self")
		      [|Js.Unsafe.inject (Js.string s)|])

let onmessage event =
  let fname = event##data##fname in
  let args = event##data##args in
  let handle = Js.Unsafe.get js_handler fname in
  let result = Js.Unsafe.fun_call handle (Js.to_array args) in
  let response = jsnew js_object () in
  Js.Unsafe.set response (Js.string "fname") fname;
  Js.Unsafe.set response (Js.string "result") result;
  Js.Unsafe.call postMessage (Js.Unsafe.variable "self") [|Js.Unsafe.inject response|]

let _ = Js.Unsafe.set (Js.Unsafe.variable "self") (Js.string "onmessage") onmessage


(* compilation *)

let process_of_string s = Parser.main Lexer.lex (Lexing.from_string s)

(* let _ = print_endline (printProcess (process_of_string "!(Tell (error ^ stop ^ false)) || Next (Tell (hola)) || Unless {error : *(Tell (hola))}")) *)

(* let program = "!(When {signal : Next (Tell (on))} || Unless {signal : Tell (off)})" *)


(* let _ = print_endline ("NTCC Process : \n"^program^"\n") *)

(* let _ = print_endline "LTS Generated :" *)

(* let symbolic_model = buildSymbolicModel ntcc_program *)

let js_symbolicNTCC s =
  log ("computing symbolic model of " ^ (Js.to_string s));
  let ntcc_program = process_of_string (Js.to_string s) in 
  Js.string (printProcess ntcc_program)

let _ = Js.Unsafe.set js_handler (Js.string "symbolicNTCC") (Js.wrap_callback js_symbolicNTCC)

