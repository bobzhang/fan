open Formula

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

(* The NNF conversion and registration in JS. *)
let formula_of_string s = Parser.parse_formula Lexer.lex (Lexing.from_string s)
let js_nnf s = 
  log ("computing nnf of " ^ (Js.to_string s));
  Js.string (str (nnf (formula_of_string (Js.to_string s))))

let _ = Js.Unsafe.set js_handler (Js.string "nnf") (Js.wrap_callback js_nnf)
    
