open Format


#camlp4o;;

let of_file filename =
  let chan = open_in filename in
  Stream.of_channel chan 


let stream = of_file "_build/camlp4ast_signature.inferred.mli" ;;

open Camlp4.PreCast
(* open Fan_camlp4 *)

(* <:sig_item< type .$typ:type$. >> *)
let f  = wrap_stream_parser Syntax.parse_interf;;  
let a = f  stream;;
