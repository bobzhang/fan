
let () = 
  let in_chan = open_in Sys.argv.(1) in
  let lexbuf =  (Lexing.from_channel in_chan) in
  let () = Location.init lexbuf Sys.argv.(1) in
  begin 
    Printast.implementation Format.std_formatter 
    @@ Parse.implementation lexbuf;
    close_in in_chan
  end



(* local variables: *)
(* compile-command: "ocamlopt.opt -I +compiler-libs ocamlcommon.cmxa dparsetree.ml -o dparsetree.native" *)
(* end: *)


