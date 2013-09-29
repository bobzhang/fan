

(* module FanLexer =FanLexer.Make (FanToken) *)
    
(*
  let _ = FanLexer.debug_from_string "\"\\$\""
  This is an error test case, actually
 *)
let _ = FanLexer.debug_from_string "\"\\\\$\""

(*
  now the warning is removed, if un-escaped, then we can write this way:
  <<<<\$>>>>
 *)  


















