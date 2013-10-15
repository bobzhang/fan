
type t = Locf.t -> string -> unit


let emit p msg =
  Printf.eprintf "File \"%s\", line %d, character %d:\n\
            %s\n" p.Lexing.pos_fname p.Lexing.pos_lnum
  (p.Lexing.pos_cnum - p.Lexing.pos_bol) msg

(* emit Emacs frienly error message
   {[
   Fan_warnings.emitf p  "Reference to unbound regexp name `%s'" x ;
   ]}
   Don't add "@." to the fmt, otherwise the order is wrong
 *)    
let emitf p fmt = Format.ksprintf (emit p) fmt     

let default loc txt = Format.eprintf "<W> %a: %s@." Locf.print loc txt
    
let current = ref default
    
let print loc txt = !current loc txt;;

(* local variables: *)
(* compile-command: "pmake fan_warnings.cmo" *)
(* end: *)
