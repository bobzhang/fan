


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

