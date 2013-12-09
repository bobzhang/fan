Re.get_ofs (Re.exec (Re_perl.compile @@ Re_perl.re "a(a)*") "bbaaaa" ) 0;;

s/a(a)/


let (u,v) = Lexgen.make_single_dfa {shortest=false; clauses = [ (%re{'a'},0)]};;

let s = match v with [| Lexgen.Shift (No_remember, s); b|] -> s;;

Array.length s;;
- : int = 257

s.(Char.code 'a');;
- : Lexgen.automata_move * Lexgen.memory_action list = (Lexgen.Goto 1, [])  
