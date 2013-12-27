open Astf


(* let antiquot_exp = Gramlib.eoi_entry Syntaxf.exp  *)
(* let antiquot_pat = Gramlib.eoi_entry Syntaxf.pat *)
;;
%create{ep};;
%extend{
ep: [Lid x %ep{$lid:x}]
};;    
(* let antiquot_ident = Gramlib.eoi_entry Syntaxf.ident *)


(* let antiquot_ep = Gramlib.eoi_entry ep *)

let exp  loc str =
  Gramlib.parse_string_eoi   Syntaxf.exp (* antiquot_exp *) ~loc str

let pat loc str =
  Gramlib.parse_string_eoi  Syntaxf.pat (* antiquot_pat *) ~loc str

let stru  loc str =
  Gramlib.parse_string_eoi   Syntaxf.stru (* antiquot_exp *) ~loc str
    
(* FIXME -- to be improved *)
let ep  loc str : ep =
  Gramlib.parse_string_eoi  ep  ~loc str 
    
let ident  loc str =
  Gramlib.parse_string_eoi Syntaxf.ident(* antiquot_ident *) ~loc str


let expand_exp  (x:Tokenf.quot)  =
  if x.name = Tokenf.empty_name then
    let expander loc _ s =
      exp  loc s in
    Tokenf.quot_expand expander x
  else
    Ast_quotation.expand x Dyn_tag.exp

let expand_stru (x:Tokenf.quot) =
  if x.name = Tokenf.empty_name then
    let expander loc _ s =
      stru  loc s in
    Tokenf.quot_expand expander x
  else
    Ast_quotation.expand x Dyn_tag.stru

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/parsef.cmo" *)
(* end: *)
