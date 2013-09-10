
type t = {
    is_kwd : string -> bool;
      mutable filter : FToken.filter;
  }

open LibUtil

let mk ~is_kwd ={
  is_kwd ;
  filter = FToken.ignore_layout
}
    
let filter x =
  let f (tok, loc) = 
    let tok = FToken.keyword_conversion tok x.is_kwd in begin 
      FToken.check_keyword_as_label tok loc x.is_kwd ;
      (* if !error_on_unknown_keywords  then *)
      (*   check_unknown_keywords tok loc *)
      (* else (); *)
      (* debug token "@[<hov 2>Lexer before filter:@ %a@ at@ %a@]@." *)
      (*   print tok FLoc.dump loc in *)
      (tok, loc)
    end in
  (* let rec filter = parser *)
  (*   [ [< (tok, loc); 's >] -> [< f tok loc; '(filter s) >] *)
  (*   | [< >] -> [< >] ] in *)
  (* let rec tracer = (\* FIXME add a debug block construct *\) parser *)
  (*   [ [< ((_tok, _loc) as x); 'xs >] -> *)
  (*       debug token "@[<hov 2>Lexer after filter:@ %a@ at@ %a@]@." *)
  (*                   print _tok FLoc.dump _loc in *)
  (*       [< x; 'tracer xs >] *)
  (*   | [< >] -> [< >] ] *)
  (* in fun strm -> tracer (x.filter (filter strm)); *)
  fun strm -> x.filter (XStream.map f strm)

let set_filter x f = x.filter <- f x.filter



