

open Format
include Gstructure
  
include Gentry
include Gstru
    
module Action = Gaction

let default_keywords =
  ["&&"; "functor"; "||"; "private"; "sig"; "include";
   "exception"; "inherit"; "<"; "~"; "and"; 
   "when"; ","; "mod"; "then"; "|]"; "initializer";
   "#";  "!"; "-." ; "_"; ">]" ; "??" ; "in"
     ; "->"; "downto"; "lsr"; "as"; "function"; "begin";
   ".."; ")"; "="; ":"; "|"; "[<"; 
   "class"; "=="; "."; "{<"; "land"; ">}"; "lxor"; "do";
   "end"; "assert"; "external";  "+"; "virtual";
   "to"; "try"; ":>"; "lsl"; "struct"; "else"; "*"; "val"
     ;  "constraint"; "type"; "new"; "of";
   "<-"; "done"; "for"; "&"; ";;"; "{"; "fun"; "method"
     ; "'"; ";"; "mutable"; "lazy"; "["; "}";
   "[|"; "with"; "[^"; "`"; "::"; "]"; "asr"; "[>";
   ":=";  "if"; "while" ; "rec";  "object"; "or"; "-"; "("; "match"
     ; "open"; "module";  "?"; ">"; "let"; "lor"; "["]

let gkeywords = ref (Setf.String.of_list default_keywords)
  

let rec fan_filter = %parser{
  | #Tokenf.space_token; 'xs -> fan_filter xs
  |  x; 'xs  ->
      %stream{ x; ' fan_filter xs }
  |  -> %stream{}}

let rec ignore_layout : Tokenf.filter =
  %parser{
    | #Tokenf.space_token; 'xs -> ignore_layout  xs
    | x ; 'xs  ->
        %stream{x; 'ignore_layout xs }
    | -> %stream{}}
          
let gram =  {
  annot="Fan";
  gfilter =
  { kwds =   Setf.String.of_list default_keywords ;
    filter = fan_filter;  }
}

let filter = FanTokenFilter.filter gram.gfilter
  
let create_lexer ?(filter=ignore_layout) ~annot ~keywords   () = {
  annot;
  gfilter = {
  kwds = Setf.String.of_list keywords;
  filter;  
  }
 }


(* FIXME duplicate some code from Entry *)
let mk f = mk_dynamic gram f

let of_parser name strm = of_parser gram name strm

let get_filter () = gram.gfilter


let token_stream_of_string s =
    lex_string Locf.string_loc s

  
let debug_origin_token_stream (entry:'a t ) tokens : 'a =
  parse_origin_tokens entry tokens
  
let debug_filtered_token_stream entry tokens =
  filter_and_parse_tokens entry tokens 

(* with a special exception handler *)  
let parse_string_safe ?(loc=Locf.string_loc) entry  s =
  try
    parse_string entry ~loc s
  with Locf.Exc_located(loc, e) -> begin
      eprintf "%s" (Printexc.to_string e);
      Locf.error_report (loc,s);
      Locf.raise loc e ;
  end 
;;
    
    
(* let parse_file_with ~rule file  = *)
(*   if Sys.file_exists file then *)
(*     let ch = open_in file in *)
(*     let st = Streamf.of_channel ch in  *)
(*     parse rule (Locf.mk file) st *)
(*   else  failwithf "@[file: %s not found@]@." file; *)
  


(* FIXME [srules] the productions are also scanned  *)  
(* let srules rl = *)
(*     `Stree (List.fold_right Ginsert.add_production   rl DeadEnd) *)

%import{
Gfold:
  sfold0
  sfold1
  sfold0sep
  sfold1sep
  ;
};;    


(* [eoi_entry] could be improved   *)
let eoi_entry entry =
  let open! Gstru in
  let g = gram_of_entry entry in
  let entry_eoi = (mk_dynamic g (name entry ^ "_eoi")) in
  begin
    %extend{ entry_eoi: [  entry{x}; EOI %{x} ] } ;
    entry_eoi
  end

let find_level ?position (entry:Gstructure.entry) =
  match entry.desc with
  | Dparser _ -> invalid_arg "Gramf.find_level"
  | Dlevels levs -> let (_,f,_) = Ginsert.find_level ?position entry levs in f



(*************************************************************************)
(** utilities for parsing *)      
let parse_include_file entry =
  let dir_ok file dir = Sys.file_exists (dir ^ file) in
  fun file ->
    let file =
      try (List.find (dir_ok file) ( "./" :: !Configf.include_dirs )) ^ file
      with | Not_found -> file  in
    let ch = open_in file in
    let st = Streamf.of_channel ch in
      parse entry (Locf.mk file) st
    

let parse_string_of_entry ?(loc=Locf.mk "<string>") entry  s =
  try parse_string entry  ~loc s  with
    Locf.Exc_located(loc, e) -> begin
      eprintf "%s" (Printexc.to_string e);
      Locf.error_report (loc,s);
      Locf.raise loc e ;
  end

let wrap_stream_parser ?(loc=Locf.mk "<stream>") p s =
  try p ~loc s
  with Locf.Exc_located(loc,e) -> begin
    eprintf "error: %s" (Locf.to_string loc) ;
    Locf.raise loc e;
  end 











(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/gramf.cmo" *)
(* end: *)
