%regex{ (** FIXME remove duplication later see lexing_util.cmo *)
let newline = ('\010' | '\013' | "\013\010")
let ocaml_blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let lident = lowercase identchar *    
let ident = (lowercase|uppercase) identchar*
let ocaml_lid =  lowercase identchar *
let ocaml_uid =  uppercase identchar *
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let int_literal =  decimal_literal 
};;


%import{
Lexing_util:
  update_loc
  lex_comment
  lexing_store
  new_cxt
  push_loc_cont
  pop_loc
  lex_string
  buff_contents
  err
  warn
  move_curr_p
  store
  with_store
  ;
Location_util:
  (--)
  from_lexbuf as (!!)
   ;
};;    



let  rec token : Lexing.lexbuf -> Tokenf.t = %lex{
  | newline  %{
    begin
      update_loc  lexbuf;
      token lexbuf 
    end}
  | ocaml_lid as txt %{ `Lid {loc =  !! lexbuf ; txt}}
  | ocaml_uid as txt  %{ `Uid {loc = !! lexbuf ; txt}}      
  | int_literal as txt %{`Int{loc = !!lexbuf; txt}}
  | "."|":"|"-"|","|"("|")"|"="|
    "\\="|"=:="|"=\\="|"<"|
    "=<"|">"|">="|"+"| "_"|"!"|
    "["|"]"|"|"|"%:"|"?"|":-" as txt %{`Sym {loc = !!lexbuf;txt}}
  | ocaml_blank + %{ token lexbuf }
  | "(*"(')' as x) ? %{
    let c = new_cxt () in
    begin
      if x <> None then warn Comment_start (!! lexbuf);
      store c lexbuf;
      push_loc_cont c lexbuf lex_comment;
      token lexbuf 
    end}
  | eof %{
      let pos = lexbuf.lex_curr_p in (* FIXME *)
      (lexbuf.lex_curr_p <-
      { pos with pos_bol  = pos.pos_bol  + 1 ;
        pos_cnum = pos.pos_cnum + 1 };
       let loc = !!lexbuf in
       `EOI {loc;txt=""})}
    
  | _ as c %{ err (Illegal_character c) @@  !!lexbuf }}

let from_lexbuf lb = Streamf.from (fun _ -> Some (token lb))

let from_stream (loc:Locf.t) strm =
  let lb = Lexing.from_function (lexing_store strm) in begin
    lb.lex_abs_pos <- loc.loc_start.pos_cnum;
    lb.lex_curr_p <- loc.loc_start;
    from_lexbuf  lb
  end
