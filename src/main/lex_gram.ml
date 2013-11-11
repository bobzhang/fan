(** lexing gram *)  
%regex{ (** FIXME remove duplication later see lexing_util.cmo *)
let lident = lowercase identchar *    
let quotation_name = '.' ? (uppercase  identchar* '.') *
    (lowercase (identchar | '-') * )
let antifollowident =   identchar +       
let locname = ident
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] hexa_char ['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal

    
};;


(*************************************)
(*    local operators                *)    
(*************************************)        
let (++) = Buffer.add_string       
let (+>) = Buffer.add_char
(** get the location of current the lexeme *)
let (!!)  = Location_util.from_lexbuf ;;

(* let opt_char_len = Lexing_util;; *)
%import{
Lexing_util:
  update_loc
  new_cxt
  push_loc_cont
  pop_loc
  lex_string
  lex_comment
  lex_quotation
  buff_contents
  err
  warn
  move_curr_p
  store
  lexing_store
  with_store
  ;
Location_util:
   (--)
   ;
};;    


let  rec token = %lex_fan{
  | newline  %{
    begin
      update_loc  lexbuf;
      token lexbuf 
    end}
  | ocaml_lid as txt %{ `Lid {loc =  !! lexbuf ; txt}}
  | ocaml_uid as txt  %{ `Uid {loc = !! lexbuf ; txt}}      
  | '"' %{
    let c = new_cxt ()  in
    let old = lexbuf.lex_start_p in
    begin
      push_loc_cont c lexbuf lex_string;
      let loc = old --  lexbuf.lex_curr_p in
      `Str {loc;txt=buff_contents c}
    end}
  | int_literal as txt %{`Int{loc = !!lexbuf; txt}}
  | "'" (newline as txt) "'" %{
    begin
      update_loc   lexbuf ~retract:1;
      let loc = !! lexbuf in
      `Chr {loc;txt}
    end}
  | "'" (ocaml_char as txt ) "'"
      %{ let loc =  !! lexbuf in `Chr {loc;txt}}
  | "'\\" (_ as c) %{err (Illegal_escape (String.make 1 c)) @@ !! lexbuf}

  | "#" | "|" | "^" | "<" | "->" |"="  |"_" | "*" | "["
  |"]" | "*" | "?" | "+" | "(" | ")" | "-" | ":" | "@" |"{" | "}"
  |";" |"." | "," as txt %{
    let loc = !!lexbuf in `Sym {loc;txt}}
  | ocaml_blank + %{ token lexbuf }

  | "(*"(')' as x) ? %{
    let c = new_cxt () in
    begin
      if x <> None then warn Comment_start (!! lexbuf);
      store c lexbuf;
      push_loc_cont c lexbuf lex_comment;
      token lexbuf 
    end}
  | '%'  (quotation_name as name) ? ('@' (locname as meta))? "{"    as shift %{
       let c = new_cxt () in
       let name =
         match name with
         | Some name -> Tokenf.name_of_string name
         | None -> Tokenf.empty_name  in
       begin
         let old = lexbuf.lex_start_p in
         let txt =
           begin
             store c lexbuf;
             push_loc_cont c lexbuf lex_quotation;
             buff_contents c
           end in
         let loc = old -- lexbuf.lex_curr_p in
         let shift = String.length shift in
         let retract =  1  in
         `Quot{Tokenf.name;meta;shift;txt;loc;retract}
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

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lex_gram.cmo" *)
(* end: *)




