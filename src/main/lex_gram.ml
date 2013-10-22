(** lexing gram *)  
%regex{ (** FIXME remove duplication later see lexing_util.cmo *)
let newline = ('\010' | '\013' | "\013\010")
let ocaml_blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let lident = lowercase identchar *    
let ident = (lowercase|uppercase) identchar*
let quotation_name = '.' ? (uppercase  identchar* '.') *
    (lowercase (identchar | '-') * )
let antifollowident =   identchar +       
let locname = ident
let hexa_char = ['0'-'9' 'A'-'F' 'a'-'f']
let ocaml_escaped_char =
  '\\'
    (['\\' '"' 'n' 't' 'b' 'r' ' ' '\'']
     | ['0'-'9'] ['0'-'9'] ['0'-'9']
     |'x' hexa_char hexa_char)
let ocaml_char =
  ( [^ '\\' '\010' '\013'] | ocaml_escaped_char)
let ocaml_lid =  lowercase identchar *
let ocaml_uid =  uppercase identchar * 
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
  lex_antiquot
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


let  rec token : Lexing.lexbuf -> (Ftoken.t * Locf.t ) = %lex{
  | newline %{
    begin
      update_loc  lexbuf;
      (`NEWLINE, !! lexbuf )
    end}
  | ocaml_lid as x %{let loc =  !! lexbuf in (`Lid (loc,x), loc)}
  | ocaml_uid as x  %{let loc = !! lexbuf in (`Uid (loc,x), loc)}      
  | '"' %{
    let c = new_cxt ()  in
    let old = lexbuf.lex_start_p in
    begin
      push_loc_cont c lexbuf lex_string;
      (`Str (buff_contents c), old --  lexbuf.lex_curr_p )
    end}
  | "'" (newline as x) "'" %{
    begin
      update_loc   lexbuf ~retract:1;
      let loc = !! lexbuf in
      (`Chr (loc,x), loc)
    end}
  | "'" (ocaml_char as x ) "'" %{ let loc =  !! lexbuf in (`Chr (loc,x) , loc )}
  | "'\\" (_ as c) %{err (Illegal_escape (String.make 1 c)) @@ !! lexbuf}
  | "#" | "|" | "^" | "<" | "->" |"="  |"_" | "*" | "["
  |"]" | "*" | "?" | "+" | "(" | ")" | "-" as x %{(`Sym x, !! lexbuf)}
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
         | Some name -> Ftoken.name_of_string name
         | None -> Ftoken.empty_name  in
       begin
         let old = lexbuf.lex_start_p in
         let content =
           begin
             store c lexbuf;
             push_loc_cont c lexbuf lex_quotation;
             buff_contents c
           end in
         let loc = old -- lexbuf.lex_curr_p in
         let shift = String.length shift in
         let retract = (* 2 *) 1  in
         (`Quot{Ftoken.name;meta;shift;content;loc;retract} ,loc)
       end}
  | '$' %{
       let  dollar (c:Lexing_util.context) =
         %lex{
         | ('`'? (identchar*|['.' '!']+) as name) ':' (antifollowident as x) %{
             begin
               let old =
                 let v = lexbuf.lex_start_p in
                 { v with pos_cnum = v.pos_cnum + String.length name + 1} in
               (`Ant(name,x), old -- lexbuf.lex_curr_p)
             end}
         | lident as x  %{ (`Ant("",x), !!lexbuf)}  (* $lid *)
         | '(' ('`'? (identchar*|['.' '!']+) as name) ':' %{
            (* $(lid:ghohgosho)  )
               the first char is faked '(' to match the last ')', so we mvoe
               backwards one character *)
             let old =
               let v = List.hd c.loc in
               {v with pos_cnum = v.pos_cnum + 1+1+1+String.length name - 1} in
             begin
               c.buffer +> '(';
               push_loc_cont c lexbuf lex_antiquot;
               (`Ant(name,buff_contents c),
                old -- Lexing.lexeme_end_p lexbuf)
             end}
         | '(' %{     (* $(xxxx)*)
             let old =
               let v = List.hd c.loc in
               {v with pos_cnum = v.pos_cnum + 1 + 1 - 1 } in
             begin
               c.buffer +> '(';
               push_loc_cont c lexbuf lex_antiquot;
               (`Ant("", buff_contents c ), old -- Lexing.lexeme_end_p lexbuf)
             end}
         | _ as c %{err (Illegal_character c) (!! lexbuf) } }in
       let c = new_cxt () in
       if  !Configf.antiquotations then  (* FIXME maybe always lex as antiquot?*)
         push_loc_cont c lexbuf  dollar
       else err Illegal_antiquote (!! lexbuf) }

  | eof %{
      let pos = lexbuf.lex_curr_p in (* FIXME *)
      (lexbuf.lex_curr_p <-
      { pos with pos_bol  = pos.pos_bol  + 1 ;
        pos_cnum = pos.pos_cnum + 1 };
     (`EOI, !!lexbuf ))}
    
  | _ as c %{ err (Illegal_character c) @@  !!lexbuf }}
    

let from_lexbuf lb = Fstream.from (fun _ -> Some (token lb))

let from_stream (loc:Locf.t) strm =
  let lb = Lexing.from_function (lexing_store strm) in begin
    lb.lex_abs_pos <- loc.loc_start.pos_cnum;
    lb.lex_curr_p <- loc.loc_start;
    from_lexbuf  lb
  end

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lex_gram.cmo" *)
(* end: *)




