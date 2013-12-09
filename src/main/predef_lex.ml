
let named_regexps =
  (Hashtbl.create 13 : (string, Translate_lex.concrete_regexp) Hashtbl.t)


type desc = {
    quot_opt : Tokenf.quot option;
    tokens_opt : Tokenf.txt list option;
    loc : Locf.t
  }    
let named_cases =
  (Hashtbl.create 13 :
     (string,
      (desc -> (Translate_lex.concrete_regexp * Astf.exp) list)) Hashtbl.t )

let _ =
  let (+>) = Hashtbl.add named_regexps in
  begin
    "newline"  +> %re{('\010' | '\013' | "\013\010")};
    "ocaml_blank" +> %re{[' ' '\009' '\012']};
    "lowercase" +> %re{['a'-'z' '\223'-'\246' '\248'-'\255' '_']};
    "uppercase"  +> %re{['A'-'Z' '\192'-'\214' '\216'-'\222']};
    "identchar" +> %re{['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']};

    "eof" +> Eof;
    "_"   +> Characters Fcset.all_chars ;
    
    "hexa_char" +> %re{['0'-'9' 'A'-'F' 'a'-'f']};
    "ident" +> %re{(lowercase|uppercase) identchar*};
    "ocaml_escaped_char" +>
    %re{  '\\' (['\\' '"' 'n' 't' 'b' 'r' ' ' '\'']
       | ['0'-'9'] ['0'-'9'] ['0'-'9']
       |'x' hexa_char hexa_char)};
    "ocaml_char" +> %re{( [^ '\\' '\010' '\013'] | ocaml_escaped_char)};
    "ocaml_lid" +> %re{ lowercase identchar *};
    "ocaml_uid" +> %re{uppercase identchar *};

  "decimal_literal" +> %re{ ['0'-'9'] ['0'-'9' '_']*};
  "hex_literal" +> %re{  '0' ['x' 'X'] hexa_char ['0'-'9' 'A'-'F' 'a'-'f' '_']*};
  "oct_literal" +> %re{  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*};
  "bin_literal" +> %re{ '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*};
  "int_literal" +> %re{decimal_literal | hex_literal | oct_literal | bin_literal};
  "float_literal" +> %re{
   ['0'-'9'] ['0'-'9' '_']*
   ('.' ['0'-'9' '_']* )?
   (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?};
  "quotation_name" +> %re{'.' ? (uppercase  identchar* '.') *
    (lowercase (identchar | '-') * )};
  "identchars" +> %re{identchar+}
   end

let append_quot (y:Tokenf.quot option) (e:Astf.exp)  =
  match y with
  | None -> e
  | Some y -> (* FIXME -- should not need type annot*)
      let a = Parsef.expand_exp y in
      let _loc = y.loc in
       %exp{ begin $e ; $a end}
let _ =
  Hashtblf.add_list named_cases 
  [
  ("ocaml_uid",
    fun {tokens_opt = ls; loc =  _loc; _} ->
      [(%re{ocaml_uid as txt},
        let default = 
          %exp{
          `Uid{loc =
             {loc_start = lexbuf.lex_start_p;
              loc_end = lexbuf.lex_curr_p;
              loc_ghost = false} ; txt }} in
        match ls with
        | None -> 
            default 
        | Some x ->
            let cases  =
             Ast_gen.bar_of_list @@ 
             List.map (*FIXME check the txt is needed ... *)
               ( fun (x:Tokenf.txt) ->
                 let v = x.txt in
                 let i = Hashtbl.hash v in
                 %case{$int':i -> txt = $str:v}) x in
           %exp{
           let v = Hashtbl.hash txt in 
           if (function | $cases | _ -> false) v then
             `Key {loc =
                   {loc_start = lexbuf.lex_start_p;
                    loc_end = lexbuf.lex_curr_p;
                    loc_ghost = false} ; txt }
           else $default}        
       )]);

  ("ocaml_lid",
   fun {tokens_opt = ls; loc =  _loc;_}  ->
     [(%re{ocaml_lid as txt},
       let default =
         %exp{
         `Lid{loc =
              {loc_start = lexbuf.lex_start_p;
               loc_end = lexbuf.lex_curr_p;
               loc_ghost = false} ; txt }} in
       match ls with
       | None -> 
          default 
       | Some x ->
           let cases  =
             Ast_gen.bar_of_list @@ 
             List.map (*FIXME check the txt is needed ... *)
               ( fun (x:Tokenf.txt) ->
                 let v = x.txt in
                 let i = Hashtbl.hash v in
                 %case{$int':i -> txt = $str:v}) x in
           %exp{
           let v = Hashtbl.hash txt in 
           if (function | $cases | _ -> false) v then
             `Key {loc =
                   {loc_start = lexbuf.lex_start_p;
                    loc_end = lexbuf.lex_curr_p;
                    loc_ghost = false} ; txt }
           else $default}
             (* check following actions -- warning?*)
      )])
  ;

   ("kwd_symbol",
   fun {tokens_opt = ls; loc = _loc; _} ->
     match ls with
     | Some ls  -> 
         let regexp =
           Listf.reduce_left_with ~compose:(fun r1 r2 ->
             ((Alternative (r1,r2)):Translate_lex.concrete_regexp))
             ~project:(fun (x:Tokenf.txt) ->
               Translate_lex.regexp_for_string @@ Escape.string x.txt)
             ls in
         [(regexp,
           %exp{
           let txt = Lexing.sub_lexeme lexbuf
               lexbuf.lex_start_pos lexbuf.lex_curr_pos in
           (`Key {loc =
                 {loc_start = lexbuf.lex_start_p;
                  loc_end = lexbuf.lex_curr_p;
                  loc_ghost = false}; txt}:Tokenf.t)})]
     | None -> Locf.failf _loc "no following strings after kwd_symbol")
  
  ;
  ("ocaml_int",
   fun {loc = _loc; _} ->
     [(%re{int_literal as txt}, 
       %exp{`Int {loc = Lexing_util.from_lexbuf lexbuf;txt}})]
  );
  ("ocaml_num_literal",
   fun {loc = _loc; _} ->
     [( %re{int_literal  (('l'|'L'|'n' as s ) ?) as txt },
        %exp{
        let (loc:Locf.t) =
          {loc_start = lexbuf.lex_start_p;
           loc_end = lexbuf.lex_curr_p;
           loc_ghost = false} in
        match s with
        | Some 'l' -> `Int32 {loc;txt}
        | Some 'L' -> `Int64 {loc;txt}
        | Some 'n' -> `Nativeint {loc;txt}
        | _ -> `Int {loc;txt}})]);
  
  ("ocaml_char",
   fun {loc = _loc; _} ->
     [
      (%re{"'" (newline as txt) "'"},  %exp{
       begin
         let pos = lexbuf.lex_curr_p in
         lexbuf.lex_curr_p <-
           { pos with
             pos_lnum =  pos.pos_lnum + 1;
             pos_bol = pos.pos_cnum - 1;};
         (`Chr {loc =
                {loc_start = lexbuf.lex_start_p;
                 loc_end = lexbuf.lex_curr_p;
                 loc_ghost = false}; txt } : Tokenf.t)
       end});
         
      (%re{ "'" (ocaml_char as txt ) "'"}, 
       %exp{ (`Chr {loc= 
            {loc_start = lexbuf.lex_start_p;
              loc_end = lexbuf.lex_curr_p;
              loc_ghost = false}; txt } : Tokenf.t)});
         
   (%re{ "'\\" (_ as c)}, 
           %exp{Lexing_util.err (Illegal_escape (String.make 1 c))
                  ({loc_start = lexbuf.lex_start_p; loc_end = lexbuf.lex_curr_p;
                    loc_ghost = false}:Locf.t)})]);

  ("ocaml_float_literal",
   fun {loc = _loc; _} -> 
     [ (%re{float_literal as txt},
        %exp{ (`Flo {loc = {
                     loc_start = lexbuf.lex_start_p;
                     loc_end = lexbuf.lex_curr_p;
                     loc_ghost = false};txt} : Tokenf.t)})])
  ;
  
  ("ocaml_comment",
   fun { quot_opt = q ; loc =  _loc; _} ->
     [(%re{ "(*" (')' as x) ?},
       append_quot q 
       %exp{
       let c = Lexing_util.new_cxt () in
       (* let old = lexbuf.lex_start_p in *)
       begin
         if x <> None then Lexing_util.warn Comment_start (Lexing_util.from_lexbuf lexbuf);
         Lexing_util.store c lexbuf;
         Lexing_util.push_loc_cont c lexbuf Lexing_util.lex_comment;
         ignore (Lexing_util.buff_contents c) ; (* Needed to clean the buffer *)
         (* let loc = old -- lexbuf.lex_curr_p in *)
         (* `Comment {loc;txt= buff_contents c} *)
       end})])
  ;
  ("whitespace", fun {quot_opt = q; loc =  _loc; _} -> 
    [
     (%re{ocaml_blank + }, append_quot q %exp{()});
     (%re{newline}, append_quot q %exp{Lexing_util.update_loc lexbuf})
   ]
  )
  ;
  
  ("ocaml_string", 
  fun {loc = _loc; _} ->
    [(%re{'"'}, %exp{
      let c = Lexing_util.new_cxt () in
      let old = lexbuf.lex_start_p in
      begin
        Lexing_util.push_loc_cont c lexbuf Lexing_util.lex_string;
        `Str {loc = Location_util.(--) old  lexbuf.lex_curr_p;
              txt = Lexing_util.buff_contents c}
      end})]);

 ("default",
  fun {loc = _loc; _} ->
    [(%re{_ as c}, %exp{
      Lexing_util.err (Illegal_character c)  @@ Lexing_util.from_lexbuf lexbuf})]
 )
 ;
  ("ocaml_eof",
   fun { loc = _loc; _} ->
     [(%re{eof},%exp{
       let pos = lexbuf.lex_curr_p in (* FIXME *)
       (lexbuf.lex_curr_p <-
         { pos with pos_bol  = pos.pos_bol  + 1 ;
           pos_cnum = pos.pos_cnum + 1 };
        let loc = Lexing_util.from_lexbuf lexbuf in
        (`EOI {loc;txt=""} : Tokenf.t)) })])
  ;

  ("ocaml_simple_quotation",
  fun {loc = _loc;_} ->   
    [(%re{"%{"}, %exp{
      let old = lexbuf.lex_start_p in
      let c = Lexing_util.new_cxt () in
      begin
        Lexing_util.store c lexbuf;
        Lexing_util.push_loc_cont c lexbuf Lexing_util.lex_quotation;
        `Quot {name=Tokenf.empty_name;
               meta=None;
               txt = Lexing_util.buff_contents c ;
               shift = 2;
               retract = 1;
               loc = Location_util.(--) old lexbuf.lex_curr_p}
      end})])
 
 ;
  ("ocaml_quotation",
   fun {loc = _loc; _} ->
     [(%re{'%'  (quotation_name as name) ? ('@' (ident as meta))? "{"    as shift},
       %exp{
       let c = Lexing_util.new_cxt () in
       let name =
         match name with
         | Some name -> Tokenf.name_of_string name
         | None -> Tokenf.empty_name  in
       begin
         let old = lexbuf.lex_start_p in
         let txt =
           begin
             Lexing_util.store c lexbuf;
             Lexing_util.push_loc_cont c lexbuf Lexing_util.lex_quotation;
             Lexing_util.buff_contents c
           end in
         let loc = Location_util.(--) old lexbuf.lex_curr_p in
         let shift = String.length shift in
         let retract =  1  in
         `Quot{Tokenf.name;meta;shift;txt;loc;retract}
       end})])
   ;
  ("ocaml_double_quotation",
   fun {loc = _loc; _} ->
     [(%re{ ("%" as x) ? '%'  (quotation_name as name) ? ('@' (ident as meta))? "{" as shift},
       %exp{
       let c = Lexing_util.new_cxt () in
       let name =
         match name with
         | Some name -> Tokenf.name_of_string name
         | None -> Tokenf.empty_name  in
       begin
         let old = lexbuf.lex_start_p in
         let txt =
           begin
             Lexing_util.store c lexbuf;
             Lexing_util.push_loc_cont c lexbuf Lexing_util.lex_quotation;
             Lexing_util.buff_contents c
           end in
         let loc = Location_util.(--) old lexbuf.lex_curr_p in
         let shift = String.length shift in
         let retract = 1  in
         if x = None then
           (`Quot{name;meta;shift;txt;loc;retract} : Tokenf.t)
         else
           (`DirQuotation {name;meta;shift;txt;loc;retract} : Tokenf.t)
       end})])

  ;

  ("line_directive",
   fun {quot_opt = q; loc =  _loc; _} ->   
   [(%re{"#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
       ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
       [^'\010' '\013']* newline},
     append_quot q
       %exp{
         begin
           Lexing_util.update_loc
             lexbuf ?file:name ~line:(int_of_string num) ~absolute:true 
         end})])
  ;
 
       (**************************)
       (* Antiquotation handling *)       
       (* $x                     *)
       (* $x{}                   *)
       (* $x:id                  *)
       (* ${}                    *)
       (**************************)
    ("ocaml_ant",
     fun  {loc = _loc; _} ->
     [
     (%re{ '$' ( ocaml_lid as name) (':'  identchars as follow)? as txt}, %exp{
     let (kind,shift) =
       match follow with
       | None -> ("", 1 )
       | Some _ -> (name, String.length name + 2) in 
     (`Ant{loc = Lexing_util.from_lexbuf lexbuf;
          kind ;
          txt ;
          shift ;
          retract = 0;
           cxt = None} : Tokenf.t)}) ;
     (%re{ "$" ( ocaml_lid as name)? "{"  as txt},  %exp{
     let old = lexbuf.lex_start_p in
     let c = Lexing_util.new_cxt () in
     begin
       Lexing_util.store c lexbuf;
       Lexing_util.push_loc_cont c lexbuf Lexing_util.lex_quotation;
       `Ant{loc =
            {loc_start = old;
             loc_end = lexbuf.lex_curr_p;
             loc_ghost = false};
            kind = match name with | Some n -> n | None -> "";
            txt = Lexing_util.buff_contents c;
            shift =  String.length txt ;
            retract =  1 ;
            cxt = None}
     end});
   (%re{ '$' (_ as c)},
    %exp{Lexing_util.err (Illegal_character c) @@ Lexing_util.from_lexbuf lexbuf})])
    ;
  ]

