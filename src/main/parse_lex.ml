%import{
Translate_lex:
  as_cset
  regexp_for_string
  remove_as
  ;
};;

let named_regexps =
  (Hashtbl.create 13 : (string, Translate_lex.concrete_regexp) Hashtbl.t)

let named_cases =
  (Hashtbl.create 13 :
     (string,
      (Tokenf.txt list option -> Tokenf.quot option
        -> Locf.t
          -> (Translate_lex.concrete_regexp *
                FAst.exp) list)) Hashtbl.t )

let _ =
  let (+>) = Hashtbl.add named_regexps in
  begin
    "newline"  +> %re{('\010' | '\013' | "\013\010")};
    "ocaml_blank" +> %re{[' ' '\009' '\012']};
    "lowercase" +> %re{['a'-'z' '\223'-'\246' '\248'-'\255' '_']};
    "uppercase"  +> %re{['A'-'Z' '\192'-'\214' '\216'-'\222']};
    "identchar" +> %re{['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']};

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

let append_quot (y:Tokenf.quot option) (e:FAst.exp)  =
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
    fun ls _ _loc ->
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
   fun ls _ _loc  ->
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
   fun ls _ _loc ->
     match ls with
     | Some ls  -> 
         let regexp =
           Listf.reduce_left_with ~compose:(fun r1 r2 ->
             ((Alternative (r1,r2)):Translate_lex.concrete_regexp))
             ~project:(fun (x:Tokenf.txt) ->
               regexp_for_string @@ TokenEval.string x.txt)
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
   fun _ _ _loc ->
     [(%re{int_literal as txt}, 
       %exp{`Int {loc = Lexing_util.from_lexbuf lexbuf;txt}})]
  );
  ("ocaml_num_literal",
   fun _ _ _loc ->
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
   fun _ _ _loc ->
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
   fun _ _ _loc -> 
     [ (%re{float_literal as txt},
        %exp{ (`Flo {loc = {
                     loc_start = lexbuf.lex_start_p;
                     loc_end = lexbuf.lex_curr_p;
                     loc_ghost = false};txt} : Tokenf.t)})])
  ;
  
  ("ocaml_comment",
   fun _ q _loc ->
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
  ("whitespace", fun _ q _loc -> 
    [
     (%re{ocaml_blank + }, append_quot q %exp{()});
     (%re{newline}, append_quot q %exp{Lexing_util.update_loc lexbuf})
   ]
  )
  ;
  
  ("ocaml_string", 
  fun _ _ _loc ->
    [(%re{'"'}, %exp{
      let c = Lexing_util.new_cxt () in
      let old = lexbuf.lex_start_p in
      begin
        Lexing_util.push_loc_cont c lexbuf Lexing_util.lex_string;
        `Str {loc = Location_util.(--) old  lexbuf.lex_curr_p;
              txt = Lexing_util.buff_contents c}
      end})]);

 ("default",
  fun _ _ _loc ->
    [(%re{_ as c}, %exp{
      Lexing_util.err (Illegal_character c)  @@ Lexing_util.from_lexbuf lexbuf})]
 )
 ;
  ("ocaml_eof",
   fun _ _ _loc ->
     [(%re{eof},%exp{
       let pos = lexbuf.lex_curr_p in (* FIXME *)
       (lexbuf.lex_curr_p <-
         { pos with pos_bol  = pos.pos_bol  + 1 ;
           pos_cnum = pos.pos_cnum + 1 };
        let loc = Lexing_util.from_lexbuf lexbuf in
        (`EOI {loc;txt=""} : Tokenf.t)) })])
  ;

  ("ocaml_simple_quotation",
  fun _ _ _loc ->   
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
   fun _ _ _loc ->
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
   fun _ _ _loc ->
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
   fun _ q _loc ->   
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
     fun _ _ _loc ->
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
  (* ("ocaml_ant", *)
  (*  [(,)] *)


  ]

let meta_cset _loc (x:Fcset.t)=
  Fan_ops.meta_list (fun _loc (a,b) -> %ep{($int':a,$int':b)}) _loc x
    
(** FIXME derive later *)    
let rec meta_concrete_regexp _loc (x : Translate_lex.concrete_regexp )  =
  match x with
  | Epsilon -> %ep{ Epsilon}
  | Eof -> %ep{Eof}
  | Characters a -> %ep{Characters ${meta_cset _loc a}}
  | Sequence (a0,a1) ->
      %ep{Sequence ${meta_concrete_regexp _loc a0} ${meta_concrete_regexp _loc a1}}
  | Alternative(a0,a1) ->
      %ep{Alternative ${meta_concrete_regexp _loc a0} ${meta_concrete_regexp _loc a1}}
  | Repetition a -> %ep{Repetition ${meta_concrete_regexp _loc a}}
  | Bind (a,(loc,s)) ->
  %ep{Bind (${meta_concrete_regexp _loc  a},
  (${Ast_gen.meta_here _loc loc }, ${%ep@loc{$str':s}}))}

        
let _ = begin
  Hashtbl.add named_regexps "eof" Eof ;
end

exception UnboundRegexp;;
exception UnboundCase;;

%create{ regexp
          char_class
          char_class1
          lex
          declare_regexp
          lex_fan
          case};;

(*
   since we do unsafe_extend on top of Gramf...
 *)

let make_automata shortest l =
  Compile_lex.output_entry @@
  Lexgen.make_single_dfa {shortest;clauses=Listf.concat l};;

let make_lex nt a b = %extend{
  nt:
  [  "|" ; L0 case SEP "|"  ${a}
  | "<";L0 case SEP "|"  ${b}]};;


let _ = begin
  make_lex lex
    (fun l _ _ -> make_automata false l)
    (fun l _ _ -> make_automata true l);
  make_lex lex_fan
    (fun l _ _loc -> let e = make_automata false l in
    %exp{ ($e : Lexing.lexbuf -> Tokenf.t)})
    (fun l _ _loc -> let e = make_automata true l in
    %exp{ ($e : Lexing.lexbuf -> Tokenf.t)});
end;;
    
%extend{(g:Gramf.t) 
  case:
    [ regexp as r;  Quot x  %{
      [(r,Parsef.expand_exp x  )]}
    | "@"; Lid@xloc x; ?Quot y %{
        let res =
          try Hashtbl.find named_cases x
          with Not_found ->  begin
            Fan_warnings.emitf xloc.loc_start
              "Reference to unbound case name %s" x;
            raise UnboundCase
          end in
        res None y xloc}
    | "@"; Lid@xloc x ; "("; L1 Str SEP "|" as l; ")"; ? Quot y %{
       (* FIXME ? Quote -- better error message do you mean Quot -- possible ? *)
      let res =
          try Hashtbl.find named_cases x
          with Not_found ->  begin
            Fan_warnings.emitf xloc.loc_start
              "Reference to unbound case name %s" x;
            raise UnboundCase
          end in
        res (Some l) y xloc}]  
  declare_regexp:
  ["let"; Lid@xloc x ; "=";regexp as r %{
    if Hashtbl.mem named_regexps x then begin 
      Fan_warnings.emitf  xloc.loc_start
        "fanlex (warning): multiple definition of named regexp '%s'\n" x;
       %stru{let _ = ()}
    end
    else begin
      Hashtbl.add named_regexps x r;
      %stru{let _ = () }
    end}
  | S; S as x %{x}]
  regexp: 10
  [S as r1;"as"; Lid@xloc y %{ Bind(r1,(xloc,y))} ]
  regexp: 20          
  [S as r1; "#" ; S as r2 %{
      let s1 = as_cset r1 in
      let s2 = as_cset r2 in
      Characters (Fcset.diff s1 s2)}]
  regexp: 30  
  [S as r1; "|"; S as r2 %{ Alternative (r1,r2)}]
  regexp: 40
  [ S as r1;S as r2 %{ Sequence(r1,r2)}]  
  regexp: 50  
  [ "_" %{ Characters Fcset.all_chars}
  | Chr c %{ Characters (Fcset.singleton (Char.code @@ TokenEval.char c))}
  | Str s %{ regexp_for_string @@ TokenEval.string s (* FIXME *)}
  | "["; char_class as cc; "]" %{ Characters cc}
  | S as r1;"*" %{ Repetition r1}
  | S as r1;"?" %{ Alternative (Epsilon,r1)}
  | S as r1;"+" %{ Sequence (Repetition (remove_as r1), r1)}
  | "("; S as r1; ")" %{ r1}
  | "eof" %{ Eof}
  | Lid@xloc x %{
         try Hashtbl.find named_regexps x
         with Not_found ->
         begin 
           Fan_warnings.emitf xloc.loc_start
             "Reference to unbound regexp name `%s'" x ;
           raise UnboundRegexp
        end
    }
  ] (* FIXME rule mask more friendly error message *) 
  
  char_class:
  [ "^"; char_class1 as r %{ Fcset.complement r}
  | char_class1 as r %{ r}
  ]

  char_class1:
  [ Chr c1; "-"; Chr c2 %{
    let c1 = Char.code @@ TokenEval.char c1 in
    let c2 = Char.code @@ TokenEval.char c2 in
    Fcset.interval c1 c2}
  | Chr c1   %{ Fcset.singleton (Char.code @@ TokenEval.char c1)}
  | S as cc1; S as cc2 %{ Fcset.union cc1 cc2 }
  ] };;  



let () =
  let d  =Ns.lang in
  begin
    Ast_quotation.of_exp ~lexer:Lex_lex.from_stream
      ~name:(d,"lex") ~entry:lex ();
    Ast_quotation.of_exp ~lexer:Lex_lex.from_stream
      ~name:(d,"lex_fan") ~entry:lex_fan ();
    (* Ast_quotation.of_exp ~lexer:Lex_lex.from_stream *)
    (*   ~name:(d,"lex_stream") ~entry:lex_stream (); *)
    Ast_quotation.of_stru
      ~lexer:Lex_lex.from_stream
      ~name:(d,"regex")
      ~entry:declare_regexp ();
    Ast_quotation.add_quotation
      ~lexer:Lex_lex.from_stream
      (d,"re") regexp
      ~mexp:meta_concrete_regexp
      ~mpat:meta_concrete_regexp
      ~exp_filter:(fun x -> (x : FAst.ep :>FAst.exp))
      ~pat_filter:(fun x -> (x : FAst.ep :>FAst.pat));
  end;;


(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/parse_lex.cmo" *)
(* end: *)
