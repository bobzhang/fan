%import{
Translate_lex:
  as_cset
  regexp_for_string
  remove_as
  ;
};;
(* open Util *)
let named_regexps =
  (Hashtbl.create 13 : (string, Translate_lex.concrete_regexp) Hashtbl.t)

let named_cases =
  (Hashtbl.create 13 :
     (string, (Translate_lex.concrete_regexp * FAst.exp) list) Hashtbl.t )

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
   (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?}
   end


let _ =
  let _loc = Locf.ghost in
  Hashtblf.add_list named_cases 
  [
  ("ocaml_uid", [(%re{ocaml_uid as txt},
  %exp{
  `Uid{loc =
  {loc_start = lexbuf.lex_start_p;
   loc_end = lexbuf.lex_curr_p;
  loc_ghost = false} ; txt }} )]);
  
  ("ocaml_int_literal",
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
  | _ -> `Int {loc;txt}
  })]);
  
  ("ocaml_char", [
  (%re{"'" (newline as txt) "'"}, %exp{
       begin
         Lexing_util.update_loc   lexbuf ~retract:1;
        `Chr {loc =  !!lexbuf;txt}
       end});
         
   (%re{ "'" (ocaml_char as txt ) "'"}, %exp{ `Chr {loc= !!lexbuf ;txt}});
         
   (%re{ "'\\" (_ as c)}, %exp{Lexing_util.err (Illegal_escape (String.make 1 c)) @@ !! lexbuf})
   ]);

  ("ocaml_float_literal",
  [  (%re{float_literal as txt},
  %exp{`Flo {loc = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
  loc_ghost = false};txt}})]
  )
  ;
  
  ("ocaml_comment",
     [(%re{ "(*" (')' as x) ?}, %exp{
       let c = Lexing_util.new_cxt () in
       (* let old = lexbuf.lex_start_p in *)
       begin
         if x <> None then Lexing_util.warn Comment_start (!!lexbuf);
         Lexing_util.store c lexbuf;
         Lexing_util.push_loc_cont c lexbuf lex_comment;
         ignore (Lexing_util.buff_contents c) ; (* Needed to clean the buffer *)
         (* let loc = old -- lexbuf.lex_curr_p in *)
         (* `Comment {loc;txt= buff_contents c} *)
       end})])
  ;
  ("whitespace",
  [
  (%re{ocaml_blank + }, %exp{()});
  (%re{newline}, %exp{
  update_loc lexbuf})
  ]
  )
  ;
  
  ("ocaml_string",
  
   [(%re{'"'}, %exp{
  let c = Lexing_util.new_cxt () in
  let old = lexbuf.lex_start_p in
  begin
    Lexing_util.push_loc_cont c lexbuf lex_string;
    let loc = old --  lexbuf.lex_curr_p in
   `Str {loc; txt = buff_contents c}
  end}
  )])
  ;

  (* ("ocaml_quotation", *)
  (*    [(%re{ ("%" as x) ? '%'  (quotation_name as name) ? ('@' (locname as meta))? "{" as shift}, %exp{ *)
  (*      let c = new_cxt () in *)
  (*      let name = *)
  (*        match name with *)
  (*        | Some name -> Tokenf.name_of_string name *)
  (*        | None -> Tokenf.empty_name  in *)
  (*      begin *)
  (*        let old = lexbuf.lex_start_p in *)
  (*        let txt = *)
  (*          begin *)
  (*            store c lexbuf; *)
  (*            push_loc_cont c lexbuf lex_quotation; *)
  (*            buff_contents c *)
  (*          end in *)
  (*        let loc = old -- lexbuf.lex_curr_p in *)
  (*        let shift = String.length shift in *)
  (*        let retract = 1  in *)
  (*        if x = None then *)
  (*          `Quot{name;meta;shift;txt;loc;retract} *)
  (*        else `DirQuotation {name;meta;shift;txt;loc;retract} *)
  (*      end})]) *)

  (* ; *)

  ("line_directive",
   [(%re{"#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
       ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
       [^'\010' '\013']* newline},   %exp{
         begin
           update_loc  lexbuf ?file:name ~line:(int_of_string num) ~absolute:true ;
           token lexbuf
         end})])
  ;

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
  (* failwithf "Bind not supported yet" *)
        
let _ = begin
  Hashtbl.add named_regexps "eof" Eof ;
end

exception UnboundRegexp;;
exception UnboundCase;;
let g =
  Gramf.create_lexer ~annot:"Lexer's lexer"
    ~keywords:["as";"eof";"let";
               "#" ; "|" ; "^" ;
               "<" ; "->" ;"=" ;
               "_" ; "*" ; "[" ;
               "]" ; "*" ; "?" ;
               "+" ; "(" ; ")" ;
               "-" ; "@" ] ();;

%create{(g:Gramf.t) regexp
          char_class
          char_class1
          lex
          declare_regexp
          lex_fan
      };;

(*
   since we do unsafe_extend on top of Gramf...
 *)

let make_automata shortest l =
  Compile_lex.output_entry @@
  Lexgen.make_single_dfa {shortest;clauses=Listf.concat l};;
  
%extend{(g:Gramf.t)  (* FIXME location wrong *)
    lex:
    [  "|" ; L0 case SEP "|" as l %{
        make_automata false l}
    | "<";L0 case SEP "|" as l %{
        make_automata true l }]
    lex_fan:
    [  "|" ; L0 case SEP "|" as l %{
      let e = make_automata false l in
      %exp{ ($e : Lexing.lexbuf -> Tokenf.t)}}
    | "<";L0 case SEP "|" as l %{
        let e = make_automata true l in
        %exp{($e: Lexing.lexbuf -> Tokenf.t)}}]          
  case@Local:
    [ regexp as r;  Quot x  %{
      let expander loc _ s = Gramf.parse_string ~loc Syntaxf.exp s in
      [(r,Tokenf.quot_expand expander x)]}
    | "@"; Lid@xloc x; ?Quot y %{
        let res =
          try Hashtbl.find named_cases x
          with Not_found ->  begin
            Fan_warnings.emitf xloc.loc_start
              "Reference to unbound case name %s" x;
            raise UnboundCase
          end in
        match y with
        | None -> res
        | Some y ->
           let expander loc _ s = Gramf.parse_string ~loc Syntaxf.exp s in
           let e = Tokenf.quot_expand expander y in 
           List.map (fun (x,v) -> (x, %exp{ begin $v; $e ; end})) res 
  }]  
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
  regexp:
  {
   "as"
   [S as r1;"as"; (* lid as z *) Lid@xloc y %{ Bind(r1,(xloc,y))} ] 
   "#"
   [S as r1; "#" ; S as r2 %{
      let s1 = as_cset r1 in
      let s2 = as_cset r2 in
      Characters (Fcset.diff s1 s2)}]
     
   "|"
   [S as r1; "|"; S as r2 %{ Alternative (r1,r2)}
   ]
   "app"
   [ S as r1;S as r2 %{ Sequence(r1,r2)}
   ]  
   "basic"  
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
  ] (* FIXME rule mask more friendly error message *) }
  
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
    Ast_quotation.of_stru
      ~lexer:Lex_lex.from_stream
      ~name:(d,"regex")
      ~entry:declare_regexp ();
    Ast_quotation.add_quotation (d,"re") regexp
      ~mexp:meta_concrete_regexp
      ~mpat:meta_concrete_regexp
      ~exp_filter:(fun x -> (x : FAst.ep :>FAst.exp))
      ~pat_filter:(fun x -> (x : FAst.ep :>FAst.pat));
  end;;


(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/parse_lex.cmo" *)
(* end: *)
