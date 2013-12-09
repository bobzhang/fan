%import{
Translate_lex:
  as_cset
  regexp_for_string
  remove_as
  ;
};;


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

let make_automata loc shortest l =
  Compile_lex.output_entry loc
    (Lexgen.make_single_dfa {shortest;clauses=Listf.concat l})

let make_lex nt a b = %extend{
  nt:
  [  "|" ; L0 case SEP "|"  ${a }
  | "<"  ; L0 case SEP "|"  ${b }]};;


let _ = begin
  make_lex lex
    (fun l _ loc -> make_automata loc false l)
    (fun l _ loc -> make_automata loc true  l);
  make_lex lex_fan
    (fun l _ _loc -> let e = make_automata _loc false l in
    %exp{ ($e : Lexing.lexbuf -> Tokenf.t)})
    (fun l _ _loc -> let e = make_automata _loc true l in
    %exp{ ($e : Lexing.lexbuf -> Tokenf.t)});
end;;
    
%extend{(g:Gramf.t) 
  case:
    [ regexp as r;  Quot x  %{
      [(r,Parsef.expand_exp x  )]}
    | "@"; Lid@xloc x; ?Quot y %{
        let res =
          try Hashtbl.find Predef_lex.named_cases x
          with Not_found ->  begin
            Fan_warnings.emitf xloc.loc_start
              "Reference to unbound case name %s" x;
            raise UnboundCase
          end in
        res {tokens_opt = None;  quot_opt =  y; loc =  xloc}}
    | "@"; Lid@xloc x ; "("; L1 Str SEP "|" as l; ")"; ? Quot y %{
       (* FIXME ? Quote -- better error message do you mean Quot -- possible ? *)
      let res =
          try Hashtbl.find Predef_lex.named_cases x
          with Not_found ->  begin
            Fan_warnings.emitf xloc.loc_start
              "Reference to unbound case name %s" x;
            raise UnboundCase
          end in
        res {tokens_opt = Some l;  quot_opt = y; loc =  xloc}}]  
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
  [ Chr c %{ Characters (Fcset.singleton (Char.code @@ Escape.char c))}
  | Str s %{ regexp_for_string @@ Escape.string s (* FIXME *)}
  | "["; char_class as cc; "]" %{ Characters cc}
  | S as r1;"*" %{ Repetition r1}
  | S as r1;"?" %{ Alternative (Epsilon,r1)}
  | S as r1;"+" %{ Sequence (Repetition (remove_as r1), r1)}
  | "("; S as r1; ")" %{ r1}
  | Lid@xloc x %{
         try Hashtbl.find Predef_lex.named_regexps x
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
    let c1 = Char.code @@ Escape.char c1 in
    let c2 = Char.code @@ Escape.char c2 in
    Fcset.interval c1 c2}
  | Chr c1   %{ Fcset.singleton (Char.code @@ Escape.char c1)}
  | S as cc1; S as cc2 %{ Fcset.union cc1 cc2 }]

  declare_regexp:
  ["let"; Lid@xloc x ; "=";regexp as r %{
    if Hashtbl.mem Predef_lex.named_regexps x then begin 
      Fan_warnings.emitf  xloc.loc_start
        "fanlex (warning): multiple definition of named regexp '%s'\n" x;
       (* %stru{let _ = ()} *)
    end
    else begin
      Hashtbl.add Predef_lex.named_regexps x r;
      (* %stru{let _ = () } *)
    end}
  | S; S as x %{x}]};;  



let () =
  let domain  = Ns.lang in
  begin
    Ast_quotation.of_exp ~lexer:Lex_lex.from_stream
      ~name:{domain; name = "lex"} ~entry:lex ();
    Ast_quotation.of_exp ~lexer:Lex_lex.from_stream
      ~name:{domain; name = "lex_fan"} ~entry:lex_fan ();
    (* Ast_quotation.of_exp ~lexer:Lex_lex.from_stream *)
    (*   ~name:(d,"lex_stream") ~entry:lex_stream (); *)
    
    (* Ast_quotation.of_stru *)
    (*   ~lexer:Lex_lex.from_stream *)
    (*   ~name:{domain; name = "regex"} *)
    (*   ~entry:declare_regexp (); *)
    Ast_quotation.register_unit_parser
      ~lexer:Lex_lex.from_stream
      (Tokenf.name_of_string"regex", declare_regexp);
    (* Fdir.register (d,"regex")  *)
    Ast_quotation.add_quotation
      ~lexer:Lex_lex.from_stream
      {domain; name = "re"} regexp
      ~mexp:meta_concrete_regexp
      ~mpat:meta_concrete_regexp
      ~exp_filter:(fun x -> (x : Astf.ep :>Astf.exp))
      ~pat_filter:(fun x -> (x : Astf.ep :>Astf.pat));
  end;;


(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/parse_lex.cmo" *)
(* end: *)
