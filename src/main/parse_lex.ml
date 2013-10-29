
open Translate_lex
let named_regexps =
  (Hashtbl.create 13 : (string, concrete_regexp) Hashtbl.t)

let _ = begin
  Hashtbl.add named_regexps "eof" Eof ;
end

exception UnboundRegexp;;

let g =
  Gramf.create_lexer ~annot:"Lexer's lexer"
    ~keywords:["as";"eof";"let";
               "#" ; "|" ; "^" ;
               "<" ; "->" ;"=" ;
               "_" ; "*" ; "[" ;
               "]" ; "*" ; "?" ;
               "+" ; "(" ; ")" ;
               "-"] ();;

%create{(g:Gramf.t) regexp  char_class  char_class1  lex  declare_regexp};;

(* open Parse_lex2,
   it will suck eof handling, to be investigated.
   since we do unsafe_extend on top of Gramf...
 *)

%extend{(g:Gramf.t)  (* FIXME location wrong *)
    lex:
    [  "|" ; L0 case SEP "|" {l} %{
      Compile_lex.output_entry @@ Lexgen.make_single_dfa {shortest=false;clauses=l}}
    | "<";L0 case SEP "|" {l} %{
        Compile_lex.output_entry @@ Lexgen.make_single_dfa {shortest=true;clauses=l}}]
  let case:
    [ regexp as r;  Quot x  %{
      let expander loc _ s = Gramf.parse_string ~loc Syntaxf.exp s in
      let e = Tokenf.quot_expand expander x in (r,e)}]  
  declare_regexp:
  ["let"; Lid x ; "=";regexp as r %{
    if Hashtbl.mem named_regexps x then begin 
      Printf.eprintf
        "fanlex (warning): multiple definition of named regexp '%s'\n" x;
      exit 2 
    end
    else begin
      Hashtbl.add named_regexps x r;
      %stru{let _ = () }
    end}
  | S; S as x %{x}]

  let lid: [ Lid y %{ (_loc, y)} ]  
  regexp:
  {
   "as"
   [S as r1;"as"; lid {z} %{ Bind(r1,(z))} ] 
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
   | Lid x %{ begin (* FIXME token with location *)
       try Hashtbl.find named_regexps x
       with Not_found ->
         let p = _loc.loc_start in begin
           Fan_warnings.emitf p  "Reference to unbound regexp name `%s'" x ;
           raise UnboundRegexp
        end
    end}
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
    Ast_quotation.of_stru
      ~lexer:Lex_lex.from_stream
      ~name:(d,"regex")
      ~entry:declare_regexp ();  
  end;;


(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/parse_lex.cmo" *)
(* end: *)
