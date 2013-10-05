

open Translate_lex
open! Fsyntax

let named_regexps =
  (Hashtbl.create 13 : (string, concrete_regexp) Hashtbl.t)

let _ = begin
  Hashtbl.add named_regexps "eof" Eof ;
end

      
(* type concrete_regexp *)
(*       = *)
(*   | Epsilon *)
(*   | Eof       *)
(*   | Characters of Fcset.t *)
(*   | Sequence of concrete_regexp * concrete_regexp *)
(*   | Alternative of concrete_regexp * concrete_regexp *)
(*   | Repetition of concrete_regexp *)
(*   | Bind of concrete_regexp * (FLoc.t * string) with ("Print") *)

exception UnboundRegexp;;

{:create|
  regexp  char_class  char_class1  lex  declare_regexp
|};;

{:extend|Fgram
    lex:
    [ "|"; L0 case SEP "|"{l} ->
      Compile_lex.output_entry @@
        Lexgen.make_single_dfa {shortest=false;clauses=l}
    | "<";L0 case SEP "|"{l} ->
        Compile_lex.output_entry @@ 
        Lexgen.make_single_dfa {shortest=true;clauses=l}
    ]
  let case:
    [ regexp{r};"->";exp{a} -> (r,a)]  
  declare_regexp:
  ["let";`Lid x ; "=";regexp{r} ->
    if Hashtbl.mem named_regexps x then begin 
      Printf.eprintf
        "fanlex (warning): multiple definition of named regexp '%s'\n" x;
      exit 2 
    end
    else begin
      Hashtbl.add named_regexps x r;
      {:stru|let _ = () |}
    end
  | S; S{x} -> x]

  regexp:
  {
   "as"
   [S{r1};"as"; a_lident{x} ->
     match x with
      | `Lid(loc,y) (* (#FAst.lident as y) *) ->   
          Bind(r1,(loc,y)) (* FIXME *)
      | `Ant(_loc,_) -> assert false]  
   "#"
   [S{r1}; "#" ; S{r2} ->
      let s1 = as_cset r1 in
      let s2 = as_cset r2 in
      Characters (Fcset.diff s1 s2)]
     
   "|"
   [S{r1}; "|"; S{r2} -> Alternative (r1,r2)]
   "app"
   [ S{r1};S{r2} -> Sequence(r1,r2)]  
   "basic"  
   [ "_" -> Characters Fcset.all_chars
   | `Chr c -> Characters (Fcset.singleton (Char.code @@ TokenEval.char c))
   | `Str s -> regexp_for_string @@ TokenEval.string s (* FIXME *)
   | "["; char_class{cc}; "]" -> Characters cc
   | S{r1};"*" -> Repetition r1
   | S{r1};"?" -> Alternative (Epsilon,r1)
   | S{r1};"+" -> Sequence (Repetition (remove_as r1), r1)

   | "("; S{r1}; ")" -> r1
   | `Lid x -> begin (* FIXME token with location *)
       try Hashtbl.find named_regexps x
       with Not_found ->
         let p = FLoc.start_pos _loc in begin
           Fan_warnings.emitf p  "Reference to unbound regexp name `%s'" x ;
           raise UnboundRegexp
        end
    end
  ] (* FIXME rule mask more friendly error message *)
 }
  
  char_class:
  [ "!"; char_class1{r} -> Fcset.complement r
  | char_class1{r} -> r ]

  char_class1:
  [ `Chr c1; "-"; `Chr c2 ->
    let c1 = Char.code @@ TokenEval.char c1 in
    let c2 = Char.code @@ TokenEval.char c2 in
    Fcset.interval c1 c2
  | `Chr c1   -> Fcset.singleton (Char.code @@ TokenEval.char c1)
  | S{cc1}; S{cc2} -> Fcset.union cc1 cc2 
  ]
|};;  

let d = `Absolute ["Fan";"Lang"];;
begin
  Ast_quotation.of_exp
  ~name:(d,"lexer") ~entry:lex ();
  Ast_quotation.of_stru
    ~name:(d,"regexp")
    ~entry:declare_regexp ();  
end;;

