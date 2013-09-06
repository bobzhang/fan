
open LibUtil
open Translate_lex
open! Fsyntax

let regexp_for_string s =
  let rec re_string n =
    if n >= String.length s then Epsilon
    else if succ n = String.length s then
      Characters (Fcset.singleton (Char.code s.[n]))
    else
      Sequence
        (Characters(Fcset.singleton (Char.code s.[n])),
         re_string (succ n))
  in re_string 0
    
let named_regexps =
  (Hashtbl.create 13 : (string, concrete_regexp) Hashtbl.t)

let rec remove_as = function
  | Bind (e,_) -> remove_as e
  | Epsilon|Eof|Characters _ as e -> e
  | Sequence (e1, e2) -> Sequence (remove_as e1, remove_as e2)
  | Alternative (e1, e2) -> Alternative (remove_as e1, remove_as e2)
  | Repetition e -> Repetition (remove_as e)

        
let as_cset = function
  | Characters s -> s
  | _ -> raise Fcset.Bad
;;

{:create|Fgram
  regexp
  char_class
  char_class1
  lex
  declare_regexp
|};;

{:extend|Fgram
    lex:
    [ "|"; L0 case SEP "|"{l} ->
      Compile_lex.output_entry
        (Lexgen.make_single_dfa
        {shortest=false;clauses=l})
    | "<";L0 case SEP "|"{l} ->
        Compile_lex.output_entry
        (Lexgen.make_single_dfa
        {shortest=true;clauses=l})]
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
      | `Ant(_loc,_) ->
          assert false]  
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
   | "!" -> Eof (* eof *)
   | `CHAR(c,_) -> (Characters (Fcset.singleton (Char.code c)))
   | `STR(s,_) -> regexp_for_string s
   | "["; char_class{cc}; "]" -> Characters cc
   | S{r1};"*" -> Repetition r1
   | S{r1};"?" -> Alternative (Epsilon,r1)
   | S{r1};"+" -> Sequence (Repetition (remove_as r1), r1)

   | "("; S{r1}; ")" -> r1
   | `Lid x -> begin (* FIXME token with location *)
       try Hashtbl.find named_regexps x
       with Not_found ->
         let p = FLoc.start_pos _loc in begin
           Printf.eprintf "File \"%s\", line %d, character %d:\n\
            Reference to unbound regexp name `%s'.\n" p.Lexing.pos_fname p.Lexing.pos_lnum
           (p.Lexing.pos_cnum - p.Lexing.pos_bol) x;
           exit 2
        end
    end
  ] (* FIXME rule mask more friendly error message *)
 }
  
  char_class:
  [ "!"; char_class1{r} -> Fcset.complement r
  | char_class1{r} -> r ]

  char_class1:
  [ `CHAR (c1,_); "-"; `CHAR(c2,_) ->
    let c1 = Char.code c1 in
    let c2 = Char.code c2 in
    Fcset.interval c1 c2
  | `CHAR(c1,_)  -> Fcset.singleton (Char.code c1)
  | S{cc1}; S{cc2} -> Fcset.union cc1 cc2 
  ]
|};;  

let d = `Absolute ["Fan";"Lang"];;
begin
  AstQuotation.of_exp
  ~name:(d,"lexer") ~entry:lex ;
AstQuotation.of_stru
    ~name:(d,"regexp")
    ~entry:declare_regexp;  
end;;

