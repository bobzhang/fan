
module Ast = Camlp4Ast;
open PreCast.Syntax; (* FIXME contains a lot of modules, like Gen*)

{:extend.create|Gram regexp chr ch_class|};

let apply () =
  {:extend|Gram
  expr: Level "top"
  ["lexer"; "["; L0 [regexp{r};"->";expr{a} -> (r,a)] SEP "|"{l}; "]" ->
    LexGen.gen_definition _loc l  ]
  str_item: Level "top"
  ["let"; `LID "regexp"; `LID x ; "="; regexp{r} -> begin 
    if Hashtbl.mem Ulex.named_regexps x then
      Printf.eprintf 
        "pa_ulex (warning): multiple definition of named regexp '%s'\n"
        x
    else ();
    Hashtbl.add Ulex.named_regexps x r ;
    {:str_item||}
  end ]
  regexp:
  {[S{r1};"|";S{r2} -> Ulex.alt r1 r2]
   [S{r1}; S{r2} -> Ulex.seq r1 r2 ]
   [S{r1}; "*" -> Ulex.rep r1
   |S{r1}; "+" -> Ulex.plus r1
   |S{r1}; "?" -> Ulex.alt Ulex.eps r1
   |"("; S{r1}; ")" -> r1
   |"_" -> Ulex.chars Cset.any
   |chr{c} -> Ulex.chars (Cset.singleton c)
   |`STR(s,_) -> LexGen.regexp_for_string s
   |"["; ch_class{cc};"]" -> Ulex.chars cc
   | "[^"; ch_class{cc}; "]" -> Ulex.chars (Cset.difference Cset.any cc)         
   |`LID x ->
       try  Hashtbl.find Ulex.named_regexps x
       with Not_found ->
         failwith 
           ("pa_ulex (error): reference to unbound regexp name `"^x^"'")
  ]}

  chr: 
  [ `CHAR (c,_) -> Char.code c
  |  `INT(_,i) -> LexGen.char_int i ]
  ch_class: 
   [ chr{c1}; "-"; chr{c2} -> Cset.interval c1 c2
   | chr{c} -> Cset.singleton c
   | S{cc1}; S{cc2} -> Cset.union cc1 cc2
   | `STR(s,_) ->  begin 
       let c = ref Cset.empty ;
       for i = 0 to String.length s - 1 do
	 c := Cset.union !c (Cset.singleton (Char.code s.[i])) 
       done;
       !c
   end ]
     
|};




AstParsers.register_parser ("lexer", apply);








let change_ids suffix = object
  inherit Camlp4Ast.map (* as super *);
  method! ident = function
    [ Ast.IdLid (loc, s) when String.length s > 6 && String.sub s 0 6 = "__ulex" ->
      Ast.IdLid (loc, s ^ suffix)
    | i -> i];
end;

let () =
  let first = ref true in
  let _loc = FanLoc.ghost in 
  AstFilters.register_str_item_filter 
    ("ulex",(fun s -> begin 
       assert(!first); first := false;
       let parts = List.map LexGen.partition (Ulex.partitions ()) in
       let tables = List.map LexGen.table (LexGen.get_tables ()) in
       let suffix = "__" ^ Digest.to_hex (Digest.string (Marshal.to_string (parts, tables) [])) in
       (change_ids suffix) # str_item {:str_item| $list:tables; $list:parts; $s |}
    end
    ));






