

open Lib;
open LibUtil;  
open PreCast.Syntax; (* FIXME contains a lot of modules, like Gen*)

{:extend.create|Gram regexp chr ch_class regexps lex declare_regexp|};

(* let apply () = *)
  {:extend|Gram
    lex:
    [ "|"; L0[regexp{r};"->"; sequence{a} -> (r, Expr.mksequence  a)] SEP "|"{l} ->
      FanLexTools.gen_definition _loc l  ]
    declare_regexp:
    [ FOLD1 (fun (x,r) () -> begin 
        if Hashtbl.mem FanLexTools.named_regexps x then
          Printf.eprintf 
            "pa_ulex (warning): multiple definition of named regexp '%s'\n" x
        else ();
        Hashtbl.add FanLexTools.named_regexps x r
    end) (())
        [`LID x ; ":"; regexp{r} -> (x,r)]  SEP ";" -> {:str_item| |}]
    regexps:
    ["{" ; L1 regexp SEP ";"{xs};"}"  -> Array.of_list xs ]  
    regexp:
    {
     [S{r1};"|";S{r2} -> FanLexTools.alt r1 r2]
     [S{r1}; S{r2} -> FanLexTools.seq r1 r2 ]
     [S{r1}; "*" -> FanLexTools.rep r1
     |S{r1}; "+" -> FanLexTools.plus r1
     |S{r1}; "?" -> FanLexTools.alt FanLexTools.eps r1
     |"("; S{r1}; ")" -> r1
     |"_" -> FanLexTools.chars LexSet.any
     |chr{c} -> FanLexTools.chars (LexSet.singleton c)
     |`STR(s,_) -> FanLexTools.of_string s
     |"["; ch_class{cc};"]" -> FanLexTools.chars cc
     | "[^"; ch_class{cc}; "]" -> FanLexTools.chars (LexSet.difference LexSet.any cc)         
     |`LID x ->
         try  Hashtbl.find FanLexTools.named_regexps x
         with Not_found ->
           failwithf "referenced to unbound named  regexp  `%s'" x  ]}
    chr: 
    [ `CHAR (c,_) -> Char.code c
    |  `INT(i,s) ->
        if i >= 0 && i <= LexSet.max_code then i
        else failwithf "Invalid Unicode code point:%s" s]
    ch_class: 
    [ chr{c1}; "-"; chr{c2} -> LexSet.interval c1 c2
    | chr{c} -> LexSet.singleton c
    | S{cc1}; S{cc2} -> LexSet.union cc1 cc2
    | `STR(s,_) ->  begin 
        let c = ref LexSet.empty ;
        for i = 0 to String.length s - 1 do
	  c := LexSet.union !c (LexSet.singleton (Char.code s.[i])) 
        done;
          !c
    end ] |};
AstQuotation.of_expr ~name:"lex" ~entry:lex ;
AstQuotation.of_str_item ~name:"lex.regexp" ~entry:declare_regexp;  
  
(*
let change_ids suffix = object
  inherit FanAst.map (* as super *);
  method! ident = with ident function
    [ {|$lid:s|}  when String.length s > 6 && String.sub s 0 6 = "__ulex" -> {|$(lid:s^suffix)|}
    | i -> i];
end;

let () =
  let first = ref true in
  let _loc = FanLoc.ghost in 
  AstFilters.register_str_item_filter 
    ("ulex",(fun s -> begin 
      assert(!first); first := false;
      let table_counter = ref 0;
      let tables = Hashtbl.create 31 ;
      let parts = List.map (LexGen.partition ~counter:table_counter ~tables) (FanLexTools.partitions ()) in
      let tables = List.map LexGen.table (LexGen.get_tables ~tables ()) in
      let suffix = "__" ^ Digest.to_hex (Digest.string (Marshal.to_string (parts, tables) [])) in
      (change_ids suffix) # str_item {:str_item| $list:tables; $list:parts; $s |}
    end
    ));
*)





