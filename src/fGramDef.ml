open FAst
open LibUtil

  
let pp_print_loc _f _loc  = ()
(* open StdFan (\* FIXME later *\) *)
;;
{:import|
StdFan:
  pp_print_string;
Objs:
  pp_print_vid'
  pp_print_vid
  pp_print_alident
  pp_print_ant;
|};;  

class mapbase = object
  method loc (x:loc) =  x
  method string (x:string) = x
  method ant (x:ant) = x
end

type lident =
  [ `Lid of (loc * string) ]  
and simple_pat =
  [
   `Vrn of (loc * string)
  |`App of (loc * simple_pat * simple_pat )
  |`Lid of (loc * string)
  | ant 
  |`Com of (loc * simple_pat * simple_pat)
  |`Alias of (loc * simple_pat * lident)
  |`Bar of (loc * simple_pat * simple_pat)
  |`Str of (loc * string)
  |`Any of loc] with ("Print" "Map")

let wildcarder = object (self)
  inherit map as super
  method! simple_pat = function
    | `Lid (_loc,_) -> `Any _loc
    | {:pat'| ($p as $_) |} -> self#simple_pat p
    | p  -> super#simple_pat p 
end;;

{:ocaml|

type name = {(* every entry has a name *)  
    exp : exp;
    tvar : string;
    loc : loc
  }

(* we need to define a new ADT only because
   we did not find a way to expess `STself and `STtok yet  *)
type styp =
 [ (* ident' *) vid'
 | `App of (loc * styp * styp)
 | `Quote of (loc * position_flag *  alident)
 | `Self of loc
 | `Tok of loc
 | `Type of ctyp ]

type entry   = {
  name : name ;
  (*position expession node *)    
  pos : exp option ;
  local : bool ;  (* mark whether the grammar is local or not*)
  levels : levels(* list level *);
}
and levels =
 [ `Group of (level list ) | `Single of level]   
and level  ={
  (* mainly used for indexing *)  
  label : string option ;
  assoc : exp option ;
  rules : rule list
}
and rule = {
  prod : symbol list ;
  action : exp option 
}
      
and symbol ={
  text : text;
  styp : styp;
  (* the inferred type of the result parsed by the current symbol *)
  pattern : pat option 
}
and text =
 [
   `Slist of (loc * bool * symbol * symbol option )
 | `Snterm of (loc * name  * string option )
 | `Sopt of (loc * text )
 | `Stry of (loc * text )
 | `Speek of (loc * text)
 | `Sself of loc
 | `Skeyword of (loc * string)
 | `Stok of (loc * exp * simple_pat(* FAstN.pat *) )
(** The first is the match function exp(predicate),
    the second and the third  is the string description.
    The description string will be used for
    grammar insertion and left factoring.
    Keep this string [normalized] and well comparable. *) ]
  |};;

(* type used = *)
(*   | Unused | UsedScanned | UsedNotScanned  *)





(* class foldbase = object *)
(*   inherit StdFan.foldbase *)
(*   method loc (x:loc) = x  *)
(* end *)


(* make [S] a keyword ? *) 
type action_pattern =
  [ vid
  |`Com of (loc * action_pattern * action_pattern)
  |`Par of (loc * action_pattern )
  |`Any of loc ];;



FConfig.antiquotations := true;;
open Fsyntax;;

{:create| Fgram (simple_pat : simple_pat Fgram.t) |};;

{:extend|
  simple_pat "pat'":
  ["`"; luident{s}  ->  {|$vrn:s|}

  |"`"; luident{v}; `Ant (("" | "anti" as n) ,s) ->
    {| $vrn:v $(FanUtil.mk_anti _loc ~c:"pat" n s)|}
  |"`"; luident{s}; `STR(_,v) -> {| $vrn:s $str:v|}
  |"`"; luident{s}; `Lid x  -> {| $vrn:s $lid:x |}
  |"`"; luident{s}; "_" -> {|$vrn:s _|}
  |"`"; luident{s}; "("; L1 internal_pat SEP ","{v}; ")" ->
      (AstLib.appl_of_list ({:pat'|$vrn:s|} :: v))
        (* here
           we have to guarantee
           {[
           {:pat-|`a(a,b,c)|};;
           - : FAstN.pat = `App (`App (`App (`Vrn "a", `Lid "a"), `Lid "b"), `Lid "c")
           ]}
           is dumped correctly
         *) ]
  let internal_pat "pat'": (* FIXME such grammar should be deprecated soon*)
  {
   "as"
     [S{p1} ; "as";`Lid s  -> {| ($p1 as $lid:s) |} ]
     "|"
     [S{p1}; "|"; S{p2}  -> {|$p1 | $p2 |} ]
     "simple"
     [ `STR(_,s) -> {| $str:s|}
     | "_" -> {| _ |}
     | `Lid x   ->  {| $lid:x|}
     | "("; S{p}; ")" -> p] }
|};;

open Format
let p = fprintf

let rec unparse_simple_pat  f (x:simple_pat)=
  match x with
  | `Vrn (_,s) -> p f "`%s" s
  | `App _ ->
      let l = AstLib.list_of_app x [] in
      begin match l with
      | [ (`Vrn _ as x) ]  -> unparse_simple_pat  f x
      | [ (`Vrn _ as  x) ; v ] ->
          p f "%a %a" unparse_simple_pat x unparse_simple_pat v
      | (`Vrn _ as x) :: rest ->
          p f "%a (%a)"
            unparse_simple_pat x (pp_list unparse_simple_pat ~sep:",") rest 
      | _ ->
          (p Format.err_formatter  "impossible pattern %a@." pp_print_simple_pat x ;
          invalid_arg "unparse_simple_pat")
      end
  | `Com(_,a,b) -> p f "%a, %a" unparse_simple_pat a unparse_simple_pat b
  | `Alias (_,p,_) -> unparse_simple_pat f  p

  | `Bar (_,a,b) -> p f "%a| %a" unparse_simple_pat a unparse_simple_pat b
  | `Str(_,s) -> p f "%S" s
  | `Any _ -> p f "_"
  | `Lid (_,s) -> p f "%s" s 
  | `Ant (_, {FanUtil.content=s;_}) -> p f "$%s" s

let string_of_simple_pat = to_string_of_printer unparse_simple_pat
(**
   `a
   `a $x
   `a "gh"
   `a x
   `a _
   `a ( v as y)
   `
 *)    
(* type imm = *)
(*   | Ant of string *)
(*   | I of string  *)
(* let unparse_simple_pat  (x : simple_pat)= *)
(*   match x with *)
(*   | `Vrn (_,s) -> {:ep|$`str:s|} *)
(*   | `App (_loc, `Vrn(_,v),`Ant(_,{FanUtil.contents=c;_})) -> *)
(*       {:ep| $`str:v ^ "$" ^ $lid:c |} *)
(*   | `App _  -> *)
(*       let l = AstLib.appl_of_list x in *)
(*       begin match l with *)
(*       | `Vrn(_,x) ::xs -> *)
          
(*       | _ -> assert false *)
(*       end *)
(*   | `Alias (_,p,_) -> unparse_simple_pat f p *)
(*   | *)
(* ;; *)
(* let t = object *)
(*   inherit Objs.fold as super; *)
(*   method  *)
(* end *)
