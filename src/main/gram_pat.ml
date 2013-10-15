%import{
StdFan:
  pp_print_string;
Objs:
  pp_print_vid'
  pp_print_vid
  pp_print_alident
  pp_print_ant;
Syntaxf:
  luident
  ;
Format:
  fprintf
  ;
}
open FAst
let pp_print_loc _f _loc  = ()
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
    | %pat'{ ($p as $_) } -> self#simple_pat p
    | p  -> super#simple_pat p 
end;;

%create{ Fgram (simple_pat : simple_pat Fgram.t) };;

%extend{
  simple_pat :
  ["`"; luident{s}   %pat'{$vrn:s}

  |"`"; luident{v}; `Ant (("" | "anti" as n) ,s) %pat'{ $vrn:v $(FanUtil.mk_anti _loc ~c:"pat" n s)}
  |"`"; luident{s}; `Str v   %pat'{ $vrn:s $str:v}
  |"`"; luident{s}; `Lid x   %pat'{ $vrn:s $lid:x }
  |"`"; luident{s}; "_"   %pat'{$vrn:s _}
  |"`"; luident{s}; "("; L1 internal_pat SEP ","{v}; ")" %{Ast_gen.appl_of_list (%pat'{$vrn:s} :: v)}
        (* here
           we have to guarantee
           {[
           %pat-{`a(a,b,c)};;
           - : FAstN.pat = `App (`App (`App (`Vrn "a", `Lid "a"), `Lid "b"), `Lid "c")
           ]}
           is dumped correctly
         *) ]
  let internal_pat "pat'": (* FIXME such grammar should be deprecated soon*)
  {
   "as"
     [S{p1} ; "as";`Lid s   %pat'{ ($p1 as $lid:s) } ]
     "|"
     [S{p1}; "|"; S{p2}    %pat'{$p1 | $p2 } ]
     "simple"
     [ `Str s    %pat'{ $str:s}
     | "_"    %pat'{ _ } 
     | `Lid x      %pat'{ $lid:x}
     | "("; S{p}; ")"  %{ p} ] }
};;


let p = fprintf

let rec unparse_simple_pat  f (x:simple_pat)=
  match x with
  | `Vrn (_,s) -> p f "`%s" s
  | `App _ ->
      let l = Ast_basic.list_of_app x [] in
      begin match l with
      | [ (`Vrn _ as x) ]  -> unparse_simple_pat  f x
      | [ (`Vrn _ as  x) ; v ] ->
          p f "%a %a" unparse_simple_pat x unparse_simple_pat v
      | (`Vrn _ as x) :: rest ->
          p f "%a (%a)"
            unparse_simple_pat x (Formatf.pp_list unparse_simple_pat ~sep:",") rest 
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

let string_of_simple_pat = Formatf.to_string unparse_simple_pat

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/gram_pat.cmo" *)
(* end: *)
