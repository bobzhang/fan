
open Format

let pp = fprintf 


(* [print_node] prints one node and [print_sons] its children.
   [pref] is the prefix to output at the beginning of line
   and [start] is the branching drawing (["+-"] the first time, 
   and then ["|-"]). *)
let rec print_node decomp pref f t = 
  let (s, sons) = decomp t in begin
    pp f "%s" s;
    if sons <> [] then 
      let w = String.length s in
      let pref' = pref ^ String.make (w + 1) ' ' in
      match sons with
      | [t'] ->  pp f "---%a" (print_node decomp (pref' ^ "  ")) t'
      | _ -> pp f "-%a" (print_sons  "+-" decomp pref') sons 
      else ()
    end
and print_sons (start:string) (decomp:'a -> (string * 'a list))
    (pref:string) f = function
      | [] ->  () (* when entering into foreset *)
      | [s] -> pp f "`-%a" (print_node decomp (pref ^ "  ")) s
      | s :: sons ->  
          pp f "%s%a@\n%s%a"
            start (print_node decomp (pref ^ "| ")) s
            pref  (print_sons "|-"  decomp  pref ) sons 
let pp_assoc f  = function
    | `LA -> pp f "LA"
    | `RA -> pp f "RA"
    | `NA -> pp f "NA"           

class type grammar_print  = object
  method set_action : bool -> unit
  (* method description : formatter -> description -> unit *)
  method entry : formatter -> Gdefs.entry -> unit
  method level : formatter -> Gdefs.level -> unit
  method levels : formatter -> Gdefs.level list -> unit
  method rule : formatter -> Gdefs.symbol list -> unit
  method production :  formatter -> Gdefs.production -> unit
  method productions : formatter -> Gdefs.production list -> unit      
  method rules : formatter -> Gdefs.symbol list list -> unit
  method symbol : formatter -> Gdefs.symbol -> unit
  method symbol1 : formatter -> Gdefs.symbol -> unit
  method tree : formatter -> Gdefs.tree -> unit
end


class text_grammar : grammar_print = object(self:'self)
  val mutable action = true

  method set_action v = action <- v
      
  method symbol f (x:Gdefs.symbol) =
    match x with 
    | `List0 s -> pp f "L0 %a" self#symbol1 s
    | `List0sep (s, t) ->
        pp f "L0 %a SEP %a" self#symbol1 s self#symbol1 t
    | `List1 s -> pp f "L1 %a" self#symbol1 s
    | `List1sep (s, t) ->
        pp f "L1 %a SEP %a" self#symbol1 s self#symbol1 t
    (* | `Opt s -> pp f "OPT %a" self#symbol1 s *)
    | `Try s -> pp f "TRY %a" self#symbol1 s
    | `Peek s -> pp f "PEEK %a" self#symbol1 s 
    | `Snterml (e, l) -> pp f "%s Level %S" e.name l
    | `Nterm _  | `Self  | `Token _ | `Keyword _ as s ->
        self#symbol1 f s 
          
  method symbol1 f x =
    match (x: Gdefs.symbol) with 
    | `Nterm e -> pp f "%s" e.name
    | `Self -> pp f "%s" "S"
    | `Token p -> pp f "%s" (Tokenf.string_of_pattern p )
    | `Keyword s -> pp f "%S" s
    | `Snterml (_, _) | `List0 _ | `List0sep (_, _) | `List1 _ |
      `List1sep (_, _) (* | `Opt _ *) | `Try _ | `Peek _ as s ->
        pp f "(%a)" self#symbol s
  method production 
      f ((symbols,(annot,_action)):Gdefs.production) =
    if not action then
      pp f "@[<0>%a@]" (* action ignored*)
        (Formatf.pp_list self#symbol ~sep:";@;") symbols
    else
      pp f
        "@[<0>%a@;->@ @[%s@]@]"
        (Formatf.pp_list self#symbol ~sep:";@;") symbols
        annot
        
  method productions f ps =
    pp f "@[<hv0>%a@]"
      (Formatf.pp_list (self#production ) ~sep:"@;| "
         ~first:"[ " ~last:" ]") ps
  (* the same as production, but only print lhs, i.e. symbols*)    
  method rule f symbols= 
    pp f "@[<0>%a@]" (Formatf.pp_list self#symbol ~sep:";@ ") symbols
  method rules f  rules= 
    pp f "@[<hv0>[ %a]@]" (Formatf.pp_list self#rule ~sep:("@;| ")) rules
      
  method level f (x:Gdefs.level) =
    pp f "%a %a@;%a"
      (Formatf.pp_option (fun f s -> pp f "%S" s)) x.lname
      pp_assoc x.assoc (self#productions ) x.productions
 
          
  method levels f elev:unit =
    pp f "@[<hv0>  %a@]" (Formatf.pp_list self#level ~sep:"@;| ") elev
  method entry f (e:Gdefs.entry) :unit= 
    pp f "@[<2>%s:@;[%a]@]" e.name
      (fun f (e:Gdefs.entry) -> self#levels f e.levels) e

  (* used in dumping symbol [`Stree] *)    
  method tree f t = self#rules f  @@ Gtools.flatten_tree t
end

let text = new text_grammar

(* FIXME #a if I move this into the object, the output is different*)  
let string_of_symbol s = begin
  ignore (flush_str_formatter ());
  text#symbol str_formatter s;
  flush_str_formatter ()
end
  
class dump_grammar : grammar_print  = object(self:'self)
  inherit text_grammar 
  method! tree f tree = (* see FIXME #a*)
    print_sons "|-"
      (function (x:Gtools.brothers) ->
        match x with
        | Bro (s, ls) -> string_of_symbol s, ls
        | End -> ".",[]) "" f
      @@ Gtools.get_brothers tree
  method! level f (x:Gdefs.level)  =
    pp f "%a %a@;@[<hv2>cont:@\n%a@]@;@[<hv2>start:@\n%a@]"
      (Formatf.pp_option (fun f s -> pp f "%S" s)) x.lname
      pp_assoc x.assoc
      self#tree x.lsuffix
      self#tree x.lprefix 
end

let dump = new dump_grammar
    

(* local variables: *)
(* compile-command: "pmake gprint.cmo" *)
(* end: *)
