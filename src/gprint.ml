open Gstructure
open Format
open LibUtil
open Gstru

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

class type grammar_print  = object
  method assoc : formatter -> assoc -> unit
  method description : formatter -> description -> unit
  method entry : formatter -> entry -> unit
  method level : formatter -> level -> unit
  method levels : formatter -> level list -> unit
  method list :
      ?sep:space_formatter ->
        ?first:space_formatter ->
          ?last:space_formatter ->
            (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
  method meta :
      string list -> formatter -> symbol list -> unit
  method option :
      ?first:space_formatter ->
        ?last:space_formatter ->
          (formatter -> 'a -> unit) ->
            formatter -> 'a option -> unit
  method rule : formatter -> symbol list -> unit
  method production : ?action:bool -> formatter -> production -> unit
  method productions : ?action:bool -> formatter -> production list -> unit      
  method rules : formatter -> symbol list list -> unit
  method symbol : formatter -> symbol -> unit
  method symbol1 : formatter -> symbol -> unit
  method tree : formatter -> tree -> unit
end
      
      
class text_grammar : grammar_print = object(self:'self)
  method tree f t = self#rules f  (flatten_tree t)
  method list :  ! 'a .
      ?sep:space_formatter -> ?first:space_formatter ->
        ?last:space_formatter -> (Format.formatter -> 'a -> unit) ->
          Format.formatter ->  'a list -> unit
              =
            fun  ?sep ?first  ?last fu f xs
              -> 
                let first = Option.default ("":space_formatter) first in
                let last = Option.default ("":space_formatter) last in
                let sep = Option.default ("@ ":space_formatter) sep in
                let aux f = function
                  | [] -> ()
                  | [x] -> fu f x
                  | xs ->
                      let rec loop  f = function
                        | [x] -> fu f x
                        | x::xs ->  pp f "%a%(%)%a" fu x sep loop xs 
                        | _ -> assert false  in 
                            pp f "%(%)%a%(%)" first loop xs last in
                aux f xs
  method option : ! 'a . ?first:space_formatter -> ?last:space_formatter ->
    (Format.formatter -> 'a -> unit) -> Format.formatter ->  'a option -> unit =
      fun  ?first  ?last fu f a ->
        let first =
          match first with
          | Some x -> x
          | None -> ""
        and last =
          match last with
          | Some x -> x
          | None -> ""  in
        match a with
        | None -> ()
        | Some x -> pp f "%(%)%a%(%)" first fu x last
  method symbol f =  function
    | `Smeta (n, sl, _) -> self#meta n f  sl
    | `Slist0 s -> pp f "LIST0 %a" self#symbol1 s
    | `Slist0sep (s, t) ->
        pp f "LIST0 %a SEP %a" self#symbol1 s self#symbol1 t
    | `Slist1 s -> pp f "LIST1 %a" self#symbol1 s
    | `Slist1sep (s, t) ->
        pp f "LIST1 %a SEP %a" self#symbol1 s self#symbol1 t
    | `Sopt s -> pp f "OPT %a" self#symbol1 s
    | `Stry s -> pp f "TRY %a" self#symbol1 s
    | `Speek s -> pp f "PEEK %a" self#symbol1 s 
    | `Snterml (e, l) -> pp f "%s Level %S" e.ename l
    | `Snterm _ | `Snext | `Sself | `Stree _ | `Stoken _ | `Skeyword _ as s ->
        self#symbol1 f s 
  method meta ns f  sl=
    match ns with
    | [x] ->
        pp f "%s@;%a" x (self#list self#symbol ) sl
    | [x;y] ->
        let l = List.length sl in
        let (a,b) = List.split_at (l-1) sl in 
        pp f "%s@;%a@;%s@;%a" x (self#list self#symbol) a y  (self#list self#symbol) b
    | _ -> invalid_arg "meta in print"   
  method description f = function
    | `Normal -> ()
    | `Antiquot -> pp f "$"
  method symbol1 f = function
    | `Snterm e -> pp f "%s" e.ename
    | `Sself -> pp f "%s" "S"
    | `Snext -> pp f "%s" "N" 
    | `Stoken (_, (description,content)) ->
        pp f "%a%s" self#description description content
    | `Skeyword s -> pp f "%S" s
    | `Stree t -> self#tree f t
    | `Smeta (_, _, _) | `Snterml (_, _) | `Slist0 _ | `Slist0sep (_, _) | `Slist1 _ |
      `Slist1sep (_, _) | `Sopt _ | `Stry _ | `Speek _ as s ->
        pp f "(%a)" self#symbol s
  method production ?(action=false)
      f ((symbols,(annot,_action)):production) =
    if not action then 
      pp f "@[<0>%a@]" (* action ignored*)
        (self#list self#symbol ~sep:";@;") symbols
    else
      pp f
        "@[<0>%a@;->@ @[%s@]@]"
        (self#list self#symbol ~sep:";@;") symbols
        annot
        
  method productions ?(action=false) f ps =
    pp f "@[<0>%a@]"
      (self#list (self#production ~action) ~sep:"@;| "
         ~first:"[ " ~last:" ]") ps 
  method rule f symbols= 
    pp f "@[<0>%a@]" (self#list self#symbol ~sep:";@ ") symbols
  method rules f  rules= 
    pp f "@[<hv0>[ %a]@]" (self#list self#rule ~sep:("@;| ")) rules
  method level f  = fun {assoc;lname;lsuffix;lprefix;_} ->
    (* FIXME we have original [productions] not used *)
    let rules =
      List.map (fun t  -> `Sself :: t) (flatten_tree lsuffix) @ flatten_tree lprefix in 
    pp f "%a %a@;%a" (self#option (fun f s -> pp f "%S" s)) lname self#assoc assoc self#rules rules 
  method assoc f = function
    | `LA -> pp f "LA"
    | `RA -> pp f "RA"
    | `NA -> pp f "NA" 
          
  method levels f elev:unit =
    pp f "@[<hv0>  %a@]" (self#list self#level ~sep:"@;| ") elev
  method entry f e :unit= begin
    pp f "@[<2>%s:@;[%a]@]" e.ename
      (fun f e ->
        match e.edesc with
        |Dlevels elev -> self#levels f elev
        |Dparser _ -> pp f "<parser>"
      ) e
  end
end

let text = new text_grammar

(* FIXME if I move this into the object, the output is different*)  
let string_of_symbol s = begin
  ignore (flush_str_formatter ());
  text#symbol str_formatter s;
  flush_str_formatter ()
end
  
class dump_grammar : grammar_print  = object(self:'self)
  inherit text_grammar ;
  method! tree f tree =
    (* let string_of_symbol s = begin *)
    (*   ignore (flush_str_formatter ()); *)
    (*   self#symbol str_formatter s; *)
    (*   flush_str_formatter () *)
    (* end in *)
    print_sons "|-"
      (function
        | Bro (s, ls) -> (string_of_symbol s, ls) | End -> (".",[])) "" f
      (get_brothers tree)
  method! level f =
    function {assoc;lname;lsuffix;lprefix;_} ->
    (* FIXME the original [productions] not used *)
    pp f "%a %a@;@[<hv2>suffix:@\n%a@]@;@[<hv2>prefix:@\n%a@]"
      (self#option (fun f s -> pp f "%S" s)) lname
      self#assoc assoc
      self#tree lsuffix
      self#tree lprefix 
end

let dump = new dump_grammar
    
