open Gstructure

open Format

open LibUtil

open Gstru

let pp = fprintf

let rec print_node decomp pref f t =
  let (s,sons) = decomp t in
  begin
    pp f "%s" s;
    if sons <> []
    then
      (let w = String.length s in
       let pref' = pref ^ (String.make (w + 1) ' ') in
       match sons with
       | t'::[] -> pp f "---%a" (print_node decomp (pref' ^ "  ")) t'
       | _ -> pp f "-%a" (print_sons "+-" decomp pref') sons)
    else ()
  end
and print_sons (start : string) (decomp : 'a -> (string * 'a list))
  (pref : string) f =
  function
  | [] -> ()
  | s::[] -> pp f "`-%a" (print_node decomp (pref ^ "  ")) s
  | s::sons ->
      pp f "%s%a@\n%s%a" start (print_node decomp (pref ^ "| ")) s pref
        (print_sons "|-" decomp pref) sons

let pp_list ?sep  ?first  ?last  fu f xs =
  let first = Option.default ("" : space_formatter ) first in
  let last = Option.default ("" : space_formatter ) last in
  let sep = Option.default ("@ " : space_formatter ) sep in
  let aux f =
    function
    | [] -> ()
    | x::[] -> fu f x
    | xs ->
        let rec loop f =
          function
          | x::[] -> fu f x
          | x::xs -> pp f "%a%(%)%a" fu x sep loop xs
          | _ -> assert false in
        pp f "%(%)%a%(%)" first loop xs last in
  aux f xs

let pp_option:
  ?first:space_formatter ->
    ?last:space_formatter ->
      (Format.formatter -> 'a -> unit) ->
        Format.formatter -> 'a option -> unit
  =
  fun ?first  ?last  fu  f  a  ->
    let first = match first with | Some x -> x | None  -> ""
    and last = match last with | Some x -> x | None  -> "" in
    match a with | None  -> () | Some x -> pp f "%(%)%a%(%)" first fu x last

let pp_assoc f =
  function | `LA -> pp f "LA" | `RA -> pp f "RA" | `NA -> pp f "NA"

class type grammar_print
  =
  object 
    method description : formatter -> description -> unit
    method entry : formatter -> entry -> unit
    method level : formatter -> level -> unit
    method levels : formatter -> level list -> unit
    method rule : formatter -> symbol list -> unit
    method production : ?action:bool -> formatter -> production -> unit
    method productions : ?action:bool -> formatter -> production list -> unit
    method rules : formatter -> symbol list list -> unit
    method symbol : formatter -> symbol -> unit
    method symbol1 : formatter -> symbol -> unit
    method tree : formatter -> tree -> unit
  end

class text_grammar : grammar_print =
  object (self : 'self)
    val mutable action = false
    method symbol f =
      function
      | `Slist0 s -> pp f "L0 %a" self#symbol1 s
      | `Slist0sep (s,t) -> pp f "L0 %a SEP %a" self#symbol1 s self#symbol1 t
      | `Slist1 s -> pp f "L1 %a" self#symbol1 s
      | `Slist1sep (s,t) -> pp f "L1 %a SEP %a" self#symbol1 s self#symbol1 t
      | `Sopt s -> pp f "OPT %a" self#symbol1 s
      | `Stry s -> pp f "TRY %a" self#symbol1 s
      | `Speek s -> pp f "PEEK %a" self#symbol1 s
      | `Snterml (e,l) -> pp f "%s Level %S" e.ename l
      | `Snterm _|`Snext|`Sself|`Stree _|`Stoken _|`Skeyword _ as s ->
          self#symbol1 f s
    method description f = function | `Normal -> () | `Antiquot -> pp f "$"
    method symbol1 f =
      function
      | `Snterm e -> pp f "%s" e.ename
      | `Sself -> pp f "%s" "S"
      | `Snext -> pp f "%s" "N"
      | `Stoken (_,(description,content)) ->
          pp f "%a%s" self#description description content
      | `Skeyword s -> pp f "%S" s
      | `Stree t -> self#tree f t
      | `Snterml (_,_)|`Slist0 _|`Slist0sep (_,_)|`Slist1 _|`Slist1sep (_,_)
        |`Sopt _|`Stry _|`Speek _ as s -> pp f "(%a)" self#symbol s
    method production ?(action= false)  f
      ((symbols,(annot,_action)) : production) =
      if not action
      then pp f "@[<0>%a@]" (pp_list self#symbol ~sep:";@;") symbols
      else
        pp f "@[<0>%a@;->@ @[%s@]@]" (pp_list self#symbol ~sep:";@;") symbols
          annot
    method productions ?(action= false)  f ps =
      pp f "@[<hv0>%a@]"
        (pp_list (self#production ~action) ~sep:"@;| " ~first:"[ " ~last:" ]")
        ps
    method rule f symbols =
      pp f "@[<0>%a@]" (pp_list self#symbol ~sep:";@ ") symbols
    method rules f rules =
      pp f "@[<hv0>[ %a]@]" (pp_list self#rule ~sep:"@;| ") rules
    method level f { assoc; lname; productions;_} =
      pp f "%a %a@;%a" (pp_option (fun f  s  -> pp f "%S" s)) lname pp_assoc
        assoc (self#productions ~action:true) productions
    method levels f elev =
      (pp f "@[<hv0>  %a@]" (pp_list self#level ~sep:"@;| ") elev : unit )
    method entry f e =
      (pp f "@[<2>%s:@;[%a]@]" e.ename
         (fun f  e  ->
            match e.edesc with
            | Dlevels elev -> self#levels f elev
            | Dparser _ -> pp f "<parser>") e : unit )
    method tree f t = self#rules f (flatten_tree t)
  end

let text = new text_grammar

let string_of_symbol s =
  begin
    ignore (flush_str_formatter ()); text#symbol str_formatter s;
    flush_str_formatter ()
  end

class dump_grammar : grammar_print =
  object (self : 'self)
    inherit  text_grammar
    method! tree f tree =
      print_sons "|-"
        (function
         | Bro (s,ls) -> ((string_of_symbol s), ls)
         | End  -> (".", [])) "" f (get_brothers tree)
    method! level f { assoc; lname; lsuffix; lprefix;_} =
      pp f "%a %a@;@[<hv2>cont:@\n%a@]@;@[<hv2>start:@\n%a@]"
        (pp_option (fun f  s  -> pp f "%S" s)) lname pp_assoc assoc self#tree
        lsuffix self#tree lprefix
  end

let dump = new dump_grammar