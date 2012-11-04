open Structure
open Format
let pp = fprintf
type brothers =  
  | Bro of symbol* brothers list 
type space_formatter = (unit,Format.formatter,unit) format 
class text_grammar =
  object (self : 'self)
    method tree f t = self#level f (flatten_tree t)
    method list :
      'a .
        ?sep:space_formatter ->
          ?first:space_formatter ->
            ?last:space_formatter ->
              (Format.formatter -> 'a -> unit) ->
                Format.formatter -> 'a list -> unit=
      fun ?sep  ?first  ?last  fu  f  xs  ->
        let first = match first with | Some x -> x | None  -> ""
        and last = match last with | Some x -> x | None  -> ""
        and sep = match sep with | Some x -> x | None  -> "@ " in
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
    method symbol f =
      function
      | `Smeta (n,sl,_) -> self#meta n f sl
      | `Slist0 s -> pp f "LIST0 %a" self#symbol1 s
      | `Slist0sep (s,t) ->
          pp f "LIST0 %a SEP %a" self#symbol1 s self#symbol1 t
      | `Slist1 s -> pp f "LIST1 %a" self#symbol1 s
      | `Slist1sep (s,t) ->
          pp f "LIST1 %a SEP %a" self#symbol1 s self#symbol1 t
      | `Sopt s -> pp f "OPT %a" self#symbol1 s
      | `Stry s -> pp f "TRY %a" self#symbol1 s
      | `Snterml (e,l) -> pp f "%s@ Level@ %S" e.ename l
      | `Snterm _|`Snext|`Sself|`Stree _|`Stoken _|`Skeyword _ as s ->
          self#symbol1 f s
    method description f = function | `Normal -> () | `Antiquot -> pp f "$"
    method symbol1 f =
      function
      | `Snterm e -> pp f "%s" e.ename
      | `Sself -> pp f "%s" "SELF"
      | `Snext -> pp f "%s" "NEXT"
      | `Stoken (_,(description,content)) ->
          pp f "%a%s" self#description description content
      | `Skeyword s -> pp f "%S" s
      | `Stree t -> self#tree f t
      | `Smeta (_,_,_)|`Snterml (_,_)|`Slist0 _|`Slist0sep (_,_)|`Slist1 _|
          `Slist1sep (_,_)|`Sopt _|`Stry _ as s -> pp f "(%a)" self#symbol s
    method meta n f sl =
      let rec loop i =
        function
        | [] -> ()
        | s::sl ->
            let j =
              try String.index_from n i ' '
              with | Not_found  -> String.length n in
            (pp f "%s %a" (String.sub n i (j - i)) self#symbol1 s;
             if sl = []
             then ()
             else (pp f " "; loop (min (j + 1) (String.length n)) sl)) in
      loop 0 sl
    method rule f symbols =
      pp f "@[<0>%a@]" (self#list self#symbol ~sep:";@ ") symbols
    method level ?space:(space : space_formatter option)  f rules =
      let space =
        match space with | None  -> ("@ " : space_formatter ) | Some x -> x in
      pp f "@[<0>[@;%a@;]@]" (self#list self#rule ~sep:("|" ^^ space)) rules
    method assoc f =
      function | `LA -> pp f "LA" | `RA -> pp f "RA" | `NA -> pp f "NA"
    method levels f elev =
      (List.fold_left
         (fun sep  lev  ->
            let rules =
              (List.map (fun t  -> `Sself :: t) (flatten_tree lev.lsuffix)) @
                (flatten_tree lev.lprefix) in
            pp f "%t@[<hov 2>" sep;
            (match lev.lname with
             | Some n -> pp f "%S@;<1 2>" n
             | None  -> ());
            self#assoc f lev.assoc;
            pp f "@]@;<1 2>";
            self#level ~space:"@\n" f rules;
            (fun f  -> pp f "@,| ")) (fun _  -> ()) elev f : unit
      )
    method entry f e =
      (pp f "@[<v 0>%s: [ " e.ename;
       (match e.edesc with
        | Dlevels elev -> self#levels f elev
        | Dparser _ -> pp f "<parser>");
       pp f " ]@]" : unit
      )
  end
class dump_grammar =
  object (self : 'self)
    inherit  text_grammar
    method! tree f tree =
      let rec get_brothers acc =
                function
                | DeadEnd  -> List.rev acc
                | LocAct (_,_) -> List.rev acc
                | Node { node = n; brother = b; son = s } ->
                    get_brothers ((Bro (n, (get_brothers [] s))) :: acc) b
      and print_brothers f brothers =
            if brothers = []
            then pp f "@ []"
            else
              List.iter
                (fun (Bro (n,xs))  ->
                   pp f "@ @[<hv2>- %a" self#symbol n;
                   (match xs with
                    | [] -> ()
                    | _::[] ->
                        (try print_children f (get_children [] xs)
                         with | Exit  -> pp f ":%a" print_brothers xs)
                    | _ -> pp f ":%a" print_brothers xs);
                   pp f "@]") brothers
      and print_children f = List.iter (pp f ";@ %a" self#symbol)
      and get_children acc =
            function
            | [] -> List.rev acc
            | (Bro (n,x))::[] -> get_children (n :: acc) x
            | _ -> raise Exit in
      print_brothers f (get_brothers [] tree)
    method! levels f elev =
      List.fold_left
        (fun sep  lev  ->
           pp f "%t@[<v2>" sep;
           (match lev.lname with | Some n -> pp f "%S@;<1 2>" n | None  -> ());
           self#assoc f lev.assoc;
           pp f "@]@;<1 2>";
           pp f "@[<v2>suffix:@ ";
           self#tree f lev.lsuffix;
           pp f "@]@ @[<v2>prefix:@ ";
           self#tree f lev.lprefix;
           pp f "@]";
           (fun f  -> pp f "@,| ")) (fun _  -> ()) elev f
  end
let text = new text_grammar
let dump = new dump_grammar