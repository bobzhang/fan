open Structure;
open Format;
type brothers = [ Bro of symbol and list brothers ];
let rec flatten_tree = fun
  [ DeadEnd -> []
  | LocAct _ _ -> [[]]
  | Node {node = n; brother = b; son = s} ->
      [ [n :: l] | l <- flatten_tree s ] @ flatten_tree b ];

class text_grammar= object(self:'self)
  method tree ppf t = self#level ppf Format.pp_print_space (flatten_tree t);
  method symbol ppf =  fun
    [ `Smeta n sl _ -> self#meta ppf n sl
    | `Slist0 s -> fprintf ppf "LIST0 %a" self#symbol1 s
    | `Slist0sep s t ->
        fprintf ppf "LIST0 %a SEP %a" self#symbol1 s self#symbol1 t
    | `Slist1 s -> fprintf ppf "LIST1 %a" self#symbol1 s
    | `Slist1sep s t ->
        fprintf ppf "LIST1 %a SEP %a" self#symbol1 s self#symbol1 t
    | `Sopt s -> fprintf ppf "OPT %a" self#symbol1 s
    | `Stry s -> fprintf ppf "TRY %a" self#symbol1 s
    | `Snterml e l -> fprintf ppf "%s@ Level@ %S" e.ename l
    | `Snterm _ | `Snext | `Sself | `Stree _ | `Stoken _ | `Skeyword _ as s ->
        self#symbol1 ppf s ];
  method description ppf = fun
    [ `Normal -> ()
    | `Antiquot -> fprintf ppf "$"];
  method symbol1 ppf = fun
    [ `Snterm e -> pp_print_string ppf e.ename
    | `Sself -> pp_print_string ppf "SELF"
    | `Snext -> pp_print_string ppf "NEXT"
    | `Stoken (_, (description,content)) -> begin 
        self#description ppf description;
        pp_print_string ppf content
    end
    | `Skeyword s -> fprintf ppf "%S" s
    | `Stree t -> self#tree ppf t
    | `Smeta _ _ _ | `Snterml _ _ | `Slist0 _ | `Slist0sep _ _ | `Slist1 _ |
      `Slist1sep _ _ | `Sopt _ | `Stry _ as s -> fprintf ppf "(%a)" self#symbol s ];
  method meta ppf n sl=
    let rec loop i =fun
    [ [] -> ()
    | [s :: sl] ->
          let j = try String.index_from n i ' '
          with [ Not_found -> String.length n ] in begin 
            fprintf ppf "%s %a" (String.sub n i (j - i)) self#symbol1 s;
            if sl = [] then ()
            else do { fprintf ppf " "; loop (min (j + 1) (String.length n)) sl }
          end ] in
    loop 0 sl ;
  method rule ppf symbols= begin
    fprintf ppf "@[<hov 0>";
    List.fold_left
      (fun sep symbol ->begin
        fprintf ppf "%t%a" sep self#symbol symbol;
        fun ppf -> fprintf ppf ";@ "
      end) (fun _ -> ()) symbols ppf;
    fprintf ppf "@]"
  end;
  method level ppf space rules= begin
    fprintf ppf "@[<hov 0>[ ";
    List.fold_left
          (fun sep rule -> begin
              fprintf ppf "%t%a" sep self#rule rule;
              fun ppf -> fprintf ppf "%a| " space ()
            end)
          (fun _ -> ()) rules ppf ; (* BUGFIX the original version misses [ppf]*)
    fprintf ppf " ]@]"
  end;
  method assoc ppf = fun
  [ `LA -> fprintf ppf "LA"
  | `RA -> fprintf ppf "RA"
  | `NA -> fprintf ppf "NA" ];
    
  method levels ppf elev:unit =
    List.fold_left (fun sep lev ->
      let rules =
        [ [`Sself :: t] | t <- flatten_tree lev.lsuffix ] @
        flatten_tree lev.lprefix    in begin 
          fprintf ppf "%t@[<hov 2>" sep;
          match lev.lname with
          [ Some n -> fprintf ppf "%S@;<1 2>" n
          | None -> () ];
          self#assoc ppf  lev.assoc ;
          fprintf ppf "@]@;<1 2>";
          self#level ppf Format.pp_force_newline rules;
          fun ppf -> fprintf ppf "@,| "
          end)
      (fun _ -> ()) elev ppf;
  method entry ppf e :unit= begin
    fprintf ppf "@[<v 0>%s: [ " e.ename;
    match e.edesc with
    [ Dlevels elev -> self#levels ppf elev
    | Dparser _ -> fprintf ppf "<parser>" ];
    fprintf ppf " ]@]"
  end;
end;

class dump_grammar = object(self:'self)
  inherit text_grammar ;
  method! tree ppf tree =
    let rec get_brothers acc =  fun
      [ DeadEnd -> List.rev acc
      | LocAct _ _ -> List.rev acc
      | Node {node = n; brother = b; son = s} ->
          get_brothers [Bro n (get_brothers [] s) :: acc] b ]
    and print_brothers ppf brothers =
      if brothers = [] then
        fprintf ppf "@ []"
      else
        List.iter (fun [ Bro n xs -> begin
          fprintf ppf "@ @[<hv2>- %a" self#symbol n;
          match xs with
          [ [] -> ()
          | [_] -> try print_children ppf (get_children [] xs)
                   with [ Exit -> fprintf ppf ":%a" print_brothers xs ]
          | _ -> fprintf ppf ":%a" print_brothers xs ];
          fprintf ppf "@]";
        end]) brothers
    and print_children ppf = List.iter (fprintf ppf ";@ %a" self#symbol)
    and get_children acc =
      fun
      [ [] -> List.rev acc
      | [Bro n x] -> get_children [n::acc] x
      | _ -> raise Exit ]
    in print_brothers ppf (get_brothers [] tree);
  method! levels ppf elev =
    List.fold_left
      (fun sep lev -> begin
        fprintf ppf "%t@[<v2>" sep;
        match lev.lname with
        [ Some n -> fprintf ppf "%S@;<1 2>" n
        | None -> () ];
        self#assoc ppf lev.assoc;
        fprintf ppf "@]@;<1 2>";
        fprintf ppf "@[<v2>suffix:@ ";
        self#tree ppf lev.lsuffix;
        fprintf ppf "@]@ @[<v2>prefix:@ ";
        self#tree ppf lev.lprefix;
        fprintf ppf "@]";
        fun ppf -> fprintf ppf "@,| "
      end)
      (fun _ -> ()) elev ppf;
end;
let text = new text_grammar;
let dump = new dump_grammar;
    
