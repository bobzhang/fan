open Structure
open Format
type brothers = Bro of symbol * brothers list
let rec flatten_tree =
 function
 | DeadEnd -> ([])
 | LocAct (_ , _) -> [[] ]
 | Node ({node = n ; brother = b ; son = s}) ->
    (( (List.map ( fun l -> ( n ) :: l  ) ( (flatten_tree s) )) ) @ (
      (flatten_tree b) ))
class text_grammar =
 object (self : 'self)
  method tree =
   fun ppf ->
    fun t -> (self#level ppf Format.pp_print_space ( (flatten_tree t) ))
 method symbol =
  fun ppf ->
   function
   | (`Smeta (n , sl , _)) -> (self#meta ppf n sl)
   | (`Slist0 s) -> (fprintf ppf "LIST0 %a" ( self#symbol1 ) s)
   | (`Slist0sep (s , t)) ->
      (fprintf ppf "LIST0 %a SEP %a" ( self#symbol1 ) s ( self#symbol1 ) t)
   | (`Slist1 s) -> (fprintf ppf "LIST1 %a" ( self#symbol1 ) s)
   | (`Slist1sep (s , t)) ->
      (fprintf ppf "LIST1 %a SEP %a" ( self#symbol1 ) s ( self#symbol1 ) t)
   | (`Sopt s) -> (fprintf ppf "OPT %a" ( self#symbol1 ) s)
   | (`Stry s) -> (fprintf ppf "TRY %a" ( self#symbol1 ) s)
   | (`Snterml (e , l)) -> (fprintf ppf "%s@ Level@ %S" ( e.ename ) l)
   | (((((((`Snterm _) | `Snext) | `Sself) | (`Stree _)) | (`Stoken _))
       | (`Skeyword _)) as s) ->
      (self#symbol1 ppf s)
 method description =
  fun ppf -> function | `Normal -> () | `Antiquot -> (fprintf ppf "$")
 method symbol1 =
  fun ppf ->
   function
   | (`Snterm e) -> (pp_print_string ppf ( e.ename ))
   | `Sself -> (pp_print_string ppf "SELF")
   | `Snext -> (pp_print_string ppf "NEXT")
   | (`Stoken (_ , (description , content))) ->
      ( (self#description ppf description) ); (pp_print_string ppf content)
   | (`Skeyword s) -> (fprintf ppf "%S" s)
   | (`Stree t) -> (self#tree ppf t)
   | (((((((((`Smeta (_ , _ , _)) | (`Snterml (_ , _))) | (`Slist0 _))
           | (`Slist0sep (_ , _))) | (`Slist1 _)) | (`Slist1sep (_ , _)))
        | (`Sopt _)) | (`Stry _)) as s) ->
      (fprintf ppf "(%a)" ( self#symbol ) s)
 method meta =
  fun ppf ->
   fun n ->
    fun sl ->
     let rec loop =
      fun i ->
       function
       | [] -> ()
       | (s :: sl) ->
          let j =
           (try (String.index_from n i ' ') with
            Not_found -> (String.length n)) in
          (
          (fprintf ppf "%s %a" ( (String.sub n i ( (j - i) )) ) (
            self#symbol1 ) s)
          );
          if (sl = [] ) then () 
          else begin
           (
           (fprintf ppf " ")
           );
           (loop ( (min ( (j + 1) ) ( (String.length n) )) ) sl)
          end in
     (loop 0 sl)
 method rule =
  fun ppf ->
   fun symbols ->
    (
    (fprintf ppf "@[<hov 0>")
    );
    (
    (List.fold_left (
      fun sep ->
       fun symbol ->
        (
        (fprintf ppf "%t%a" sep ( self#symbol ) symbol)
        );
        fun ppf -> (fprintf ppf ";@ ") ) ( fun _ -> () ) symbols ppf)
    );
    (fprintf ppf "@]")
 method level =
  fun ppf ->
   fun space ->
    fun rules ->
     (
     (fprintf ppf "@[<hov 0>[ ")
     );
     (
     (List.fold_left (
       fun sep ->
        fun rule ->
         (
         (fprintf ppf "%t%a" sep ( self#rule ) rule)
         );
         fun ppf -> (fprintf ppf "%a| " space () ) ) ( fun _ -> () ) rules
       ppf)
     );
     (fprintf ppf " ]@]")
 method assoc =
  fun ppf ->
   function
   | `LA -> (fprintf ppf "LA")
   | `RA -> (fprintf ppf "RA")
   | `NA -> (fprintf ppf "NA")
 method levels =
  fun ppf ->
   fun elev ->
    ((List.fold_left (
       fun sep ->
        fun lev ->
         let rules =
          ((
            (List.map ( fun t -> ( `Sself ) :: t  ) (
              (flatten_tree ( lev.lsuffix )) )) ) @ (
            (flatten_tree ( lev.lprefix )) )) in
         (
         (fprintf ppf "%t@[<hov 2>" sep)
         );
         (
         (match lev.lname with
          | Some (n) -> (fprintf ppf "%S@;<1 2>" n)
          | None -> ())
         );
         (
         (self#assoc ppf ( lev.assoc ))
         );
         (
         (fprintf ppf "@]@;<1 2>")
         );
         (
         (self#level ppf Format.pp_force_newline rules)
         );
         fun ppf -> (fprintf ppf "@,| ") ) ( fun _ -> () ) elev ppf) : 
      unit)
 method entry =
  fun ppf ->
   fun e ->
    (begin
      (
     (fprintf ppf "@[<v 0>%s: [ " ( e.ename ))
     );
      (
     (match e.edesc with
      | Dlevels (elev) -> (self#levels ppf elev)
      | Dparser (_) -> (fprintf ppf "<parser>"))
     );
      (fprintf ppf " ]@]")
     end : unit)
 end
class dump_grammar =
 object (self : 'self)
  inherit text_grammar
 method! tree =
  fun ppf ->
   fun tree ->
    let rec get_brothers =
     fun acc ->
      function
      | DeadEnd -> (List.rev acc)
      | LocAct (_ , _) -> (List.rev acc)
      | Node ({node = n ; brother = b ; son = s}) ->
         (get_brothers ( ( ( (Bro (n , ( (get_brothers []  s) ))) ) ) :: acc 
           ) b)
    and print_brothers =
     fun ppf ->
      fun brothers ->
       if (brothers = [] ) then ( (fprintf ppf "@ []") )
       else
        (List.iter (
          fun (Bro (n , xs)) ->
           (
           (fprintf ppf "@ @[<hv2>- %a" ( self#symbol ) n)
           );
           (
           (match xs with
            | [] -> ()
            | (_ :: []) ->
               (try (print_children ppf ( (get_children []  xs) )) with
                Exit -> (fprintf ppf ":%a" print_brothers xs))
            | _ -> (fprintf ppf ":%a" print_brothers xs))
           );
           (fprintf ppf "@]") ) brothers)
    and print_children =
     fun ppf -> (List.iter ( (fprintf ppf ";@ %a" ( self#symbol )) ))
    and get_children =
     fun acc ->
      function
      | [] -> (List.rev acc)
      | (Bro (n , x) :: []) -> (get_children ( ( n ) :: acc  ) x)
      | _ -> (raise Exit ) in
    (print_brothers ppf ( (get_brothers []  tree) ))
 method! levels =
  fun ppf ->
   fun elev ->
    (List.fold_left (
      fun sep ->
       fun lev ->
        (
        (fprintf ppf "%t@[<v2>" sep)
        );
        (
        (match lev.lname with
         | Some (n) -> (fprintf ppf "%S@;<1 2>" n)
         | None -> ())
        );
        (
        (self#assoc ppf ( lev.assoc ))
        );
        (
        (fprintf ppf "@]@;<1 2>")
        );
        (
        (fprintf ppf "@[<v2>suffix:@ ")
        );
        (
        (self#tree ppf ( lev.lsuffix ))
        );
        (
        (fprintf ppf "@]@ @[<v2>prefix:@ ")
        );
        (
        (self#tree ppf ( lev.lprefix ))
        );
        (
        (fprintf ppf "@]")
        );
        fun ppf -> (fprintf ppf "@,| ") ) ( fun _ -> () ) elev ppf)
 end
let text = new text_grammar
let dump = new dump_grammar