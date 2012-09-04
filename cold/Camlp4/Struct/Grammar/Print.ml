module Make =
 functor (Structure : Structure.S) ->
  struct
   open Structure

   open Format

   open Sig.Grammar

   let rec flatten_tree =
    function
    | DeadEnd -> ([])
    | LocAct (_, _) -> [[] ]
    | Node ({node = n; brother = b; son = s}) ->
       (( (List.map ( fun l -> ( n ) :: l  ) ( (flatten_tree s) )) ) @ (
         (flatten_tree b) ))

   let rec print_symbol =
    fun ppf ->
     function
     | Smeta (n, sl, _) -> (print_meta ppf n sl)
     | Slist0 (s) -> (fprintf ppf "LIST0 %a" print_symbol1 s)
     | Slist0sep (s, t) ->
        (fprintf ppf "LIST0 %a SEP %a" print_symbol1 s print_symbol1 t)
     | Slist1 (s) -> (fprintf ppf "LIST1 %a" print_symbol1 s)
     | Slist1sep (s, t) ->
        (fprintf ppf "LIST1 %a SEP %a" print_symbol1 s print_symbol1 t)
     | Sopt (s) -> (fprintf ppf "OPT %a" print_symbol1 s)
     | Stry (s) -> (fprintf ppf "TRY %a" print_symbol1 s)
     | Snterml (e, l) -> (fprintf ppf "%s@ LEVEL@ %S" ( e.ename ) l)
     | ((((((Snterm (_) | Snext) | Sself) | Stree (_)) | Stoken (_))
         | Skeyword (_)) as s) ->
        (print_symbol1 ppf s)
   and print_meta =
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
            (fprintf ppf "%s %a" ( (String.sub n i ( (j - i) )) )
              print_symbol1 s)
            );
            if (sl = [] ) then () 
            else begin
             (
             (fprintf ppf " ")
             );
             (loop ( (min ( (j + 1) ) ( (String.length n) )) ) sl)
            end in
       (loop 0 sl)
   and print_symbol1 =
    fun ppf ->
     function
     | Snterm (e) -> (pp_print_string ppf ( e.ename ))
     | Sself -> (pp_print_string ppf "SELF")
     | Snext -> (pp_print_string ppf "NEXT")
     | Stoken (_, descr) -> (pp_print_string ppf descr)
     | Skeyword (s) -> (fprintf ppf "%S" s)
     | Stree (t) -> (print_level ppf pp_print_space ( (flatten_tree t) ))
     | ((((((((Smeta (_, _, _) | Snterml (_, _)) | Slist0 (_))
             | Slist0sep (_, _)) | Slist1 (_)) | Slist1sep (_, _))
          | Sopt (_)) | Stry (_)) as s) ->
        (fprintf ppf "(%a)" print_symbol s)
   and print_rule =
    fun ppf ->
     fun symbols ->
      (
      (fprintf ppf "@[<hov 0>")
      );
      let _ =
       (List.fold_left (
         fun sep ->
          fun symbol ->
           (
           (fprintf ppf "%t%a" sep print_symbol symbol)
           );
           fun ppf -> (fprintf ppf ";@ ") ) ( fun _ -> () ) symbols) in
      (fprintf ppf "@]")
   and print_level =
    fun ppf ->
     fun pp_print_space ->
      fun rules ->
       (
       (fprintf ppf "@[<hov 0>[ ")
       );
       let _ =
        (List.fold_left (
          fun sep ->
           fun rule ->
            (
            (fprintf ppf "%t%a" sep print_rule rule)
            );
            fun ppf -> (fprintf ppf "%a| " pp_print_space () ) ) (
          fun _ -> () ) rules) in
       (fprintf ppf " ]@]")

   let levels =
    fun ppf ->
     fun elev ->
      let _ =
       (List.fold_left (
         fun sep ->
          fun lev ->
           let rules =
            ((
              (List.map ( fun t -> ( Sself  ) :: t  ) (
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
           (match lev.assoc with
            | LeftA -> (fprintf ppf "LEFTA")
            | RightA -> (fprintf ppf "RIGHTA")
            | NonA -> (fprintf ppf "NONA"))
           );
           (
           (fprintf ppf "@]@;<1 2>")
           );
           (
           (print_level ppf pp_force_newline rules)
           );
           fun ppf -> (fprintf ppf "@,| ") ) ( fun _ -> () ) elev) in
      ()

   let entry =
    fun ppf ->
     fun e ->
      (
      (fprintf ppf "@[<v 0>%s: [ " ( e.ename ))
      );
      (
      (match e.edesc with
       | Dlevels (elev) -> (levels ppf elev)
       | Dparser (_) -> (fprintf ppf "<parser>"))
      );
      (fprintf ppf " ]@]")

  end

module MakeDump =
        functor (Structure : Structure.S) ->
         struct
          open Structure

          open Format

          open Sig.Grammar

          type brothers = Bro of symbol * brothers list

          let rec print_tree =
           fun ppf ->
            fun tree ->
             let rec get_brothers =
              fun acc ->
               function
               | DeadEnd -> (List.rev acc)
               | LocAct (_, _) -> (List.rev acc)
               | Node ({node = n; brother = b; son = s}) ->
                  (get_brothers (
                    ( ( (Bro (n, ( (get_brothers []  s) ))) ) ) :: acc  )
                    b)
             and print_brothers =
              fun ppf ->
               fun brothers ->
                if (brothers = [] ) then ( (fprintf ppf "@ []") )
                else
                 (List.iter (
                   fun (Bro (n, xs)) ->
                    (
                    (fprintf ppf "@ @[<hv2>- %a" print_symbol n)
                    );
                    (
                    (match xs with
                     | [] -> ()
                     | (_ :: []) ->
                        (try
                          (print_children ppf ( (get_children []  xs) ))
                         with
                         Exit -> (fprintf ppf ":%a" print_brothers xs))
                     | _ -> (fprintf ppf ":%a" print_brothers xs))
                    );
                    (fprintf ppf "@]") ) brothers)
             and print_children =
              fun ppf ->
               (List.iter ( (fprintf ppf ";@ %a" print_symbol) ))
             and get_children =
              fun acc ->
               function
               | [] -> (List.rev acc)
               | (Bro (n, x) :: []) -> (get_children ( ( n ) :: acc  ) x)
               | _ -> (raise Exit ) in
             (print_brothers ppf ( (get_brothers []  tree) ))
          and print_symbol =
           fun ppf ->
            function
            | Smeta (n, sl, _) -> (print_meta ppf n sl)
            | Slist0 (s) -> (fprintf ppf "LIST0 %a" print_symbol1 s)
            | Slist0sep (s, t) ->
               (fprintf ppf "LIST0 %a SEP %a" print_symbol1 s
                 print_symbol1 t)
            | Slist1 (s) -> (fprintf ppf "LIST1 %a" print_symbol1 s)
            | Slist1sep (s, t) ->
               (fprintf ppf "LIST1 %a SEP %a" print_symbol1 s
                 print_symbol1 t)
            | Sopt (s) -> (fprintf ppf "OPT %a" print_symbol1 s)
            | Stry (s) -> (fprintf ppf "TRY %a" print_symbol1 s)
            | Snterml (e, l) ->
               (fprintf ppf "%s@ LEVEL@ %S" ( e.ename ) l)
            | ((((((Snterm (_) | Snext) | Sself) | Stree (_))
                 | Stoken (_)) | Skeyword (_)) as s) ->
               (print_symbol1 ppf s)
          and print_meta =
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
                   (fprintf ppf "%s %a" ( (String.sub n i ( (j - i) )) )
                     print_symbol1 s)
                   );
                   if (sl = [] ) then () 
                   else begin
                    (
                    (fprintf ppf " ")
                    );
                    (loop ( (min ( (j + 1) ) ( (String.length n) )) ) sl)
                   end in
              (loop 0 sl)
          and print_symbol1 =
           fun ppf ->
            function
            | Snterm (e) -> (pp_print_string ppf ( e.ename ))
            | Sself -> (pp_print_string ppf "SELF")
            | Snext -> (pp_print_string ppf "NEXT")
            | Stoken (_, descr) -> (pp_print_string ppf descr)
            | Skeyword (s) -> (fprintf ppf "%S" s)
            | Stree (t) -> (print_tree ppf t)
            | ((((((((Smeta (_, _, _) | Snterml (_, _)) | Slist0 (_))
                    | Slist0sep (_, _)) | Slist1 (_)) | Slist1sep (_, _))
                 | Sopt (_)) | Stry (_)) as s) ->
               (fprintf ppf "(%a)" print_symbol s)
          and print_rule =
           fun ppf ->
            fun symbols ->
             (
             (fprintf ppf "@[<hov 0>")
             );
             let _ =
              (List.fold_left (
                fun sep ->
                 fun symbol ->
                  (
                  (fprintf ppf "%t%a" sep print_symbol symbol)
                  );
                  fun ppf -> (fprintf ppf ";@ ") ) ( fun _ -> () )
                symbols) in
             (fprintf ppf "@]")
          and print_level =
           fun ppf ->
            fun pp_print_space ->
             fun rules ->
              (
              (fprintf ppf "@[<hov 0>[ ")
              );
              let _ =
               (List.fold_left (
                 fun sep ->
                  fun rule ->
                   (
                   (fprintf ppf "%t%a" sep print_rule rule)
                   );
                   fun ppf -> (fprintf ppf "%a| " pp_print_space () ) ) (
                 fun _ -> () ) rules) in
              (fprintf ppf " ]@]")

          let levels =
           fun ppf ->
            fun elev ->
             let _ =
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
                  (match lev.assoc with
                   | LeftA -> (fprintf ppf "LEFTA")
                   | RightA -> (fprintf ppf "RIGHTA")
                   | NonA -> (fprintf ppf "NONA"))
                  );
                  (
                  (fprintf ppf "@]@;<1 2>")
                  );
                  (
                  (fprintf ppf "@[<v2>suffix:@ ")
                  );
                  (
                  (print_tree ppf ( lev.lsuffix ))
                  );
                  (
                  (fprintf ppf "@]@ @[<v2>prefix:@ ")
                  );
                  (
                  (print_tree ppf ( lev.lprefix ))
                  );
                  (
                  (fprintf ppf "@]")
                  );
                  fun ppf -> (fprintf ppf "@,| ") ) ( fun _ -> () ) elev) in
             ()

          let entry =
           fun ppf ->
            fun e ->
             (
             (fprintf ppf "@[<v 0>%s: [ " ( e.ename ))
             );
             (
             (match e.edesc with
              | Dlevels (elev) -> (levels ppf elev)
              | Dparser (_) -> (fprintf ppf "<parser>"))
             );
             (fprintf ppf " ]@]")

         end
