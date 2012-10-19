open Structure
open Format
let is_before =
 fun s1 ->
  fun s2 ->
   (match (s1 , s2 ) with
    | (((`Skeyword _) | (`Stoken _)) , ((`Skeyword _) | (`Stoken _)) ) ->
       (false)
    | (((`Skeyword _) | (`Stoken _)) , _ ) -> (true)
    | _ -> (false))
let rec derive_eps =
 function
 | (((`Slist0 _) | (`Slist0sep (_ , _ ))) | (`Sopt _)) -> (true)
 | (`Stry s) -> (derive_eps s )
 | (`Stree t) -> (tree_derive_eps t )
 | ((((`Slist1 _) | (`Slist1sep (_ , _ ))) | (`Stoken _)) | (`Skeyword _)) ->
    (false)
 | (((((`Smeta (_ , _ , _ )) | (`Snterm _)) | (`Snterml (_ , _ ))) | `Snext)
    | `Sself) ->
    (false)
and tree_derive_eps =
 function
 | LocAct (_ , _ ) -> (true)
 | Node ({node = s ; brother = bro ; son = son }) ->
    (( (( (derive_eps s ) ) && ( (tree_derive_eps son ) )) ) || (
      (tree_derive_eps bro ) ))
 | DeadEnd -> (false)
let empty_lev =
 fun lname ->
  fun assoc ->
   let assoc = (match assoc with | Some (a) -> a | None -> `LA) in
   {assoc = assoc ; lname = lname ; lsuffix = DeadEnd  ; lprefix = DeadEnd  }
let change_lev =
 fun entry ->
  fun lev ->
   fun n ->
    fun lname ->
     fun assoc ->
      let a =
       (match assoc with
        | None -> lev.assoc
        | Some (a) ->
           (
           if (( (a <> ( lev.assoc )) ) && (
                ((entry.egram).warning_verbose).contents ))
           then
            begin
            (
            (eprintf "<W> Changing associativity of level \"%s\"\n" n )
            );
            (flush Pervasives.stderr )
           end else ()
           );
           a) in
      (
      (match lname with
       | Some (n) ->
          if (( (lname <> ( lev.lname )) ) && (
               ((entry.egram).warning_verbose).contents ))
          then
           begin
           (
           (eprintf "<W> Level label \"%s\" ignored\n" n )
           );
           (flush Pervasives.stderr )
          end else ()
       | None -> ())
      );
      {assoc = a ; lname = ( lev.lname ) ; lsuffix = ( lev.lsuffix ) ;
       lprefix = ( lev.lprefix ) }
let change_to_self =
 fun entry -> function | (`Snterm e) when (e == entry) -> `Sself | x -> x
let get_level =
 fun entry ->
  fun position ->
   fun levs ->
    (match position with
     | Some (`First) -> ([]  , empty_lev , levs )
     | Some (`Last) -> (levs , empty_lev , []  )
     | Some ((`Level n)) ->
        let rec get =
         function
         | [] ->
            (
            (eprintf "No level labelled \"%s\" in entry \"%s\"\n" n (
              entry.ename ) )
            );
            (
            (flush Pervasives.stderr )
            );
            (failwith "Grammar.extend" )
         | (lev :: levs) ->
            if (Tools.is_level_labelled n lev ) then
             ([]  , ( (change_lev entry lev n ) ) , levs )
            else
             let (levs1 , rlev , levs2 ) = (get levs ) in
             (( ( lev ) :: levs1  ) , rlev , levs2 ) in
        (get levs )
     | Some ((`Before n)) ->
        let rec get =
         function
         | [] ->
            (
            (eprintf "No level labelled \"%s\" in entry \"%s\"\n" n (
              entry.ename ) )
            );
            (
            (flush Pervasives.stderr )
            );
            (failwith "Grammar.extend" )
         | (lev :: levs) ->
            if (Tools.is_level_labelled n lev ) then
             ([]  , empty_lev , ( ( lev ) :: levs  ) )
            else
             let (levs1 , rlev , levs2 ) = (get levs ) in
             (( ( lev ) :: levs1  ) , rlev , levs2 ) in
        (get levs )
     | Some ((`After n)) ->
        let rec get =
         function
         | [] ->
            (
            (eprintf "No level labelled \"%s\" in entry \"%s\"\n" n (
              entry.ename ) )
            );
            (
            (flush Pervasives.stderr )
            );
            (failwith "Grammar.extend" )
         | (lev :: levs) ->
            if (Tools.is_level_labelled n lev ) then
             (( [lev] ) , empty_lev , levs )
            else
             let (levs1 , rlev , levs2 ) = (get levs ) in
             (( ( lev ) :: levs1  ) , rlev , levs2 ) in
        (get levs )
     | None ->
        (match levs with
         | (lev :: levs) ->
            ([]  , ( (change_lev entry lev "<top>" ) ) , levs )
         | [] -> ([]  , empty_lev , []  )))
let rec check_gram =
 fun entry ->
  function
  | (`Snterm e) ->
     if (( e.egram ) != ( entry.egram ))
     then
      begin
      (
      (eprintf
        "Error: entries \"%s\" and \"%s\" do not belong to the same grammar.\n"
        ( entry.ename ) ( e.ename ) )
      );
      (
      (flush Pervasives.stderr )
      );
      (failwith "Grammar.extend error" )
     end else ()
  | (`Snterml (e , _ )) ->
     if (( e.egram ) != ( entry.egram ))
     then
      begin
      (
      (eprintf
        "Error: entries \"%s\" and \"%s\" do not belong to the same grammar.\n"
        ( entry.ename ) ( e.ename ) )
      );
      (
      (flush Pervasives.stderr )
      );
      (failwith "Grammar.extend error" )
     end else ()
  | (`Smeta (_ , sl , _ )) -> (List.iter ( (check_gram entry ) ) sl )
  | (`Slist0sep (s , t )) -> ( (check_gram entry t ) ); (check_gram entry s )
  | (`Slist1sep (s , t )) -> ( (check_gram entry t ) ); (check_gram entry s )
  | ((((`Slist0 s) | (`Slist1 s)) | (`Sopt s)) | (`Stry s)) ->
     (check_gram entry s )
  | (`Stree t) -> (tree_check_gram entry t )
  | (((`Snext | `Sself) | (`Stoken _)) | (`Skeyword _)) -> ()
and tree_check_gram =
 fun entry ->
  function
  | Node ({node = n ; brother = bro ; son = son }) ->
     (
     (check_gram entry n )
     );
     (
     (tree_check_gram entry bro )
     );
     (tree_check_gram entry son )
  | (LocAct (_ , _ ) | DeadEnd) -> ()
let get_initial =
 function
 | (`Sself :: symbols) -> (true  , symbols )
 | symbols -> (false  , symbols )
let insert_tokens =
 fun gram ->
  fun symbols ->
   let rec insert =
    function
    | (`Smeta (_ , sl , _ )) -> (List.iter insert sl )
    | ((((`Slist0 s) | (`Slist1 s)) | (`Sopt s)) | (`Stry s)) -> (insert s )
    | (`Slist0sep (s , t )) -> ( (insert s ) ); (insert t )
    | (`Slist1sep (s , t )) -> ( (insert s ) ); (insert t )
    | (`Stree t) -> (tinsert t )
    | (`Skeyword kwd) -> (using gram kwd )
    | (((((`Snterm _) | (`Snterml (_ , _ ))) | `Snext) | `Sself)
       | (`Stoken _)) ->
       ()
   and tinsert =
    function
    | Node ({node = s ; brother = bro ; son = son }) ->
       ( (insert s ) ); ( (tinsert bro ) ); (tinsert son )
    | (LocAct (_ , _ ) | DeadEnd) -> () in
   (List.iter insert symbols )
let insert_tree =
 fun entry ->
  fun gsymbols ->
   fun action ->
    fun tree ->
     let rec insert =
      fun symbols ->
       fun tree ->
        (match symbols with
         | (s :: sl) -> (insert_in_tree s sl tree )
         | [] ->
            (match tree with
             | Node ({node = s ; son = son ; brother = bro }) ->
                (Node
                  ({node = s ; son = son ; brother = ( (insert []  bro ) ) }))
             | LocAct (old_action , action_list ) ->
                let () =
                 if ((entry.egram).warning_verbose).contents then
                  (
                  (eprintf
                    "<W> Grammar extension: in [%s] some rule has been masked@."
                    ( entry.ename ) )
                  )
                 else () in
                (LocAct (action , ( ( old_action ) :: action_list  ) ))
             | DeadEnd -> (LocAct (action , []  ))))
     and insert_in_tree =
      fun s ->
       fun sl ->
        fun tree ->
         (match (try_insert s sl tree ) with
          | Some (t) -> t
          | None ->
             (Node
               ({node = s ; son = ( (insert sl DeadEnd  ) ) ; brother = tree
                 })))
     and try_insert =
      fun s ->
       fun sl ->
        fun tree ->
         (match tree with
          | Node ({node = s1 ; son = son ; brother = bro }) ->
             if (Tools.eq_symbol s s1 ) then
              (
              let t =
               (Node
                 ({node = s1 ; son = ( (insert sl son ) ) ; brother = bro })) in
              (Some (t))
              )
             else if (( (is_before s1 s ) ) || (
                       (( (derive_eps s ) ) && ( (not ( (derive_eps s1 ) ) )
                         )) )) then
                   (
                   let bro =
                    (match (try_insert s sl bro ) with
                     | Some (bro) -> bro
                     | None ->
                        (Node
                          ({node = s ; son = ( (insert sl DeadEnd  ) ) ;
                            brother = bro }))) in
                   let t = (Node ({node = s1 ; son = son ; brother = bro })) in
                   (Some (t))
                   )
             else
              (match (try_insert s sl bro ) with
               | Some (bro) ->
                  let t = (Node ({node = s1 ; son = son ; brother = bro })) in
                  (Some (t))
               | None -> (None))
          | (LocAct (_ , _ ) | DeadEnd) -> (None)) in
     (insert gsymbols tree )
let insert_level =
 fun entry ->
  fun e1 ->
   fun symbols ->
    fun action ->
     fun slev ->
      (match e1 with
       | true ->
          {assoc = ( slev.assoc ) ; lname = ( slev.lname ) ;
           lsuffix = ( (insert_tree entry symbols action ( slev.lsuffix ) ) )
           ; lprefix = ( slev.lprefix ) }
       | false ->
          {assoc = ( slev.assoc ) ; lname = ( slev.lname ) ;
           lsuffix = ( slev.lsuffix ) ;
           lprefix = ( (insert_tree entry symbols action ( slev.lprefix ) ) )
           })
let levels_of_rules =
 fun entry ->
  fun position ->
   fun rules ->
    let elev =
     (match entry.edesc with
      | Dlevels (elev) -> elev
      | Dparser (_) ->
         (
         (eprintf "Error: entry not extensible: \"%s\"\n" ( entry.ename ) )
         );
         (
         (flush Pervasives.stderr )
         );
         (failwith "Grammar.extend" )) in
    if (rules = [] ) then elev
    else
     let (levs1 , make_lev , levs2 ) = (get_level entry position elev ) in
     let (levs , _ ) =
      (List.fold_left (
        fun (levs , make_lev ) ->
         fun (lname , assoc , level ) ->
          let lev = (make_lev lname assoc ) in
          let lev =
           (List.fold_left (
             fun lev ->
              fun (symbols , action ) ->
               let symbols = (List.map ( (change_to_self entry ) ) symbols ) in
               let () = (List.iter ( (check_gram entry ) ) symbols ) in
               let (e1 , symbols ) = (get_initial symbols ) in
               let () = (insert_tokens ( entry.egram ) symbols ) in
               (insert_level entry e1 symbols action lev ) ) lev level ) in
          (( ( lev ) :: levs  ) , empty_lev ) ) ([]  , make_lev ) rules ) in
     (levs1 @ ( (( (List.rev levs ) ) @ levs2) ))
let extend =
 fun entry ->
  fun (position , rules ) ->
   let elev = (levels_of_rules entry position rules ) in
   (
   entry.edesc <- (Dlevels (elev))
   );
   (
   entry.estart <-
    fun lev ->
     fun strm ->
      let f = (Parser.start_parser_of_entry entry ) in
      (
      entry.estart <- f
      );
      (f lev strm )
   );
   entry.econtinue <-
    fun lev ->
     fun bp ->
      fun a ->
       fun strm ->
        let f = (Parser.continue_parser_of_entry entry ) in
        (
        entry.econtinue <- f
        );
        (f lev bp a strm )
