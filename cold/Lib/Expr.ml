open Camlp4Ast

open FanUtil

let rec sep_expr =
                               fun acc ->
                                function
                                | Ast.ExAcc (_, e1, e2) ->
                                   (sep_expr ( (sep_expr acc e2) ) e1)
                                | (Ast.ExId (loc, Ast.IdUid (_, s)) as e) ->
                                   (match acc with
                                    | [] -> [(loc, [] , e)]
                                    | ((loc', sl, e) :: l) ->
                                       (
                                        (( (FanLoc.merge loc loc') ), (
                                         ( s ) :: sl  ), e) ) :: l )
                                | Ast.ExId (_, (Ast.IdAcc (_, _, _) as i)) ->
                                   (sep_expr acc ( (Ident.normalize_acc i) ))
                                | e ->
                                   ( (( (loc_of_expr e) ), [] , e) ) :: acc 


let rec fa =
 fun al ->
  function | Ast.ExApp (_, f, a) -> (fa ( ( a ) :: al  ) f) | f -> (f, al)


let rec apply =
 fun accu ->
  function
  | [] -> accu
  | (x :: xs) ->
     let _loc = (loc_of_expr x) in (apply ( (Ast.ExApp (_loc, accu, x)) ) xs)


let mklist =
 fun _loc ->
  let rec loop =
   fun top ->
    function
    | [] -> (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "[]")) )))
    | (e1 :: el) ->
       let _loc =
        if top then _loc else (FanLoc.merge ( (loc_of_expr e1) ) _loc) in
       (Ast.ExApp
         (_loc, (
          (Ast.ExApp
            (_loc, ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "::")) ))) ), e1))
          ), ( (loop false  el) ))) in
  (loop true )

let mkumin =
                 fun _loc ->
                  fun f ->
                   fun arg ->
                    (match arg with
                     | Ast.ExInt (_, n) ->
                        (Ast.ExInt (_loc, ( (neg_string n) )))
                     | Ast.ExInt32 (_, n) ->
                        (Ast.ExInt32 (_loc, ( (neg_string n) )))
                     | Ast.ExInt64 (_, n) ->
                        (Ast.ExInt64 (_loc, ( (neg_string n) )))
                     | Ast.ExNativeInt (_, n) ->
                        (Ast.ExNativeInt (_loc, ( (neg_string n) )))
                     | Ast.ExFlo (_, n) ->
                        (Ast.ExFlo (_loc, ( (neg_string n) )))
                     | _ ->
                        (Ast.ExApp
                          (_loc, (
                           (Ast.ExId
                             (_loc, ( (Ast.IdLid (_loc, ( ("~" ^ f) ))) )))
                           ), arg)))

let mkassert =
                                       fun _loc ->
                                        function
                                        | Ast.ExId
                                           (_, Ast.IdUid (_, "False")) ->
                                           (Ast.ExAsf (_loc))
                                        | e -> (Ast.ExAsr (_loc, e))


let mklist_last =
 fun ?last ->
  fun _loc ->
   let rec loop =
    fun top ->
     function
     | [] ->
        (match last with
         | Some (e) -> e
         | None -> (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "[]")) ))))
     | (e1 :: el) ->
        let _loc =
         if top then _loc else (FanLoc.merge ( (loc_of_expr e1) ) _loc) in
        (Ast.ExApp
          (_loc, (
           (Ast.ExApp
             (_loc, ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "::")) ))) ), e1))
           ), ( (loop false  el) ))) in
   (loop true )

let mksequence =
                  fun _loc ->
                   function
                   | ((Ast.ExSem (_, _, _) | Ast.ExAnt (_, _)) as e) ->
                      (Ast.ExSeq (_loc, e))
                   | e -> e

let mksequence' =
                              fun _loc ->
                               function
                               | (Ast.ExSem (_, _, _) as e) ->
                                  (Ast.ExSeq (_loc, e))
                               | e -> e

let bigarray_get =
                                          fun _loc ->
                                           fun arr ->
                                            fun arg ->
                                             let coords =
                                              (match arg with
                                               | (Ast.ExTup
                                                   (_, Ast.ExCom (_, e1, e2))
                                                  | Ast.ExCom (_, e1, e2)) ->
                                                  (list_of_expr e1 (
                                                    (list_of_expr e2 [] ) ))
                                               | _ -> [arg]) in
                                             (match coords with
                                              | [] ->
                                                 (failwith
                                                   "bigarray_get null list")
                                              | (c1 :: []) ->
                                                 (Ast.ExApp
                                                   (_loc, (
                                                    (Ast.ExApp
                                                      (_loc, (
                                                       (Ast.ExId
                                                         (_loc, (
                                                          (Ast.IdAcc
                                                            (_loc, (
                                                             (Ast.IdUid
                                                               (_loc,
                                                                "Bigarray"))
                                                             ), (
                                                             (Ast.IdAcc
                                                               (_loc, (
                                                                (Ast.IdUid
                                                                  (_loc,
                                                                   "Array1"))
                                                                ), (
                                                                (Ast.IdLid
                                                                  (_loc,
                                                                   "get")) )))
                                                             ))) ))) ), arr))
                                                    ), c1))
                                              | (c1 :: c2 :: []) ->
                                                 (Ast.ExApp
                                                   (_loc, (
                                                    (Ast.ExApp
                                                      (_loc, (
                                                       (Ast.ExApp
                                                         (_loc, (
                                                          (Ast.ExId
                                                            (_loc, (
                                                             (Ast.IdAcc
                                                               (_loc, (
                                                                (Ast.IdUid
                                                                  (_loc,
                                                                   "Bigarray"))
                                                                ), (
                                                                (Ast.IdAcc
                                                                  (_loc, (
                                                                   (Ast.IdUid
                                                                    (_loc,
                                                                    "Array2"))
                                                                   ), (
                                                                   (Ast.IdLid
                                                                    (_loc,
                                                                    "get"))
                                                                   ))) ))) )))
                                                          ), arr)) ), c1)) ),
                                                    c2))
                                              | (c1 :: c2 :: c3 :: []) ->
                                                 (Ast.ExApp
                                                   (_loc, (
                                                    (Ast.ExApp
                                                      (_loc, (
                                                       (Ast.ExApp
                                                         (_loc, (
                                                          (Ast.ExApp
                                                            (_loc, (
                                                             (Ast.ExId
                                                               (_loc, (
                                                                (Ast.IdAcc
                                                                  (_loc, (
                                                                   (Ast.IdUid
                                                                    (_loc,
                                                                    "Bigarray"))
                                                                   ), (
                                                                   (Ast.IdAcc
                                                                    (_loc, (
                                                                    (Ast.IdUid
                                                                    (_loc,
                                                                    "Array3"))
                                                                    ), (
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "get"))
                                                                    ))) )))
                                                                ))) ), arr))
                                                          ), c1)) ), c2)) ),
                                                    c3))
                                              | (c1 :: c2 :: c3 :: coords) ->
                                                 (Ast.ExApp
                                                   (_loc, (
                                                    (Ast.ExApp
                                                      (_loc, (
                                                       (Ast.ExId
                                                         (_loc, (
                                                          (Ast.IdAcc
                                                            (_loc, (
                                                             (Ast.IdUid
                                                               (_loc,
                                                                "Bigarray"))
                                                             ), (
                                                             (Ast.IdAcc
                                                               (_loc, (
                                                                (Ast.IdUid
                                                                  (_loc,
                                                                   "Genarray"))
                                                                ), (
                                                                (Ast.IdLid
                                                                  (_loc,
                                                                   "get")) )))
                                                             ))) ))) ), arr))
                                                    ), (
                                                    (Ast.ExArr
                                                      (_loc, (
                                                       (Ast.ExSem
                                                         (_loc, c1, (
                                                          (Ast.ExSem
                                                            (_loc, c2, (
                                                             (Ast.ExSem
                                                               (_loc, c3, (
                                                                (exSem_of_list
                                                                  coords) )))
                                                             ))) ))) ))) ))))


let bigarray_set =
 fun _loc ->
  fun var ->
   fun newval ->
    (match var with
     | Ast.ExApp
        (_,
         Ast.ExApp
          (_,
           Ast.ExId
            (_,
             Ast.IdAcc
              (_, Ast.IdUid (_, "Bigarray"),
               Ast.IdAcc (_, Ast.IdUid (_, "Array1"), Ast.IdLid (_, "get")))),
           arr), c1) ->
        (Some
          ((Ast.ExAss
             (_loc, (
              (Ast.ExAcc
                (_loc, (
                 (Ast.ExApp
                   (_loc, (
                    (Ast.ExApp
                      (_loc, (
                       (Ast.ExId
                         (_loc, (
                          (Ast.IdAcc
                            (_loc, ( (Ast.IdUid (_loc, "Bigarray")) ), (
                             (Ast.IdAcc
                               (_loc, ( (Ast.IdUid (_loc, "Array1")) ), (
                                (Ast.IdLid (_loc, "get")) ))) ))) ))) ), arr))
                    ), c1)) ), (
                 (Ast.ExId (_loc, ( (Ast.IdLid (_loc, "contents")) ))) ))) ),
              newval))))
     | Ast.ExApp
        (_,
         Ast.ExApp
          (_,
           Ast.ExApp
            (_,
             Ast.ExId
              (_,
               Ast.IdAcc
                (_, Ast.IdUid (_, "Bigarray"),
                 Ast.IdAcc (_, Ast.IdUid (_, "Array2"), Ast.IdLid (_, "get")))),
             arr), c1), c2) ->
        (Some
          ((Ast.ExAss
             (_loc, (
              (Ast.ExAcc
                (_loc, (
                 (Ast.ExApp
                   (_loc, (
                    (Ast.ExApp
                      (_loc, (
                       (Ast.ExApp
                         (_loc, (
                          (Ast.ExId
                            (_loc, (
                             (Ast.IdAcc
                               (_loc, ( (Ast.IdUid (_loc, "Bigarray")) ), (
                                (Ast.IdAcc
                                  (_loc, ( (Ast.IdUid (_loc, "Array2")) ), (
                                   (Ast.IdLid (_loc, "get")) ))) ))) ))) ),
                          arr)) ), c1)) ), c2)) ), (
                 (Ast.ExId (_loc, ( (Ast.IdLid (_loc, "contents")) ))) ))) ),
              newval))))
     | Ast.ExApp
        (_,
         Ast.ExApp
          (_,
           Ast.ExApp
            (_,
             Ast.ExApp
              (_,
               Ast.ExId
                (_,
                 Ast.IdAcc
                  (_, Ast.IdUid (_, "Bigarray"),
                   Ast.IdAcc
                    (_, Ast.IdUid (_, "Array3"), Ast.IdLid (_, "get")))), arr),
             c1), c2), c3) ->
        (Some
          ((Ast.ExAss
             (_loc, (
              (Ast.ExAcc
                (_loc, (
                 (Ast.ExApp
                   (_loc, (
                    (Ast.ExApp
                      (_loc, (
                       (Ast.ExApp
                         (_loc, (
                          (Ast.ExApp
                            (_loc, (
                             (Ast.ExId
                               (_loc, (
                                (Ast.IdAcc
                                  (_loc, ( (Ast.IdUid (_loc, "Bigarray")) ),
                                   (
                                   (Ast.IdAcc
                                     (_loc, ( (Ast.IdUid (_loc, "Array3")) ),
                                      ( (Ast.IdLid (_loc, "get")) ))) ))) )))
                             ), arr)) ), c1)) ), c2)) ), c3)) ), (
                 (Ast.ExId (_loc, ( (Ast.IdLid (_loc, "contents")) ))) ))) ),
              newval))))
     | Ast.ExApp
        (_,
         Ast.ExApp
          (_,
           Ast.ExId
            (_,
             Ast.IdAcc
              (_, Ast.IdUid (_, "Bigarray"),
               Ast.IdAcc (_, Ast.IdUid (_, "Genarray"), Ast.IdLid (_, "get")))),
           arr), Ast.ExArr (_, coords)) ->
        (Some
          ((Ast.ExApp
             (_loc, (
              (Ast.ExApp
                (_loc, (
                 (Ast.ExApp
                   (_loc, (
                    (Ast.ExId
                      (_loc, (
                       (Ast.IdAcc
                         (_loc, ( (Ast.IdUid (_loc, "Bigarray")) ), (
                          (Ast.IdAcc
                            (_loc, ( (Ast.IdUid (_loc, "Genarray")) ), (
                             (Ast.IdLid (_loc, "set")) ))) ))) ))) ), arr))
                 ), ( (Ast.ExArr (_loc, coords)) ))) ), newval))))
     | _ -> (None))

let map =
                      fun _loc ->
                       fun p ->
                        fun e ->
                         fun l ->
                          (match (p, e) with
                           | (Ast.PaId (_, Ast.IdLid (_, x)),
                              Ast.ExId (_, Ast.IdLid (_, y))) when (x = y) ->
                              l
                           | _ ->
                              if (is_irrefut_patt p) then
                               (
                               (Ast.ExApp
                                 (_loc, (
                                  (Ast.ExApp
                                    (_loc, (
                                     (Ast.ExId
                                       (_loc, (
                                        (Ast.IdAcc
                                          (_loc, ( (Ast.IdUid (_loc, "List"))
                                           ), ( (Ast.IdLid (_loc, "map")) )))
                                        ))) ), (
                                     (Ast.ExFun
                                       (_loc, (
                                        (Ast.McArr
                                          (_loc, p, ( (Ast.ExNil (_loc)) ),
                                           e)) ))) ))) ), l))
                               )
                              else
                               (Ast.ExApp
                                 (_loc, (
                                  (Ast.ExApp
                                    (_loc, (
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "List")) ), (
                                              (Ast.IdLid (_loc, "fold_right"))
                                              ))) ))) ), (
                                        (Ast.ExFun
                                          (_loc, (
                                           (Ast.McOr
                                             (_loc, (
                                              (Ast.McArr
                                                (_loc, p, (
                                                 (Ast.ExId
                                                   (_loc, (
                                                    (Ast.IdUid (_loc, "True"))
                                                    ))) ), (
                                                 (Ast.ExApp
                                                   (_loc, (
                                                    (Ast.ExFun
                                                      (_loc, (
                                                       (Ast.McArr
                                                         (_loc, (
                                                          (Ast.PaId
                                                            (_loc, (
                                                             (Ast.IdLid
                                                               (_loc, "x"))
                                                             ))) ), (
                                                          (Ast.ExNil (_loc))
                                                          ), (
                                                          (Ast.ExFun
                                                            (_loc, (
                                                             (Ast.McArr
                                                               (_loc, (
                                                                (Ast.PaId
                                                                  (_loc, (
                                                                   (Ast.IdLid
                                                                    (_loc,
                                                                    "xs")) )))
                                                                ), (
                                                                (Ast.ExNil
                                                                  (_loc)) ),
                                                                (
                                                                (Ast.ExApp
                                                                  (_loc, (
                                                                   (Ast.ExApp
                                                                    (_loc, (
                                                                    (Ast.ExId
                                                                    (_loc, (
                                                                    (Ast.IdUid
                                                                    (_loc,
                                                                    "::")) )))
                                                                    ), (
                                                                    (Ast.ExId
                                                                    (_loc, (
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "x")) )))
                                                                    ))) ), (
                                                                   (Ast.ExId
                                                                    (_loc, (
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "xs")) )))
                                                                   ))) ))) )))
                                                          ))) ))) ), e)) )))
                                              ), (
                                              (Ast.McArr
                                                (_loc, ( (Ast.PaAny (_loc))
                                                 ), ( (Ast.ExNil (_loc)) ), (
                                                 (Ast.ExFun
                                                   (_loc, (
                                                    (Ast.McArr
                                                      (_loc, (
                                                       (Ast.PaId
                                                         (_loc, (
                                                          (Ast.IdLid
                                                            (_loc, "l")) )))
                                                       ), (
                                                       (Ast.ExNil (_loc)) ),
                                                       (
                                                       (Ast.ExId
                                                         (_loc, (
                                                          (Ast.IdLid
                                                            (_loc, "l")) )))
                                                       ))) ))) ))) ))) ))) )))
                                     ), l)) ), (
                                  (Ast.ExId
                                    (_loc, ( (Ast.IdUid (_loc, "[]")) ))) ))))


let filter =
 fun _loc ->
  fun p ->
   fun b ->
    fun l ->
     if (is_irrefut_patt p) then
      (
      (Ast.ExApp
        (_loc, (
         (Ast.ExApp
           (_loc, (
            (Ast.ExId
              (_loc, (
               (Ast.IdAcc
                 (_loc, ( (Ast.IdUid (_loc, "List")) ), (
                  (Ast.IdLid (_loc, "filter")) ))) ))) ), (
            (Ast.ExFun
              (_loc, ( (Ast.McArr (_loc, p, ( (Ast.ExNil (_loc)) ), b)) )))
            ))) ), l))
      )
     else
      (Ast.ExApp
        (_loc, (
         (Ast.ExApp
           (_loc, (
            (Ast.ExId
              (_loc, (
               (Ast.IdAcc
                 (_loc, ( (Ast.IdUid (_loc, "List")) ), (
                  (Ast.IdLid (_loc, "filter")) ))) ))) ), (
            (Ast.ExFun
              (_loc, (
               (Ast.McOr
                 (_loc, (
                  (Ast.McArr
                    (_loc, p, (
                     (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "True")) ))) ), b))
                  ), (
                  (Ast.McArr
                    (_loc, ( (Ast.PaAny (_loc)) ), ( (Ast.ExNil (_loc)) ), (
                     (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "False")) ))) )))
                  ))) ))) ))) ), l))

let concat =
                                       fun _loc ->
                                        fun l ->
                                         (Ast.ExApp
                                           (_loc, (
                                            (Ast.ExId
                                              (_loc, (
                                               (Ast.IdAcc
                                                 (_loc, (
                                                  (Ast.IdUid (_loc, "List"))
                                                  ), (
                                                  (Ast.IdLid (_loc, "concat"))
                                                  ))) ))) ), l))

let rec compr =
                                                                   fun _loc ->
                                                                    fun e ->
                                                                    function
                                                                    | ((`gen
                                                                    (p, l))
                                                                    :: []) ->
                                                                    (map _loc
                                                                    p e l)
                                                                    | ((`gen
                                                                    (p, l))
                                                                    ::
                                                                    (`cond b)
                                                                    :: items) ->
                                                                    (compr
                                                                    _loc e (
                                                                    (
                                                                    `gen
                                                                    (p, (
                                                                    (filter
                                                                    _loc p b
                                                                    l) )) ) ::
                                                                    items  ))
                                                                    | ((`gen
                                                                    (p, l))
                                                                    ::
                                                                    (((`gen
                                                                    (_, _))
                                                                    :: _) as
                                                                    is)) ->
                                                                    (concat
                                                                    _loc (
                                                                    (map _loc
                                                                    p (
                                                                    (compr
                                                                    _loc e
                                                                    is) ) l)
                                                                    ))
                                                                    | 
                                                                    _ ->
                                                                    (raise
                                                                    Stream.Failure
                                                                    )


let bad_patt =
 fun _loc ->
  (FanLoc.raise _loc (
    (Failure ("this macro cannot be used in a pattern (see its definition)"))
    ))

let substp =
         fun _loc ->
          fun env ->
           let rec loop =
            function
            | Ast.ExApp (_, e1, e2) ->
               (Ast.PaApp (_loc, ( (loop e1) ), ( (loop e2) )))
            | Ast.ExNil (_) -> (Ast.PaNil (_loc))
            | Ast.ExId (_, Ast.IdLid (_, x)) ->
               (try (List.assoc x env) with
                Not_found -> (Ast.PaId (_loc, ( (Ast.IdLid (_loc, x)) ))))
            | Ast.ExId (_, Ast.IdUid (_, x)) ->
               (try (List.assoc x env) with
                Not_found -> (Ast.PaId (_loc, ( (Ast.IdUid (_loc, x)) ))))
            | Ast.ExInt (_, x) -> (Ast.PaInt (_loc, x))
            | Ast.ExStr (_, s) -> (Ast.PaStr (_loc, s))
            | Ast.ExTup (_, x) -> (Ast.PaTup (_loc, ( (loop x) )))
            | Ast.ExCom (_, x1, x2) ->
               (Ast.PaCom (_loc, ( (loop x1) ), ( (loop x2) )))
            | Ast.ExRec (_, bi, Ast.ExNil (_)) ->
               let rec substbi =
                function
                | Ast.RbSem (_, b1, b2) ->
                   (Ast.PaSem (_loc, ( (substbi b1) ), ( (substbi b2) )))
                | Ast.RbEq (_, i, e) -> (Ast.PaEq (_loc, i, ( (loop e) )))
                | _ -> (bad_patt _loc) in
               (Ast.PaRec (_loc, ( (substbi bi) )))
            | _ -> (bad_patt _loc) in
           loop

class subst _loc env =
                  object
                   inherit (reloc _loc) as super
                   method! expr =
                    function
                    | ((Ast.ExId (_, Ast.IdLid (_, x))
                        | Ast.ExId (_, Ast.IdUid (_, x))) as e) ->
                       (try (List.assoc x env) with
                        Not_found -> (super#expr e))
                    | ((Ast.ExApp
                         (_loc, Ast.ExId (_, Ast.IdUid (_, "LOCATION_OF")),
                          Ast.ExId (_, Ast.IdLid (_, x)))
                        | Ast.ExApp
                           (_loc, Ast.ExId (_, Ast.IdUid (_, "LOCATION_OF")),
                            Ast.ExId (_, Ast.IdUid (_, x)))) as e) ->
                       (try
                         let loc = (loc_of_expr ( (List.assoc x env) )) in
                         let (a, b, c, d, e, f, g, h) = (FanLoc.to_tuple loc) in
                         (Ast.ExApp
                           (_loc, (
                            (Ast.ExId
                              (_loc, (
                               (Ast.IdAcc
                                 (_loc, ( (Ast.IdUid (_loc, "FanLoc")) ), (
                                  (Ast.IdLid (_loc, "of_tuple")) ))) ))) ), (
                            (Ast.ExTup
                              (_loc, (
                               (Ast.ExCom
                                 (_loc, (
                                  (Ast.ExStr
                                    (_loc, ( (Ast.safe_string_escaped a) )))
                                  ), (
                                  (Ast.ExCom
                                    (_loc, (
                                     (Ast.ExCom
                                       (_loc, (
                                        (Ast.ExCom
                                          (_loc, (
                                           (Ast.ExCom
                                             (_loc, (
                                              (Ast.ExCom
                                                (_loc, (
                                                 (Ast.ExCom
                                                   (_loc, (
                                                    (Ast.ExInt
                                                      (_loc, (
                                                       (string_of_int b) )))
                                                    ), (
                                                    (Ast.ExInt
                                                      (_loc, (
                                                       (string_of_int c) )))
                                                    ))) ), (
                                                 (Ast.ExInt
                                                   (_loc, ( (string_of_int d)
                                                    ))) ))) ), (
                                              (Ast.ExInt
                                                (_loc, ( (string_of_int e) )))
                                              ))) ), (
                                           (Ast.ExInt
                                             (_loc, ( (string_of_int f) )))
                                           ))) ), (
                                        (Ast.ExInt
                                          (_loc, ( (string_of_int g) ))) )))
                                     ), (
                                     if h then
                                      (
                                      (Ast.ExId
                                        (_loc, ( (Ast.IdUid (_loc, "True"))
                                         )))
                                      )
                                     else
                                      (Ast.ExId
                                        (_loc, ( (Ast.IdUid (_loc, "False"))
                                         ))) ))) ))) ))) )))
                        with
                        Not_found -> (super#expr e))
                    | e -> (super#expr e)
                   method! patt =
                    function
                    | ((Ast.PaId (_, Ast.IdLid (_, x))
                        | Ast.PaId (_, Ast.IdUid (_, x))) as p) ->
                       (try (substp _loc []  ( (List.assoc x env) )) with
                        Not_found -> (super#patt p))
                    | p -> (super#patt p)
                  end

let map_expr =
                        function
                        | (Ast.ExApp
                            (_, e, Ast.ExId (_, Ast.IdUid (_, "NOTHING")))
                           | Ast.ExFun
                              (_,
                               Ast.McArr
                                (_, Ast.PaId (_, Ast.IdUid (_, "NOTHING")),
                                 Ast.ExNil (_), e))) ->
                           e
                        | Ast.ExId (_loc, Ast.IdLid (_, "__FILE__")) ->
                           (Ast.ExStr
                             (_loc, (
                              (Ast.safe_string_escaped (
                                (FanLoc.file_name _loc) )) )))
                        | Ast.ExId (_loc, Ast.IdLid (_, "__LOCATION__")) ->
                           let (a, b, c, d, e, f, g, h) =
                            (FanLoc.to_tuple _loc) in
                           (Ast.ExApp
                             (_loc, (
                              (Ast.ExId
                                (_loc, (
                                 (Ast.IdAcc
                                   (_loc, ( (Ast.IdUid (_loc, "FanLoc")) ), (
                                    (Ast.IdLid (_loc, "of_tuple")) ))) ))) ),
                              (
                              (Ast.ExTup
                                (_loc, (
                                 (Ast.ExCom
                                   (_loc, (
                                    (Ast.ExStr
                                      (_loc, ( (Ast.safe_string_escaped a) )))
                                    ), (
                                    (Ast.ExCom
                                      (_loc, (
                                       (Ast.ExCom
                                         (_loc, (
                                          (Ast.ExCom
                                            (_loc, (
                                             (Ast.ExCom
                                               (_loc, (
                                                (Ast.ExCom
                                                  (_loc, (
                                                   (Ast.ExCom
                                                     (_loc, (
                                                      (Ast.ExInt
                                                        (_loc, (
                                                         (string_of_int b) )))
                                                      ), (
                                                      (Ast.ExInt
                                                        (_loc, (
                                                         (string_of_int c) )))
                                                      ))) ), (
                                                   (Ast.ExInt
                                                     (_loc, (
                                                      (string_of_int d) )))
                                                   ))) ), (
                                                (Ast.ExInt
                                                  (_loc, ( (string_of_int e)
                                                   ))) ))) ), (
                                             (Ast.ExInt
                                               (_loc, ( (string_of_int f) )))
                                             ))) ), (
                                          (Ast.ExInt
                                            (_loc, ( (string_of_int g) ))) )))
                                       ), (
                                       if h then
                                        (
                                        (Ast.ExId
                                          (_loc, ( (Ast.IdUid (_loc, "True"))
                                           )))
                                        )
                                       else
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdUid (_loc, "False")) ))) )))
                                    ))) ))) )))
                        | e -> e
