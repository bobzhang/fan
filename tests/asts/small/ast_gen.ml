let loc_of = Ast_loc.loc_of
let (<+>) = let open Locf.Ops in (<+>)
let (<+>) = function | a -> (function | b -> (loc_of a) <+> (loc_of b))
let sem =
  function
  | (a : 'a) ->
      (function | (b : 'a) -> let _loc = a <+> b in `Sem (_loc, a, b))
let com =
  function | a -> (function | b -> let _loc = a <+> b in `Com (_loc, a, b))
let app =
  function | a -> (function | b -> let _loc = a <+> b in `App (_loc, a, b))
let apply =
  function | a -> (function | b -> let _loc = a <+> b in `Apply (_loc, a, b))
let sta =
  function | a -> (function | b -> let _loc = a <+> b in `Sta (_loc, a, b))
let bar =
  function | a -> (function | b -> let _loc = a <+> b in `Bar (_loc, a, b))
let anda =
  function | a -> (function | b -> let _loc = a <+> b in `And (_loc, a, b))
let dot =
  function | a -> (function | b -> let _loc = a <+> b in `Dot (_loc, a, b))
let par = function | x -> let _loc = loc_of x in `Par (_loc, x)
let seq = function | a -> let _loc = loc_of a in `Seq (_loc, a)
let arrow =
  function | a -> (function | b -> let _loc = a <+> b in `Arrow (_loc, a, b))
let typing =
  function
  | a -> (function | b -> let _loc = a <+> b in `Constraint (_loc, a, b))
let bar_of_list = function | xs -> Ast_basic.of_listr bar xs
let and_of_list = function | xs -> Ast_basic.of_listr anda xs
let sem_of_list = function | xs -> Ast_basic.of_listr sem xs
let com_of_list = function | xs -> Ast_basic.of_listr com xs
let sta_of_list = function | xs -> Ast_basic.of_listr sta xs
let dot_of_list = function | xs -> Ast_basic.of_listr dot xs
let appl_of_list = function | xs -> Ast_basic.of_listl app xs
let seq_sem = function | ls -> seq (sem_of_list ls)
let binds =
  function
  | bs ->
      (function
       | (e : Astf.exp) ->
           (match bs with
            | [] -> e
            | _ ->
                let binds = and_of_list bs in
                let _loc = binds <+> e in
                (`LetIn
                   (_loc, (`Negative _loc), (binds :> Astf.bind),
                     (e :> Astf.exp)) :> Astf.exp)))
let seq_binds =
  function
  | bs ->
      (function
       | (e : Astf.exp) ->
           List.fold_right
             (function
              | b ->
                  (function
                   | e ->
                       let _loc = b <+> e in
                       (`LetIn
                          (_loc, (`Negative _loc), (b :> Astf.bind),
                            (e :> Astf.exp)) :> Astf.exp))) bs e)
let lid = function | _loc -> (function | n -> `Lid (_loc, n))
let uid = function | _loc -> (function | n -> `Uid (_loc, n))
let unit = function | _loc -> `Unit _loc
let ep_of_cons =
  function
  | _loc ->
      (function
       | n ->
           (function
            | ps ->
                (match ps with
                 | [] -> uid _loc n
                 | _ -> app (uid _loc n) (`Par (_loc, (com_of_list ps))))))
let tuple_com_unit =
  function
  | _loc ->
      (function
       | [] -> unit _loc
       | p::[] -> p
       | y -> `Par (_loc, (com_of_list y)))
let tuple_com =
  function
  | y ->
      (match y with
       | [] -> failwith "tuple_com empty"
       | x::[] -> x
       | x::_ ->
           let _loc = x <+> (Listf.last y) in `Par (_loc, (com_of_list y)))
let tuple_sta =
  function
  | y ->
      (match y with
       | [] -> failwith "tuple_sta empty"
       | x::[] -> x
       | x::_ ->
           let _loc = x <+> (Listf.last y) in `Par (_loc, (sta_of_list y)))
let (+>) =
  function
  | f ->
      (function
       | names ->
           let _loc = loc_of f in
           appl_of_list (f :: (List.map (lid _loc) names)))
let meta_here =
  function
  | _loc ->
      (function
       | (location : Locf.t) ->
           let {
                 Locf.loc_start =
                   { pos_fname = a; pos_lnum = b; pos_bol = c; pos_cnum = d };
                 loc_end = { pos_lnum = e; pos_bol = f; pos_cnum = g;_};
                 loc_ghost = h }
             = location in
           (`Constraint
              (_loc,
                (`Record
                   (_loc,
                     (`Sem
                        (_loc,
                          (`RecBind
                             (_loc, (`Lid (_loc, "loc_start")),
                               (`Record
                                  (_loc,
                                    (`Sem
                                       (_loc,
                                         (`RecBind
                                            (_loc,
                                              (`Lid (_loc, "pos_fname")),
                                              (`Str
                                                 (_loc, (String.escaped a))))),
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pos_lnum")),
                                                   (`Int
                                                      (_loc,
                                                        (string_of_int b))))),
                                              (`Sem
                                                 (_loc,
                                                   (`RecBind
                                                      (_loc,
                                                        (`Lid
                                                           (_loc, "pos_bol")),
                                                        (`Int
                                                           (_loc,
                                                             (string_of_int c))))),
                                                   (`RecBind
                                                      (_loc,
                                                        (`Lid
                                                           (_loc, "pos_cnum")),
                                                        (`Int
                                                           (_loc,
                                                             (string_of_int d))))))))))))))),
                          (`Sem
                             (_loc,
                               (`RecBind
                                  (_loc, (`Lid (_loc, "loc_end")),
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pos_fname")),
                                                   (`Str
                                                      (_loc,
                                                        (String.escaped a))))),
                                              (`Sem
                                                 (_loc,
                                                   (`RecBind
                                                      (_loc,
                                                        (`Lid
                                                           (_loc, "pos_lnum")),
                                                        (`Int
                                                           (_loc,
                                                             (string_of_int e))))),
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc,
                                                                  "pos_bol")),
                                                             (`Int
                                                                (_loc,
                                                                  (string_of_int
                                                                    f))))),
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc,
                                                                  "pos_cnum")),
                                                             (`Int
                                                                (_loc,
                                                                  (string_of_int
                                                                    g))))))))))))))),
                               (`RecBind
                                  (_loc, (`Lid (_loc, "loc_ghost")),
                                    (`Bool (_loc, h)))))))))),
                (`Dot (_loc, (`Uid (_loc, "Locf")), (`Lid (_loc, "t"))))) :> 
             Astf.ep))
