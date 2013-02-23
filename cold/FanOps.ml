open LibUtil
let bigarray_get loc arr arg =
  let coords =
    match arg with
    | `Tup (_loc,`Com (_,e1,e2))|`Com (_loc,e1,e2) ->
        FanAst.list_of_com' e1 (FanAst.list_of_com' e2 [])
    | _ -> [arg] in
  match coords with
  | [] -> failwith "bigarray_get null list"
  | c1::[] ->
      `App
        (loc,
          (`App
             (loc,
               (`Id
                  (loc,
                    (`Dot
                       (loc, (`Uid (loc, "Bigarray")),
                         (`Dot
                            (loc, (`Uid (loc, "Array1")),
                              (`Lid (loc, "get")))))))), arr)), c1)
  | c1::c2::[] ->
      `App
        (loc,
          (`App
             (loc,
               (`App
                  (loc,
                    (`Id
                       (loc,
                         (`Dot
                            (loc, (`Uid (loc, "Bigarray")),
                              (`Dot
                                 (loc, (`Uid (loc, "Array2")),
                                   (`Lid (loc, "get")))))))), arr)), c1)),
          c2)
  | c1::c2::c3::[] ->
      `App
        (loc,
          (`App
             (loc,
               (`App
                  (loc,
                    (`App
                       (loc,
                         (`Id
                            (loc,
                              (`Dot
                                 (loc, (`Uid (loc, "Bigarray")),
                                   (`Dot
                                      (loc, (`Uid (loc, "Array3")),
                                        (`Lid (loc, "get")))))))), arr)), c1)),
               c2)), c3)
  | c1::c2::c3::coords ->
      `App
        (loc,
          (`App
             (loc,
               (`Id
                  (loc,
                    (`Dot
                       (loc, (`Uid (loc, "Bigarray")),
                         (`Dot
                            (loc, (`Uid (loc, "Genarray")),
                              (`Lid (loc, "get")))))))), arr)),
          (`Array
             (loc,
               (`Sem
                  (loc, c1,
                    (`Sem
                       (loc, c2,
                         (`Sem (loc, c3, (FanAst.sem_of_list coords))))))))))
let bigarray_set loc var newval =
  match var with
  | `App
      (_loc,`App
              (_,`Id
                   (_,`Dot
                        (_,`Uid (_,"Bigarray"),`Dot
                                                 (_,`Uid (_,"Array1"),
                                                  `Lid (_,"get")))),arr),c1)
      ->
      Some
        (`App
           (loc,
             (`App
                (loc,
                  (`App
                     (loc,
                       (`Id
                          (loc,
                            (`Dot
                               (loc, (`Uid (loc, "Bigarray")),
                                 (`Dot
                                    (loc, (`Uid (loc, "Array1")),
                                      (`Lid (loc, "set")))))))), arr)), c1)),
             newval))
  | `App
      (_loc,`App
              (_,`App
                   (_,`Id
                        (_,`Dot
                             (_,`Uid (_,"Bigarray"),`Dot
                                                      (_,`Uid (_,"Array2"),
                                                       `Lid (_,"get")))),arr),c1),c2)
      ->
      Some
        (`App
           (loc,
             (`App
                (loc,
                  (`App
                     (loc,
                       (`App
                          (loc,
                            (`Id
                               (loc,
                                 (`Dot
                                    (loc, (`Uid (loc, "Bigarray")),
                                      (`Dot
                                         (loc, (`Uid (loc, "Array2")),
                                           (`Lid (loc, "set")))))))), arr)),
                       c1)), c2)), newval))
  | `App
      (_loc,`App
              (_,`App
                   (_,`App
                        (_,`Id
                             (_,`Dot
                                  (_,`Uid (_,"Bigarray"),`Dot
                                                           (_,`Uid
                                                                (_,"Array3"),
                                                            `Lid (_,"get")))),arr),c1),c2),c3)
      ->
      Some
        (`Assign
           (loc,
             (`Dot
                (loc,
                  (`App
                     (loc,
                       (`App
                          (loc,
                            (`App
                               (loc,
                                 (`App
                                    (loc,
                                      (`Id
                                         (loc,
                                           (`Dot
                                              (loc, (`Uid (loc, "Bigarray")),
                                                (`Dot
                                                   (loc,
                                                     (`Uid (loc, "Array3")),
                                                     (`Lid (loc, "get")))))))),
                                      arr)), c1)), c2)), c3)),
                  (`Id (loc, (`Lid (loc, "contents")))))), newval))
  | `App
      (_loc,`App
              (_,`Id
                   (_,`Dot
                        (_,`Uid (_,"Bigarray"),`Dot
                                                 (_,`Uid (_,"Genarray"),
                                                  `Lid (_,"get")))),arr),
       `Array (_,coords))
      ->
      Some
        (`App
           (loc,
             (`App
                (loc,
                  (`App
                     (loc,
                       (`Id
                          (loc,
                            (`Dot
                               (loc, (`Uid (loc, "Bigarray")),
                                 (`Dot
                                    (loc, (`Uid (loc, "Genarray")),
                                      (`Lid (loc, "set")))))))), arr)),
                  (`Array (loc, coords)))), newval))
  | _ -> None
let mksequence ?loc  =
  function
  | `Sem (_loc,_,_)|`Ant (_loc,_) as e ->
      let _loc = match loc with | Some x -> x | None  -> _loc in
      `Seq (_loc, e)
  | e -> e
let mksequence' ?loc  =
  function
  | `Sem (_loc,_,_) as e ->
      let _loc = match loc with | Some x -> x | None  -> _loc in
      `Seq (_loc, e)
  | e -> e
let rec to_lid =
  function
  | `Dot (_loc,_,i) -> to_lid i
  | `Lid (_loc,lid) -> lid
  | _ -> assert false
let mkumin loc prefix arg =
  match arg with
  | `Int (_loc,n) -> `Int (loc, (String.neg n))
  | `Int32 (_loc,n) -> `Int32 (loc, (String.neg n))
  | `Int64 (_loc,n) -> `Int64 (loc, (String.neg n))
  | `NativeInt (_loc,n) -> `NativeInt (loc, (String.neg n))
  | `Flo (_loc,n) -> `Flo (loc, (String.neg n))
  | _ -> `App (loc, (`Id (loc, (`Lid (loc, ("~" ^ prefix))))), arg)
let mkassert loc =
  function | `Id (_loc,`Lid (_,"false")) -> `ExAsf loc | e -> `ExAsr (loc, e)
let rec to_generalized x =
  match x with
  | `Arrow (_loc,t1,t2) ->
      let (tl,rt) = to_generalized t2 in ((t1 :: tl), rt)
  | t -> ([], t)