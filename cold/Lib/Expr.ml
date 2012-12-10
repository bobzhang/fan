open FanUtil
module Ast = Camlp4Ast
let rec sep_expr acc =
  function
  | Ast.ExAcc (_loc,e1,e2) -> sep_expr (sep_expr acc e2) e1
  | Ast.ExId (loc,Ast.IdUid (_,s)) as e ->
      (match acc with
       | [] -> [(loc, [], e)]
       | (loc',sl,e)::l -> ((FanLoc.merge loc loc'), (s :: sl), e) :: l)
  | Ast.ExId (_loc,(Ast.IdAcc (_l,_,_) as i)) ->
      sep_expr acc (Ident.normalize_acc i)
  | e -> ((Ast.loc_of_expr e), [], e) :: acc
let rec fa al =
  function | Ast.ExApp (_loc,f,a) -> fa (a :: al) f | f -> (f, al)
let rec apply accu =
  function
  | [] -> accu
  | x::xs ->
      let _loc = Ast.loc_of_expr x in apply (Ast.ExApp (_loc, accu, x)) xs
let mklist _loc =
  let rec loop top =
    function
    | [] -> Ast.ExId (_loc, (Ast.IdUid (_loc, "[]")))
    | e1::el ->
        let _loc =
          if top then _loc else FanLoc.merge (Ast.loc_of_expr e1) _loc in
        Ast.ExApp
          (_loc,
            (Ast.ExApp
               (_loc, (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))), e1)),
            (loop false el)) in
  loop true
let mkumin _loc f arg =
  match arg with
  | Ast.ExInt (_loc,n) -> Ast.ExInt (_loc, (neg_string n))
  | Ast.ExInt32 (_loc,n) -> Ast.ExInt32 (_loc, (neg_string n))
  | Ast.ExInt64 (_loc,n) -> Ast.ExInt64 (_loc, (neg_string n))
  | Ast.ExNativeInt (_loc,n) -> Ast.ExNativeInt (_loc, (neg_string n))
  | Ast.ExFlo (_loc,n) -> Ast.ExFlo (_loc, (neg_string n))
  | _ ->
      Ast.ExApp (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, ("~" ^ f))))), arg)
let mkassert _loc =
  function
  | Ast.ExId (_loc,Ast.IdLid (_,"false")) -> Ast.ExAsf _loc
  | e -> Ast.ExAsr (_loc, e)
let mklist_last ?last  _loc =
  let rec loop top =
    function
    | [] ->
        (match last with
         | Some e -> e
         | None  -> Ast.ExId (_loc, (Ast.IdUid (_loc, "[]"))))
    | e1::el ->
        let _loc =
          if top then _loc else FanLoc.merge (Ast.loc_of_expr e1) _loc in
        Ast.ExApp
          (_loc,
            (Ast.ExApp
               (_loc, (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))), e1)),
            (loop false el)) in
  loop true
let mksequence _loc =
  function
  | Ast.ExSem (_loc,_,_)|Ast.ExAnt (_loc,_) as e -> Ast.ExSeq (_loc, e)
  | e -> e
let mksequence' _loc =
  function | Ast.ExSem (_loc,_,_) as e -> Ast.ExSeq (_loc, e) | e -> e
let bigarray_get _loc arr arg =
  let coords =
    match arg with
    | Ast.ExTup (_loc,Ast.ExCom (_,e1,e2))|Ast.ExCom (_loc,e1,e2) ->
        Ast.list_of_expr e1 (Ast.list_of_expr e2 [])
    | _ -> [arg] in
  match coords with
  | [] -> failwith "bigarray_get null list"
  | c1::[] ->
      Ast.ExApp
        (_loc,
          (Ast.ExApp
             (_loc,
               (Ast.ExId
                  (_loc,
                    (Ast.IdAcc
                       (_loc, (Ast.IdUid (_loc, "Bigarray")),
                         (Ast.IdAcc
                            (_loc, (Ast.IdUid (_loc, "Array1")),
                              (Ast.IdLid (_loc, "get")))))))), arr)), c1)
  | c1::c2::[] ->
      Ast.ExApp
        (_loc,
          (Ast.ExApp
             (_loc,
               (Ast.ExApp
                  (_loc,
                    (Ast.ExId
                       (_loc,
                         (Ast.IdAcc
                            (_loc, (Ast.IdUid (_loc, "Bigarray")),
                              (Ast.IdAcc
                                 (_loc, (Ast.IdUid (_loc, "Array2")),
                                   (Ast.IdLid (_loc, "get")))))))), arr)),
               c1)), c2)
  | c1::c2::c3::[] ->
      Ast.ExApp
        (_loc,
          (Ast.ExApp
             (_loc,
               (Ast.ExApp
                  (_loc,
                    (Ast.ExApp
                       (_loc,
                         (Ast.ExId
                            (_loc,
                              (Ast.IdAcc
                                 (_loc, (Ast.IdUid (_loc, "Bigarray")),
                                   (Ast.IdAcc
                                      (_loc, (Ast.IdUid (_loc, "Array3")),
                                        (Ast.IdLid (_loc, "get")))))))), arr)),
                    c1)), c2)), c3)
  | c1::c2::c3::coords ->
      Ast.ExApp
        (_loc,
          (Ast.ExApp
             (_loc,
               (Ast.ExId
                  (_loc,
                    (Ast.IdAcc
                       (_loc, (Ast.IdUid (_loc, "Bigarray")),
                         (Ast.IdAcc
                            (_loc, (Ast.IdUid (_loc, "Genarray")),
                              (Ast.IdLid (_loc, "get")))))))), arr)),
          (Ast.ExArr
             (_loc,
               (Ast.ExSem
                  (_loc, c1,
                    (Ast.ExSem
                       (_loc, c2,
                         (Ast.ExSem (_loc, c3, (Ast.exSem_of_list coords))))))))))
let bigarray_set _loc var newval =
  match var with
  | Ast.ExApp
      (_loc,Ast.ExApp
       (_,Ast.ExId
        (_,Ast.IdAcc
         (_,Ast.IdUid (_,"Bigarray"),Ast.IdAcc
          (_,Ast.IdUid (_,"Array1"),Ast.IdLid (_,"get")))),arr),c1)
      ->
      Some
        (Ast.ExAss
           (_loc,
             (Ast.ExAcc
                (_loc,
                  (Ast.ExApp
                     (_loc,
                       (Ast.ExApp
                          (_loc,
                            (Ast.ExId
                               (_loc,
                                 (Ast.IdAcc
                                    (_loc, (Ast.IdUid (_loc, "Bigarray")),
                                      (Ast.IdAcc
                                         (_loc, (Ast.IdUid (_loc, "Array1")),
                                           (Ast.IdLid (_loc, "get")))))))),
                            arr)), c1)),
                  (Ast.ExId (_loc, (Ast.IdLid (_loc, "contents")))))),
             newval))
  | Ast.ExApp
      (_loc,Ast.ExApp
       (_,Ast.ExApp
        (_,Ast.ExId
         (_,Ast.IdAcc
          (_,Ast.IdUid (_,"Bigarray"),Ast.IdAcc
           (_,Ast.IdUid (_,"Array2"),Ast.IdLid (_,"get")))),arr),c1),c2)
      ->
      Some
        (Ast.ExAss
           (_loc,
             (Ast.ExAcc
                (_loc,
                  (Ast.ExApp
                     (_loc,
                       (Ast.ExApp
                          (_loc,
                            (Ast.ExApp
                               (_loc,
                                 (Ast.ExId
                                    (_loc,
                                      (Ast.IdAcc
                                         (_loc,
                                           (Ast.IdUid (_loc, "Bigarray")),
                                           (Ast.IdAcc
                                              (_loc,
                                                (Ast.IdUid (_loc, "Array2")),
                                                (Ast.IdLid (_loc, "get")))))))),
                                 arr)), c1)), c2)),
                  (Ast.ExId (_loc, (Ast.IdLid (_loc, "contents")))))),
             newval))
  | Ast.ExApp
      (_loc,Ast.ExApp
       (_,Ast.ExApp
        (_,Ast.ExApp
         (_,Ast.ExId
          (_,Ast.IdAcc
           (_,Ast.IdUid (_,"Bigarray"),Ast.IdAcc
            (_,Ast.IdUid (_,"Array3"),Ast.IdLid (_,"get")))),arr),c1),c2),c3)
      ->
      Some
        (Ast.ExAss
           (_loc,
             (Ast.ExAcc
                (_loc,
                  (Ast.ExApp
                     (_loc,
                       (Ast.ExApp
                          (_loc,
                            (Ast.ExApp
                               (_loc,
                                 (Ast.ExApp
                                    (_loc,
                                      (Ast.ExId
                                         (_loc,
                                           (Ast.IdAcc
                                              (_loc,
                                                (Ast.IdUid (_loc, "Bigarray")),
                                                (Ast.IdAcc
                                                   (_loc,
                                                     (Ast.IdUid
                                                        (_loc, "Array3")),
                                                     (Ast.IdLid (_loc, "get")))))))),
                                      arr)), c1)), c2)), c3)),
                  (Ast.ExId (_loc, (Ast.IdLid (_loc, "contents")))))),
             newval))
  | Ast.ExApp
      (_loc,Ast.ExApp
       (_,Ast.ExId
        (_,Ast.IdAcc
         (_,Ast.IdUid (_,"Bigarray"),Ast.IdAcc
          (_,Ast.IdUid (_,"Genarray"),Ast.IdLid (_,"get")))),arr),Ast.ExArr
       (_,coords))
      ->
      Some
        (Ast.ExApp
           (_loc,
             (Ast.ExApp
                (_loc,
                  (Ast.ExApp
                     (_loc,
                       (Ast.ExId
                          (_loc,
                            (Ast.IdAcc
                               (_loc, (Ast.IdUid (_loc, "Bigarray")),
                                 (Ast.IdAcc
                                    (_loc, (Ast.IdUid (_loc, "Genarray")),
                                      (Ast.IdLid (_loc, "set")))))))), arr)),
                  (Ast.ExArr (_loc, coords)))), newval))
  | _ -> None
let rec pattern_eq_expression p e =
  match (p, e) with
  | (Ast.PaId (_loc,Ast.IdLid (_,a)),Ast.ExId (_l,Ast.IdLid (_,b))) -> a = b
  | (Ast.PaId (_loc,Ast.IdUid (_,a)),Ast.ExId (_l,Ast.IdUid (_,b))) -> a = b
  | (Ast.PaApp (_loc,p1,p2),Ast.ExApp (_l,e1,e2)) ->
      (pattern_eq_expression p1 e1) && (pattern_eq_expression p2 e2)
  | _ -> false
let map _loc p e l =
  match (p, e) with
  | (Ast.PaId (_loc,Ast.IdLid (_,x)),Ast.ExId (_l,Ast.IdLid (_,y))) when
      x = y -> l
  | _ ->
      if Ast.is_irrefut_patt p
      then
        Ast.ExApp
          (_loc,
            (Ast.ExApp
               (_loc,
                 (Ast.ExId
                    (_loc,
                      (Ast.IdAcc
                         (_loc, (Ast.IdUid (_loc, "List")),
                           (Ast.IdLid (_loc, "map")))))),
                 (Ast.ExFun
                    (_loc, (Ast.McArr (_loc, p, (Ast.ExNil _loc), e)))))), l)
      else
        Ast.ExApp
          (_loc,
            (Ast.ExApp
               (_loc,
                 (Ast.ExApp
                    (_loc,
                      (Ast.ExId
                         (_loc,
                           (Ast.IdAcc
                              (_loc, (Ast.IdUid (_loc, "List")),
                                (Ast.IdLid (_loc, "fold_right")))))),
                      (Ast.ExFun
                         (_loc,
                           (Ast.McOr
                              (_loc,
                                (Ast.McArr
                                   (_loc, p,
                                     (Ast.ExId
                                        (_loc, (Ast.IdLid (_loc, "true")))),
                                     (Ast.ExApp
                                        (_loc,
                                          (Ast.ExFun
                                             (_loc,
                                               (Ast.McArr
                                                  (_loc,
                                                    (Ast.PaId
                                                       (_loc,
                                                         (Ast.IdLid
                                                            (_loc, "x")))),
                                                    (Ast.ExNil _loc),
                                                    (Ast.ExFun
                                                       (_loc,
                                                         (Ast.McArr
                                                            (_loc,
                                                              (Ast.PaId
                                                                 (_loc,
                                                                   (Ast.IdLid
                                                                    (_loc,
                                                                    "xs")))),
                                                              (Ast.ExNil _loc),
                                                              (Ast.ExApp
                                                                 (_loc,
                                                                   (Ast.ExApp
                                                                    (_loc,
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdUid
                                                                    (_loc,
                                                                    "::")))),
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "x")))))),
                                                                   (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "xs")))))))))))))),
                                          e)))),
                                (Ast.McArr
                                   (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc),
                                     (Ast.ExFun
                                        (_loc,
                                          (Ast.McArr
                                             (_loc,
                                               (Ast.PaId
                                                  (_loc,
                                                    (Ast.IdLid (_loc, "l")))),
                                               (Ast.ExNil _loc),
                                               (Ast.ExId
                                                  (_loc,
                                                    (Ast.IdLid (_loc, "l")))))))))))))))),
                 l)), (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]")))))
let filter _loc p b l =
  if Ast.is_irrefut_patt p
  then
    Ast.ExApp
      (_loc,
        (Ast.ExApp
           (_loc,
             (Ast.ExId
                (_loc,
                  (Ast.IdAcc
                     (_loc, (Ast.IdUid (_loc, "List")),
                       (Ast.IdLid (_loc, "filter")))))),
             (Ast.ExFun (_loc, (Ast.McArr (_loc, p, (Ast.ExNil _loc), b)))))),
        l)
  else
    Ast.ExApp
      (_loc,
        (Ast.ExApp
           (_loc,
             (Ast.ExId
                (_loc,
                  (Ast.IdAcc
                     (_loc, (Ast.IdUid (_loc, "List")),
                       (Ast.IdLid (_loc, "filter")))))),
             (Ast.ExFun
                (_loc,
                  (Ast.McOr
                     (_loc,
                       (Ast.McArr
                          (_loc, p,
                            (Ast.ExId (_loc, (Ast.IdLid (_loc, "true")))), b)),
                       (Ast.McArr
                          (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc),
                            (Ast.ExId (_loc, (Ast.IdLid (_loc, "false")))))))))))),
        l)
let concat _loc l =
  Ast.ExApp
    (_loc,
      (Ast.ExId
         (_loc,
           (Ast.IdAcc
              (_loc, (Ast.IdUid (_loc, "List")),
                (Ast.IdLid (_loc, "concat")))))), l)
let rec compr _loc e =
  function
  | (`gen (p,l))::[] -> map _loc p e l
  | (`gen (p,l))::(`cond b)::items ->
      compr _loc e ((`gen (p, (filter _loc p b l))) :: items)
  | (`gen (p,l))::((`gen (_,_))::_ as is) ->
      concat _loc (map _loc p (compr _loc e is) l)
  | _ -> raise Stream.Failure
let bad_patt _loc =
  FanLoc.raise _loc
    (Failure "this macro cannot be used in a pattern (see its definition)")
let substp _loc env =
  let rec loop =
    function
    | Ast.ExApp (_loc,e1,e2) -> Ast.PaApp (_loc, (loop e1), (loop e2))
    | Ast.ExNil _loc -> Ast.PaNil _loc
    | Ast.ExId (_loc,Ast.IdLid (_,x)) ->
        (try List.assoc x env
         with | Not_found  -> Ast.PaId (_loc, (Ast.IdLid (_loc, x))))
    | Ast.ExId (_loc,Ast.IdUid (_,x)) ->
        (try List.assoc x env
         with | Not_found  -> Ast.PaId (_loc, (Ast.IdUid (_loc, x))))
    | Ast.ExInt (_loc,x) -> Ast.PaInt (_loc, x)
    | Ast.ExStr (_loc,s) -> Ast.PaStr (_loc, s)
    | Ast.ExTup (_loc,x) -> Ast.PaTup (_loc, (loop x))
    | Ast.ExCom (_loc,x1,x2) -> Ast.PaCom (_loc, (loop x1), (loop x2))
    | Ast.ExRec (_loc,bi,Ast.ExNil _) ->
        let rec substbi =
          function
          | Ast.RbSem (_loc,b1,b2) ->
              Ast.PaSem (_loc, (substbi b1), (substbi b2))
          | Ast.RbEq (_loc,i,e) -> Ast.PaEq (_loc, i, (loop e))
          | _ -> bad_patt _loc in
        Ast.PaRec (_loc, (substbi bi))
    | _ -> bad_patt _loc in
  loop
class subst _loc env =
  object 
    inherit  (Ast.reloc _loc) as super
    method! expr =
      function
      | Ast.ExId (_loc,Ast.IdLid (_,x))|Ast.ExId (_loc,Ast.IdUid (_,x)) as e
          -> (try List.assoc x env with | Not_found  -> super#expr e)
      | Ast.ExApp
          (_loc,Ast.ExId (_,Ast.IdUid (_,"LOCATION_OF")),Ast.ExId
           (_,Ast.IdLid (_,x)))|Ast.ExApp
          (_loc,Ast.ExId (_,Ast.IdUid (_,"LOCATION_OF")),Ast.ExId
           (_,Ast.IdUid (_,x)))
          as e ->
          (try
             let loc = Ast.loc_of_expr (List.assoc x env) in
             let (a,b,c,d,e,f,g,h) = FanLoc.to_tuple loc in
             Ast.ExApp
               (_loc,
                 (Ast.ExId
                    (_loc,
                      (Ast.IdAcc
                         (_loc, (Ast.IdUid (_loc, "FanLoc")),
                           (Ast.IdLid (_loc, "of_tuple")))))),
                 (Ast.ExTup
                    (_loc,
                      (Ast.ExCom
                         (_loc,
                           (Ast.ExStr (_loc, (Ast.safe_string_escaped a))),
                           (Ast.ExCom
                              (_loc,
                                (Ast.ExCom
                                   (_loc,
                                     (Ast.ExCom
                                        (_loc,
                                          (Ast.ExCom
                                             (_loc,
                                               (Ast.ExCom
                                                  (_loc,
                                                    (Ast.ExCom
                                                       (_loc,
                                                         (Ast.ExInt
                                                            (_loc,
                                                              (string_of_int
                                                                 b))),
                                                         (Ast.ExInt
                                                            (_loc,
                                                              (string_of_int
                                                                 c))))),
                                                    (Ast.ExInt
                                                       (_loc,
                                                         (string_of_int d))))),
                                               (Ast.ExInt
                                                  (_loc, (string_of_int e))))),
                                          (Ast.ExInt
                                             (_loc, (string_of_int f))))),
                                     (Ast.ExInt (_loc, (string_of_int g))))),
                                (if h
                                 then
                                   Ast.ExId
                                     (_loc, (Ast.IdLid (_loc, "true")))
                                 else
                                   Ast.ExId
                                     (_loc, (Ast.IdLid (_loc, "false")))))))))))
           with | Not_found  -> super#expr e)
      | e -> super#expr e
    method! patt =
      function
      | Ast.PaId (_loc,Ast.IdLid (_,x))|Ast.PaId (_loc,Ast.IdUid (_,x)) as p
          ->
          (try substp _loc [] (List.assoc x env)
           with | Not_found  -> super#patt p)
      | p -> super#patt p
  end
let capture_antiquot =
  object 
    inherit  Camlp4Ast.map as super
    val mutable constraints = []
    method! patt =
      function
      | Ast.PaAnt (_loc,s)|Ast.PaStr (_loc,s) as p when is_antiquot s ->
          (match view_antiquot s with
           | Some (_name,code) ->
               let cons = Ast.ExId (_loc, (Ast.IdLid (_loc, code))) in
               let code' = "__fan__" ^ code in
               let cons' = Ast.ExId (_loc, (Ast.IdLid (_loc, code'))) in
               let () = constraints <- (cons, cons') :: constraints in
               Ast.PaId (_loc, (Ast.IdLid (_loc, code')))
           | None  -> p)
      | p -> super#patt p
    method get_captured_variables = constraints
    method clear_captured_variables = constraints <- []
  end
let filter_patt_with_captured_variables patt =
  capture_antiquot#clear_captured_variables;
  (let patt = capture_antiquot#patt patt in
   let constraints = capture_antiquot#get_captured_variables in
   (patt, constraints))
let rec string_of_ident =
  function
  | Ast.IdLid (_loc,s) -> s
  | Ast.IdUid (_loc,s) -> s
  | Ast.IdAcc (_loc,i1,i2) ->
      "acc_" ^ ((string_of_ident i1) ^ ("_" ^ (string_of_ident i2)))
  | Ast.IdApp (_loc,i1,i2) ->
      "app_" ^ ((string_of_ident i1) ^ ("_" ^ (string_of_ident i2)))
  | Ast.IdAnt (_loc,_) -> assert false
let tuple _loc =
  function
  | [] -> Ast.ExId (_loc, (Ast.IdUid (_loc, "()")))
  | p::[] -> p
  | e::es -> Ast.ExTup (_loc, (Ast.ExCom (_loc, e, (Ast.exCom_of_list es))))
let fun_args _loc args body =
  if args = []
  then
    Ast.ExFun
      (_loc,
        (Ast.McArr
           (_loc, (Ast.PaId (_loc, (Ast.IdUid (_loc, "()")))),
             (Ast.ExNil _loc), body)))
  else
    List.fold_right
      (fun arg  body  ->
         Ast.ExFun (_loc, (Ast.McArr (_loc, arg, (Ast.ExNil _loc), body))))
      args body
let fun_apply _loc e args =
  if args = []
  then Ast.ExApp (_loc, e, (Ast.ExId (_loc, (Ast.IdUid (_loc, "()")))))
  else List.fold_left (fun e  arg  -> Ast.ExApp (_loc, e, arg)) e args