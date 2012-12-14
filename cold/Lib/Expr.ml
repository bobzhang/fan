open LibUtil
open Basic
open FanUtil
module Ast = Camlp4Ast
let rec sep_dot_expr acc =
  function
  | Ast.ExAcc (_loc,e1,e2) -> sep_dot_expr (sep_dot_expr acc e2) e1
  | Ast.ExId (loc,Ast.IdUid (_,s)) as e ->
      (match acc with
       | [] -> [(loc, [], e)]
       | (loc',sl,e)::l -> ((FanLoc.merge loc loc'), (s :: sl), e) :: l)
  | Ast.ExId (_loc,(Ast.IdAcc (_l,_,_) as i)) ->
      sep_dot_expr acc (Ident.normalize_acc i)
  | e -> ((Ast.loc_of_expr e), [], e) :: acc
let rec apply accu =
  function
  | [] -> accu
  | x::xs ->
      let _loc = Ast.loc_of_expr x in apply (Ast.ExApp (_loc, accu, x)) xs
let mklist loc =
  let rec loop top =
    function
    | [] -> Ast.ExId (_loc, (Ast.IdUid (_loc, "[]")))
    | e1::el ->
        let _loc = if top then loc else FanLoc.merge (Ast.loc_of_expr e1) loc in
        Ast.ExApp
          (_loc,
            (Ast.ExApp
               (_loc, (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))), e1)),
            (loop false el)) in
  loop true
let mksequence loc =
  function
  | Ast.ExSem (_loc,_,_)|Ast.ExAnt (_loc,_) as e -> Ast.ExSeq (loc, e)
  | e -> e
let mksequence' loc =
  function | Ast.ExSem (_loc,_,_) as e -> Ast.ExSeq (loc, e) | e -> e
let mkumin loc prefix arg =
  match arg with
  | Ast.ExInt (_loc,n) -> Ast.ExInt (loc, (neg_string n))
  | Ast.ExInt32 (_loc,n) -> Ast.ExInt32 (loc, (neg_string n))
  | Ast.ExInt64 (_loc,n) -> Ast.ExInt64 (loc, (neg_string n))
  | Ast.ExNativeInt (_loc,n) -> Ast.ExNativeInt (loc, (neg_string n))
  | Ast.ExFlo (_loc,n) -> Ast.ExFlo (loc, (neg_string n))
  | _ ->
      Ast.ExApp
        (loc, (Ast.ExId (loc, (Ast.IdLid (loc, ("~" ^ prefix))))), arg)
let mkassert loc =
  function
  | Ast.ExId (_loc,Ast.IdLid (_,"false")) -> Ast.ExAsf loc
  | e -> Ast.ExAsr (loc, e)
let bigarray_get loc arr arg =
  let coords =
    match arg with
    | Ast.ExTup (_loc,Ast.ExCom (_,e1,e2))|Ast.ExCom (_loc,e1,e2) ->
        Ast.list_of_expr e1 (Ast.list_of_expr e2 [])
    | _ -> [arg] in
  match coords with
  | [] -> failwith "bigarray_get null list"
  | c1::[] ->
      Ast.ExApp
        (loc,
          (Ast.ExApp
             (loc,
               (Ast.ExId
                  (loc,
                    (Ast.IdAcc
                       (loc, (Ast.IdUid (loc, "Bigarray")),
                         (Ast.IdAcc
                            (loc, (Ast.IdUid (loc, "Array1")),
                              (Ast.IdLid (loc, "get")))))))), arr)), c1)
  | c1::c2::[] ->
      Ast.ExApp
        (loc,
          (Ast.ExApp
             (loc,
               (Ast.ExApp
                  (loc,
                    (Ast.ExId
                       (loc,
                         (Ast.IdAcc
                            (loc, (Ast.IdUid (loc, "Bigarray")),
                              (Ast.IdAcc
                                 (loc, (Ast.IdUid (loc, "Array2")),
                                   (Ast.IdLid (loc, "get")))))))), arr)), c1)),
          c2)
  | c1::c2::c3::[] ->
      Ast.ExApp
        (loc,
          (Ast.ExApp
             (loc,
               (Ast.ExApp
                  (loc,
                    (Ast.ExApp
                       (loc,
                         (Ast.ExId
                            (loc,
                              (Ast.IdAcc
                                 (loc, (Ast.IdUid (loc, "Bigarray")),
                                   (Ast.IdAcc
                                      (loc, (Ast.IdUid (loc, "Array3")),
                                        (Ast.IdLid (loc, "get")))))))), arr)),
                    c1)), c2)), c3)
  | c1::c2::c3::coords ->
      Ast.ExApp
        (loc,
          (Ast.ExApp
             (loc,
               (Ast.ExId
                  (loc,
                    (Ast.IdAcc
                       (loc, (Ast.IdUid (loc, "Bigarray")),
                         (Ast.IdAcc
                            (loc, (Ast.IdUid (loc, "Genarray")),
                              (Ast.IdLid (loc, "get")))))))), arr)),
          (Ast.ExArr
             (loc,
               (Ast.ExSem
                  (loc, c1,
                    (Ast.ExSem
                       (loc, c2,
                         (Ast.ExSem (loc, c3, (Ast.exSem_of_list coords))))))))))
let bigarray_set loc var newval =
  match var with
  | Ast.ExApp
      (_loc,Ast.ExApp
       (_,Ast.ExId
        (_,Ast.IdAcc
         (_,Ast.IdUid (_,"Bigarray"),Ast.IdAcc
          (_,Ast.IdUid (_,"Array1"),Ast.IdLid (_,"get")))),arr),c1)
      ->
      Some
        (Ast.ExApp
           (loc,
             (Ast.ExApp
                (loc,
                  (Ast.ExApp
                     (loc,
                       (Ast.ExId
                          (loc,
                            (Ast.IdAcc
                               (loc, (Ast.IdUid (loc, "Bigarray")),
                                 (Ast.IdAcc
                                    (loc, (Ast.IdUid (loc, "Array1")),
                                      (Ast.IdLid (loc, "set")))))))), arr)),
                  c1)), newval))
  | Ast.ExApp
      (_loc,Ast.ExApp
       (_,Ast.ExApp
        (_,Ast.ExId
         (_,Ast.IdAcc
          (_,Ast.IdUid (_,"Bigarray"),Ast.IdAcc
           (_,Ast.IdUid (_,"Array2"),Ast.IdLid (_,"get")))),arr),c1),c2)
      ->
      Some
        (Ast.ExApp
           (loc,
             (Ast.ExApp
                (loc,
                  (Ast.ExApp
                     (loc,
                       (Ast.ExApp
                          (loc,
                            (Ast.ExId
                               (loc,
                                 (Ast.IdAcc
                                    (loc, (Ast.IdUid (loc, "Bigarray")),
                                      (Ast.IdAcc
                                         (loc, (Ast.IdUid (loc, "Array2")),
                                           (Ast.IdLid (loc, "set")))))))),
                            arr)), c1)), c2)), newval))
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
           (loc,
             (Ast.ExAcc
                (loc,
                  (Ast.ExApp
                     (loc,
                       (Ast.ExApp
                          (loc,
                            (Ast.ExApp
                               (loc,
                                 (Ast.ExApp
                                    (loc,
                                      (Ast.ExId
                                         (loc,
                                           (Ast.IdAcc
                                              (loc,
                                                (Ast.IdUid (loc, "Bigarray")),
                                                (Ast.IdAcc
                                                   (loc,
                                                     (Ast.IdUid
                                                        (loc, "Array3")),
                                                     (Ast.IdLid (loc, "get")))))))),
                                      arr)), c1)), c2)), c3)),
                  (Ast.ExId (loc, (Ast.IdLid (loc, "contents")))))), newval))
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
           (loc,
             (Ast.ExApp
                (loc,
                  (Ast.ExApp
                     (loc,
                       (Ast.ExId
                          (loc,
                            (Ast.IdAcc
                               (loc, (Ast.IdUid (loc, "Bigarray")),
                                 (Ast.IdAcc
                                    (loc, (Ast.IdUid (loc, "Genarray")),
                                      (Ast.IdLid (loc, "set")))))))), arr)),
                  (Ast.ExArr (loc, coords)))), newval))
  | _ -> None
let rec pattern_eq_expression p e =
  match (p, e) with
  | (Ast.PaId (_loc,Ast.IdLid (_,a)),Ast.ExId (_,Ast.IdLid (_,b)))|(Ast.PaId
                                                                    (_loc,Ast.IdUid
                                                                    (_,a)),Ast.ExId
                                                                    (_,Ast.IdUid
                                                                    (_,b)))
      -> a = b
  | (Ast.PaApp (_loc,p1,p2),Ast.ExApp (_,e1,e2)) ->
      (pattern_eq_expression p1 e1) && (pattern_eq_expression p2 e2)
  | _ -> false
let map loc p e l =
  match (p, e) with
  | (Ast.PaId (_loc,Ast.IdLid (_,x)),Ast.ExId (_,Ast.IdLid (_,y))) when 
      x = y -> l
  | _ ->
      if Ast.is_irrefut_patt p
      then
        Ast.ExApp
          (loc,
            (Ast.ExApp
               (loc,
                 (Ast.ExId
                    (loc,
                      (Ast.IdAcc
                         (loc, (Ast.IdUid (loc, "List")),
                           (Ast.IdLid (loc, "map")))))),
                 (Ast.ExFun (loc, (Ast.McArr (loc, p, (Ast.ExNil loc), e)))))),
            l)
      else
        Ast.ExApp
          (loc,
            (Ast.ExApp
               (loc,
                 (Ast.ExApp
                    (loc,
                      (Ast.ExId
                         (loc,
                           (Ast.IdAcc
                              (loc, (Ast.IdUid (loc, "List")),
                                (Ast.IdLid (loc, "fold_right")))))),
                      (Ast.ExFun
                         (loc,
                           (Ast.McOr
                              (loc,
                                (Ast.McArr
                                   (loc, p,
                                     (Ast.ExId
                                        (loc, (Ast.IdLid (loc, "true")))),
                                     (Ast.ExApp
                                        (loc,
                                          (Ast.ExFun
                                             (loc,
                                               (Ast.McArr
                                                  (loc,
                                                    (Ast.PaId
                                                       (loc,
                                                         (Ast.IdLid
                                                            (loc, "x")))),
                                                    (Ast.ExNil loc),
                                                    (Ast.ExFun
                                                       (loc,
                                                         (Ast.McArr
                                                            (loc,
                                                              (Ast.PaId
                                                                 (loc,
                                                                   (Ast.IdLid
                                                                    (loc,
                                                                    "xs")))),
                                                              (Ast.ExNil loc),
                                                              (Ast.ExApp
                                                                 (loc,
                                                                   (Ast.ExApp
                                                                    (loc,
                                                                    (Ast.ExId
                                                                    (loc,
                                                                    (Ast.IdUid
                                                                    (loc,
                                                                    "::")))),
                                                                    (Ast.ExId
                                                                    (loc,
                                                                    (Ast.IdLid
                                                                    (loc,
                                                                    "x")))))),
                                                                   (Ast.ExId
                                                                    (loc,
                                                                    (Ast.IdLid
                                                                    (loc,
                                                                    "xs")))))))))))))),
                                          e)))),
                                (Ast.McArr
                                   (loc, (Ast.PaAny loc), (Ast.ExNil loc),
                                     (Ast.ExFun
                                        (loc,
                                          (Ast.McArr
                                             (loc,
                                               (Ast.PaId
                                                  (loc,
                                                    (Ast.IdLid (loc, "l")))),
                                               (Ast.ExNil loc),
                                               (Ast.ExId
                                                  (loc,
                                                    (Ast.IdLid (loc, "l")))))))))))))))),
                 l)), (Ast.ExId (loc, (Ast.IdUid (loc, "[]")))))
let filter loc p b l =
  if Ast.is_irrefut_patt p
  then
    Ast.ExApp
      (loc,
        (Ast.ExApp
           (loc,
             (Ast.ExId
                (loc,
                  (Ast.IdAcc
                     (loc, (Ast.IdUid (loc, "List")),
                       (Ast.IdLid (loc, "filter")))))),
             (Ast.ExFun (loc, (Ast.McArr (loc, p, (Ast.ExNil loc), b)))))),
        l)
  else
    Ast.ExApp
      (loc,
        (Ast.ExApp
           (loc,
             (Ast.ExId
                (loc,
                  (Ast.IdAcc
                     (loc, (Ast.IdUid (loc, "List")),
                       (Ast.IdLid (loc, "filter")))))),
             (Ast.ExFun
                (loc,
                  (Ast.McOr
                     (loc,
                       (Ast.McArr
                          (loc, p,
                            (Ast.ExId (loc, (Ast.IdLid (loc, "true")))), b)),
                       (Ast.McArr
                          (loc, (Ast.PaAny loc), (Ast.ExNil loc),
                            (Ast.ExId (loc, (Ast.IdLid (loc, "false")))))))))))),
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
let substp loc env =
  let rec loop =
    function
    | Ast.ExApp (_loc,e1,e2) -> Ast.PaApp (loc, (loop e1), (loop e2))
    | Ast.ExNil _loc -> Ast.PaNil loc
    | Ast.ExId (_loc,Ast.IdLid (_,x)) ->
        (try List.assoc x env
         with | Not_found  -> Ast.PaId (loc, (Ast.IdLid (loc, x))))
    | Ast.ExId (_loc,Ast.IdUid (_,x)) ->
        (try List.assoc x env
         with | Not_found  -> Ast.PaId (loc, (Ast.IdUid (loc, x))))
    | Ast.ExInt (_loc,x) -> Ast.PaInt (loc, x)
    | Ast.ExStr (_loc,s) -> Ast.PaStr (loc, s)
    | Ast.ExTup (_loc,x) -> Ast.PaTup (loc, (loop x))
    | Ast.ExCom (_loc,x1,x2) -> Ast.PaCom (loc, (loop x1), (loop x2))
    | Ast.ExRec (_loc,bi,Ast.ExNil _) ->
        let rec substbi =
          function
          | Ast.RbSem (_loc,b1,b2) ->
              Ast.PaSem (loc, (substbi b1), (substbi b2))
          | Ast.RbEq (_loc,i,e) -> Ast.PaEq (loc, i, (loop e))
          | _ -> bad_patt _loc in
        Ast.PaRec (loc, (substbi bi))
    | _ -> bad_patt loc in
  loop
class subst loc env =
  object 
    inherit  (Ast.reloc loc) as super
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
          (try substp loc [] (List.assoc x env)
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
let _loc = FanLoc.ghost
let app a b = Ast.ExApp (_loc, a, b)
let comma a b = Ast.ExCom (_loc, a, b)
let (<$) = app
let rec apply acc = function | [] -> acc | x::xs -> apply (app acc x) xs
let sem a b = Ast.ExSem (_loc, a, b)
let list_of_app ty =
  let rec loop t acc =
    match t with
    | Ast.ExApp (_loc,t1,t2) -> loop t1 (t2 :: acc)
    | Ast.ExNil _loc -> acc
    | i -> i :: acc in
  loop ty []
let list_of_com ty =
  let rec loop t acc =
    match t with
    | Ast.ExCom (_loc,t1,t2) -> t1 :: (loop t2 acc)
    | Ast.ExNil _loc -> acc
    | i -> i :: acc in
  loop ty []
let list_of_sem ty =
  let rec loop t acc =
    match t with
    | Ast.ExSem (_loc,t1,t2) -> t1 :: (loop t2 acc)
    | Ast.ExNil _loc -> acc
    | i -> i :: acc in
  loop ty []
let rec view_app acc =
  function | Ast.ExApp (_loc,f,a) -> view_app (a :: acc) f | f -> (f, acc)
let app_of_list =
  function | [] -> Ast.ExNil _loc | l -> List.reduce_left app l
let com_of_list =
  function | [] -> Ast.ExNil _loc | l -> List.reduce_right comma l
let sem_of_list =
  function | [] -> Ast.ExNil _loc | l -> List.reduce_right sem l
let tuple_of_list =
  function
  | [] -> invalid_arg "tuple_of_list while list is empty"
  | x::[] -> x
  | xs -> Ast.ExTup (_loc, (com_of_list xs))
let mk_list lst =
  let rec loop =
    function
    | [] -> Ast.ExId (_loc, (Ast.IdUid (_loc, "[]")))
    | x::xs ->
        Ast.ExApp
          (_loc,
            (Ast.ExApp (_loc, (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))), x)),
            (loop xs)) in
  loop lst
let mk_array arr =
  let items = (arr |> Array.to_list) |> sem_of_list in
  Ast.ExArr (_loc, items)
let of_str s =
  let len = String.length s in
  if len = 0
  then invalid_arg "[expr|patt]_of_str len=0"
  else
    (match s.[0] with
     | '`' -> Ast.ExVrn (_loc, (String.sub s 1 (len - 1)))
     | x when Char.is_uppercase x -> Ast.ExId (_loc, (Ast.IdUid (_loc, s)))
     | _ -> Ast.ExId (_loc, (Ast.IdLid (_loc, s))))
let of_ident_number cons n =
  apply (Ast.ExId (_loc, cons))
    (List.init n (fun i  -> Ast.ExId (_loc, (xid i))))
let (+>) f names =
  apply f
    (List.map (fun lid  -> Ast.ExId (_loc, (Ast.IdLid (_loc, lid)))) names)
let gen_tuple_first ~number  ~off  =
  match number with
  | 1 -> Ast.ExId (_loc, (xid ~off 0))
  | n when n > 1 ->
      let lst =
        zfold_left ~start:1 ~until:(number - 1)
          ~acc:(Ast.ExId (_loc, (xid ~off 0)))
          (fun acc  i  -> comma acc (Ast.ExId (_loc, (xid ~off i)))) in
      Ast.ExTup (_loc, lst)
  | _ -> invalid_arg "n < 1 in gen_tuple_first"
let gen_tuple_second ~number  ~off  =
  match number with
  | 1 -> Ast.ExId (_loc, (xid ~off:0 off))
  | n when n > 1 ->
      let lst =
        zfold_left ~start:1 ~until:(number - 1)
          ~acc:(Ast.ExId (_loc, (xid ~off:0 off)))
          (fun acc  i  -> comma acc (Ast.ExId (_loc, (xid ~off:i off)))) in
      Ast.ExTup (_loc, lst)
  | _ -> invalid_arg "n < 1 in gen_tuple_first "
let tuple_of_number ast n =
  let res =
    zfold_left ~start:1 ~until:(n - 1) ~acc:ast
      (fun acc  _  -> comma acc ast) in
  if n > 1 then Ast.ExTup (_loc, res) else res
let tuple_of_list lst =
  let len = List.length lst in
  match len with
  | 1 -> List.hd lst
  | n when n > 1 -> Ast.ExTup (_loc, (List.reduce_left comma lst))
  | _ -> invalid_arg "tuple_of_list n < 1"
let gen_tuple_n ~arity  cons n =
  let args =
    List.init arity
      (fun i  -> List.init n (fun j  -> Ast.ExId (_loc, (xid ~off:i j)))) in
  let pat = of_str cons in
  (List.map (fun lst  -> apply pat lst) args) |> tuple_of_list
let mk_unary_min f arg =
  match arg with
  | Ast.ExInt (_loc,n) -> Ast.ExInt (_loc, (String.neg n))
  | Ast.ExInt32 (_loc,n) -> Ast.ExInt32 (_loc, (String.neg n))
  | Ast.ExInt64 (_loc,n) -> Ast.ExInt64 (_loc, (String.neg n))
  | Ast.ExNativeInt (_loc,n) -> Ast.ExNativeInt (_loc, (String.neg n))
  | Ast.ExFlo (_loc,n) -> Ast.ExFlo (_loc, (String.neg n))
  | _ ->
      Ast.ExApp (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, ("~" ^ f))))), arg)
let mk_assert =
  function
  | Ast.ExId (_loc,Ast.IdLid (_,"false")) -> Ast.ExAsf _loc
  | e -> Ast.ExAsr (_loc, e)
let mk_record label_exprs =
  let rec_bindings =
    List.map
      (fun (label,expr)  -> Ast.RbEq (_loc, (Ast.IdLid (_loc, label)), expr))
      label_exprs in
  Ast.ExRec (_loc, (Ast.rbSem_of_list rec_bindings), (Ast.ExNil _loc))
let failure =
  Ast.ExApp
    (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "raise")))),
      (Ast.ExApp
         (_loc, (Ast.ExId (_loc, (Ast.IdUid (_loc, "Failure")))),
           (Ast.ExStr (_loc, "metafilter: Cannot handle that kind of types ")))))
let (<+) names acc =
  List.fold_right
    (fun name  acc  ->
       Ast.ExFun
         (_loc,
           (Ast.McArr
              (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, name)))),
                (Ast.ExNil _loc), acc)))) names acc
let (<+<) patts acc =
  List.fold_right
    (fun p  acc  ->
       Ast.ExFun (_loc, (Ast.McArr (_loc, p, (Ast.ExNil _loc), acc)))) patts
    acc
let mk_seq es =
  let _loc = FanLoc.ghost in Ast.ExSeq (_loc, (Ast.exSem_of_list es))
let mep_comma x y =
  Ast.ExApp
    (_loc,
      (Ast.ExApp
         (_loc,
           (Ast.ExApp
              (_loc,
                (Ast.ExId
                   (_loc,
                     (Ast.IdAcc
                        (_loc, (Ast.IdUid (_loc, "Ast")),
                          (Ast.IdUid (_loc, "PaCom")))))),
                (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))), x)), y)
let mep_app x y =
  Ast.ExApp
    (_loc,
      (Ast.ExApp
         (_loc,
           (Ast.ExApp
              (_loc,
                (Ast.ExId
                   (_loc,
                     (Ast.IdAcc
                        (_loc, (Ast.IdUid (_loc, "Ast")),
                          (Ast.IdUid (_loc, "PaApp")))))),
                (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))), x)), y)
let mk_tuple_ep =
  function
  | [] -> assert false
  | x::[] -> x
  | xs ->
      Ast.ExApp
        (_loc,
          (Ast.ExApp
             (_loc,
               (Ast.ExId
                  (_loc,
                    (Ast.IdAcc
                       (_loc, (Ast.IdUid (_loc, "Ast")),
                         (Ast.IdUid (_loc, "PaTup")))))),
               (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))),
          (List.reduce_right mep_comma xs))
let mep_of_str s =
  let u =
    Ast.ExApp
      (_loc,
        (Ast.ExApp
           (_loc,
             (Ast.ExId
                (_loc,
                  (Ast.IdAcc
                     (_loc, (Ast.IdUid (_loc, "Ast")),
                       (Ast.IdUid (_loc, "IdUid")))))),
             (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))),
        (Ast.ExStr (_loc, s))) in
  Ast.ExApp
    (_loc,
      (Ast.ExApp
         (_loc,
           (Ast.ExId
              (_loc,
                (Ast.IdAcc
                   (_loc, (Ast.IdUid (_loc, "Ast")),
                     (Ast.IdUid (_loc, "PaId")))))),
           (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))), u)
let mee_of_str s =
  let u =
    Ast.ExApp
      (_loc,
        (Ast.ExApp
           (_loc,
             (Ast.ExId
                (_loc,
                  (Ast.IdAcc
                     (_loc, (Ast.IdUid (_loc, "Ast")),
                       (Ast.IdUid (_loc, "IdUid")))))),
             (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))),
        (Ast.ExStr (_loc, s))) in
  Ast.ExApp
    (_loc,
      (Ast.ExApp
         (_loc,
           (Ast.ExId
              (_loc,
                (Ast.IdAcc
                   (_loc, (Ast.IdUid (_loc, "Ast")),
                     (Ast.IdUid (_loc, "ExId")))))),
           (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))), u)
let mee_record_left str =
  let u =
    Ast.ExApp
      (_loc,
        (Ast.ExApp
           (_loc,
             (Ast.ExId
                (_loc,
                  (Ast.IdAcc
                     (_loc, (Ast.IdUid (_loc, "Ast")),
                       (Ast.IdUid (_loc, "IdLid")))))),
             (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))),
        (Ast.ExStr (_loc, str))) in
  Ast.ExApp
    (_loc,
      (Ast.ExApp
         (_loc,
           (Ast.ExId
              (_loc,
                (Ast.IdAcc
                   (_loc, (Ast.IdUid (_loc, "Ast")),
                     (Ast.IdUid (_loc, "RbEq")))))),
           (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))), u)
let mep_record_left str =
  let u =
    Ast.ExApp
      (_loc,
        (Ast.ExApp
           (_loc,
             (Ast.ExId
                (_loc,
                  (Ast.IdAcc
                     (_loc, (Ast.IdUid (_loc, "Ast")),
                       (Ast.IdUid (_loc, "IdLid")))))),
             (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))),
        (Ast.ExStr (_loc, str))) in
  Ast.ExApp
    (_loc,
      (Ast.ExApp
         (_loc,
           (Ast.ExId
              (_loc,
                (Ast.IdAcc
                   (_loc, (Ast.IdUid (_loc, "Ast")),
                     (Ast.IdUid (_loc, "PaEq")))))),
           (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))), u)
let mee_comma x y =
  Ast.ExApp
    (_loc,
      (Ast.ExApp
         (_loc,
           (Ast.ExApp
              (_loc,
                (Ast.ExId
                   (_loc,
                     (Ast.IdAcc
                        (_loc, (Ast.IdUid (_loc, "Ast")),
                          (Ast.IdUid (_loc, "ExCom")))))),
                (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))), x)), y)
let mee_app x y =
  Ast.ExApp
    (_loc,
      (Ast.ExApp
         (_loc,
           (Ast.ExApp
              (_loc,
                (Ast.ExId
                   (_loc,
                     (Ast.IdAcc
                        (_loc, (Ast.IdUid (_loc, "Ast")),
                          (Ast.IdUid (_loc, "ExApp")))))),
                (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))), x)), y)
let mk_tuple_ee =
  function
  | [] -> invalid_arg "mktupee arity is zero "
  | x::[] -> x
  | xs ->
      Ast.ExApp
        (_loc,
          (Ast.ExApp
             (_loc,
               (Ast.ExId
                  (_loc,
                    (Ast.IdAcc
                       (_loc, (Ast.IdUid (_loc, "Ast")),
                         (Ast.IdUid (_loc, "ExTup")))))),
               (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))),
          (List.reduce_right mee_comma xs))
let mee_record_col label expr =
  Ast.ExApp (_loc, (mee_record_left label), expr)
let mep_record_col label expr =
  Ast.ExApp (_loc, (mep_record_left label), expr)
let mee_record_semi a b =
  Ast.ExApp
    (_loc,
      (Ast.ExApp
         (_loc,
           (Ast.ExApp
              (_loc,
                (Ast.ExId
                   (_loc,
                     (Ast.IdAcc
                        (_loc, (Ast.IdUid (_loc, "Ast")),
                          (Ast.IdUid (_loc, "RbSem")))))),
                (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))), a)), b)
let mep_record_semi a b =
  Ast.ExApp
    (_loc,
      (Ast.ExApp
         (_loc,
           (Ast.ExApp
              (_loc,
                (Ast.ExId
                   (_loc,
                     (Ast.IdAcc
                        (_loc, (Ast.IdUid (_loc, "Ast")),
                          (Ast.IdUid (_loc, "PaSem")))))),
                (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))), a)), b)
let mk_record_ee label_exprs =
  let open List in
    (label_exprs |> (map (fun (label,expr)  -> mee_record_col label expr)))
      |>
      (fun es  ->
         Ast.ExApp
           (_loc,
             (Ast.ExApp
                (_loc,
                  (Ast.ExApp
                     (_loc,
                       (Ast.ExId
                          (_loc,
                            (Ast.IdAcc
                               (_loc, (Ast.IdUid (_loc, "Ast")),
                                 (Ast.IdUid (_loc, "ExRec")))))),
                       (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))),
                  (List.reduce_right mee_record_semi es))),
             (Ast.ExApp
                (_loc,
                  (Ast.ExId
                     (_loc,
                       (Ast.IdAcc
                          (_loc, (Ast.IdUid (_loc, "Ast")),
                            (Ast.IdUid (_loc, "ExNil")))))),
                  (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc"))))))))
let mk_record_ep label_exprs =
  let open List in
    (label_exprs |> (map (fun (label,expr)  -> mep_record_col label expr)))
      |>
      (fun es  ->
         Ast.ExApp
           (_loc,
             (Ast.ExApp
                (_loc,
                  (Ast.ExId
                     (_loc,
                       (Ast.IdAcc
                          (_loc, (Ast.IdUid (_loc, "Ast")),
                            (Ast.IdUid (_loc, "PaRec")))))),
                  (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))),
             (List.reduce_right mep_record_semi es)))
let eta_expand expr number =
  let names = List.init number (fun i  -> x ~off:0 i) in
  names <+ (expr +> names)
let gen_curry_n acc ~arity  cons n =
  let args =
    List.init arity
      (fun i  -> List.init n (fun j  -> Ast.PaId (_loc, (xid ~off:i j)))) in
  let pat = Patt.of_str cons in
  List.fold_right
    (fun p  acc  ->
       Ast.ExFun (_loc, (Ast.McArr (_loc, p, (Ast.ExNil _loc), acc))))
    (List.map (fun lst  -> Patt.apply pat lst) args) acc
let currying match_cases ~arity  =
  if arity >= 2
  then
    let names = List.init arity (fun i  -> x ~off:i 0) in
    let exprs =
      List.map (fun s  -> Ast.ExId (_loc, (Ast.IdLid (_loc, s)))) names in
    names <+
      (Ast.ExMat
         (_loc, (tuple_of_list exprs), (Ast.mcOr_of_list match_cases)))
  else Ast.ExFun (_loc, (Ast.mcOr_of_list match_cases))
let unknown len =
  if len = 0
  then
    Ast.ExSnd
      (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "self")))), "unknown")
  else
    Ast.ExApp
      (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "failwith")))),
        (Ast.ExStr (_loc, "not implemented!")))