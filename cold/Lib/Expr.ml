open Ast
open LibUtil
open Basic
open FanUtil
module Ast = FanAst
let rec sep_dot_expr acc =
  function
  | `ExAcc (_loc,e1,e2) -> sep_dot_expr (sep_dot_expr acc e2) e1
  | `ExId (loc,`Uid (_,s)) as e ->
      (match acc with
       | [] -> [(loc, [], e)]
       | (loc',sl,e)::l -> ((FanLoc.merge loc loc'), (s :: sl), e) :: l)
  | `ExId (_loc,(`IdAcc (_l,_,_) as i)) ->
      sep_dot_expr acc (Ident.normalize_acc i)
  | e -> ((FanAst.loc_of_expr e), [], e) :: acc
let mksequence ?loc  =
  function
  | `ExSem (_loc,_,_)|`Ant (_loc,_) as e ->
      let _loc = match loc with | Some x -> x | None  -> _loc in
      `ExSeq (_loc, e)
  | e -> e
let mksequence' ?loc  =
  function
  | `ExSem (_loc,_,_) as e ->
      let _loc = match loc with | Some x -> x | None  -> _loc in
      `ExSeq (_loc, e)
  | e -> e
let mkassert loc =
  function
  | `ExId (_loc,`Lid (_,"false")) -> `ExAsf loc
  | e -> `ExAsr (loc, e)
let bigarray_get loc arr arg =
  let coords =
    match arg with
    | `ExTup (_loc,`ExCom (_,e1,e2))|`ExCom (_loc,e1,e2) ->
        FanAst.list_of_expr e1 (FanAst.list_of_expr e2 [])
    | _ -> [arg] in
  match coords with
  | [] -> failwith "bigarray_get null list"
  | c1::[] ->
      `ExApp
        (loc,
          (`ExApp
             (loc,
               (`ExId
                  (loc,
                    (`IdAcc
                       (loc, (`Uid (loc, "Bigarray")),
                         (`IdAcc
                            (loc, (`Uid (loc, "Array1")),
                              (`Lid (loc, "get")))))))), arr)), c1)
  | c1::c2::[] ->
      `ExApp
        (loc,
          (`ExApp
             (loc,
               (`ExApp
                  (loc,
                    (`ExId
                       (loc,
                         (`IdAcc
                            (loc, (`Uid (loc, "Bigarray")),
                              (`IdAcc
                                 (loc, (`Uid (loc, "Array2")),
                                   (`Lid (loc, "get")))))))), arr)), c1)),
          c2)
  | c1::c2::c3::[] ->
      `ExApp
        (loc,
          (`ExApp
             (loc,
               (`ExApp
                  (loc,
                    (`ExApp
                       (loc,
                         (`ExId
                            (loc,
                              (`IdAcc
                                 (loc, (`Uid (loc, "Bigarray")),
                                   (`IdAcc
                                      (loc, (`Uid (loc, "Array3")),
                                        (`Lid (loc, "get")))))))), arr)), c1)),
               c2)), c3)
  | c1::c2::c3::coords ->
      `ExApp
        (loc,
          (`ExApp
             (loc,
               (`ExId
                  (loc,
                    (`IdAcc
                       (loc, (`Uid (loc, "Bigarray")),
                         (`IdAcc
                            (loc, (`Uid (loc, "Genarray")),
                              (`Lid (loc, "get")))))))), arr)),
          (`ExArr
             (loc,
               (`ExSem
                  (loc, c1,
                    (`ExSem
                       (loc, c2,
                         (`ExSem (loc, c3, (FanAst.exSem_of_list coords))))))))))
let bigarray_set loc var newval =
  match var with
  | `ExApp
      (_loc,`ExApp
              (_,`ExId
                   (_,`IdAcc
                        (_,`Uid (_,"Bigarray"),`IdAcc
                                                 (_,`Uid (_,"Array1"),
                                                  `Lid (_,"get")))),arr),c1)
      ->
      Some
        (`ExApp
           (loc,
             (`ExApp
                (loc,
                  (`ExApp
                     (loc,
                       (`ExId
                          (loc,
                            (`IdAcc
                               (loc, (`Uid (loc, "Bigarray")),
                                 (`IdAcc
                                    (loc, (`Uid (loc, "Array1")),
                                      (`Lid (loc, "set")))))))), arr)), c1)),
             newval))
  | `ExApp
      (_loc,`ExApp
              (_,`ExApp
                   (_,`ExId
                        (_,`IdAcc
                             (_,`Uid (_,"Bigarray"),`IdAcc
                                                      (_,`Uid (_,"Array2"),
                                                       `Lid (_,"get")))),arr),c1),c2)
      ->
      Some
        (`ExApp
           (loc,
             (`ExApp
                (loc,
                  (`ExApp
                     (loc,
                       (`ExApp
                          (loc,
                            (`ExId
                               (loc,
                                 (`IdAcc
                                    (loc, (`Uid (loc, "Bigarray")),
                                      (`IdAcc
                                         (loc, (`Uid (loc, "Array2")),
                                           (`Lid (loc, "set")))))))), arr)),
                       c1)), c2)), newval))
  | `ExApp
      (_loc,`ExApp
              (_,`ExApp
                   (_,`ExApp
                        (_,`ExId
                             (_,`IdAcc
                                  (_,`Uid (_,"Bigarray"),`IdAcc
                                                           (_,`Uid
                                                                (_,"Array3"),
                                                            `Lid (_,"get")))),arr),c1),c2),c3)
      ->
      Some
        (`ExAss
           (loc,
             (`ExAcc
                (loc,
                  (`ExApp
                     (loc,
                       (`ExApp
                          (loc,
                            (`ExApp
                               (loc,
                                 (`ExApp
                                    (loc,
                                      (`ExId
                                         (loc,
                                           (`IdAcc
                                              (loc, (`Uid (loc, "Bigarray")),
                                                (`IdAcc
                                                   (loc,
                                                     (`Uid (loc, "Array3")),
                                                     (`Lid (loc, "get")))))))),
                                      arr)), c1)), c2)), c3)),
                  (`ExId (loc, (`Lid (loc, "contents")))))), newval))
  | `ExApp
      (_loc,`ExApp
              (_,`ExId
                   (_,`IdAcc
                        (_,`Uid (_,"Bigarray"),`IdAcc
                                                 (_,`Uid (_,"Genarray"),
                                                  `Lid (_,"get")))),arr),
       `ExArr (_,coords))
      ->
      Some
        (`ExApp
           (loc,
             (`ExApp
                (loc,
                  (`ExApp
                     (loc,
                       (`ExId
                          (loc,
                            (`IdAcc
                               (loc, (`Uid (loc, "Bigarray")),
                                 (`IdAcc
                                    (loc, (`Uid (loc, "Genarray")),
                                      (`Lid (loc, "set")))))))), arr)),
                  (`ExArr (loc, coords)))), newval))
  | _ -> None
let rec pattern_eq_expression p e =
  match (p, e) with
  | (`PaId (_loc,`Lid (_,a)),`ExId (_,`Lid (_,b)))|(`PaId (_loc,`Uid (_,a)),
                                                    `ExId (_,`Uid (_,b)))
      -> a = b
  | (`PaApp (_loc,p1,p2),`ExApp (_,e1,e2)) ->
      (pattern_eq_expression p1 e1) && (pattern_eq_expression p2 e2)
  | _ -> false
let map loc p e l =
  match (p, e) with
  | (`PaId (_loc,`Lid (_,x)),`ExId (_,`Lid (_,y))) when x = y -> l
  | _ ->
      if FanAst.is_irrefut_patt p
      then
        `ExApp
          (loc,
            (`ExApp
               (loc,
                 (`ExId
                    (loc,
                      (`IdAcc
                         (loc, (`Uid (loc, "List")), (`Lid (loc, "map")))))),
                 (`ExFun (loc, (`McArr (loc, p, (`ExNil loc), e)))))), l)
      else
        `ExApp
          (loc,
            (`ExApp
               (loc,
                 (`ExApp
                    (loc,
                      (`ExId
                         (loc,
                           (`IdAcc
                              (loc, (`Uid (loc, "List")),
                                (`Lid (loc, "fold_right")))))),
                      (`ExFun
                         (loc,
                           (`McOr
                              (loc,
                                (`McArr
                                   (loc, p,
                                     (`ExId (loc, (`Lid (loc, "true")))),
                                     (`ExApp
                                        (loc,
                                          (`ExFun
                                             (loc,
                                               (`McArr
                                                  (loc,
                                                    (`PaId
                                                       (loc,
                                                         (`Lid (loc, "x")))),
                                                    (`ExNil loc),
                                                    (`ExFun
                                                       (loc,
                                                         (`McArr
                                                            (loc,
                                                              (`PaId
                                                                 (loc,
                                                                   (`Lid
                                                                    (loc,
                                                                    "xs")))),
                                                              (`ExNil loc),
                                                              (`ExApp
                                                                 (loc,
                                                                   (`ExApp
                                                                    (loc,
                                                                    (`ExId
                                                                    (loc,
                                                                    (`Uid
                                                                    (loc,
                                                                    "::")))),
                                                                    (`ExId
                                                                    (loc,
                                                                    (`Lid
                                                                    (loc,
                                                                    "x")))))),
                                                                   (`ExId
                                                                    (loc,
                                                                    (`Lid
                                                                    (loc,
                                                                    "xs")))))))))))))),
                                          e)))),
                                (`McArr
                                   (loc, (`PaAny loc), (`ExNil loc),
                                     (`ExFun
                                        (loc,
                                          (`McArr
                                             (loc,
                                               (`PaId
                                                  (loc, (`Lid (loc, "l")))),
                                               (`ExNil loc),
                                               (`ExId
                                                  (loc, (`Lid (loc, "l")))))))))))))))),
                 l)), (`ExId (loc, (`Uid (loc, "[]")))))
let filter loc p b l =
  if FanAst.is_irrefut_patt p
  then
    `ExApp
      (loc,
        (`ExApp
           (loc,
             (`ExId
                (loc,
                  (`IdAcc (loc, (`Uid (loc, "List")), (`Lid (loc, "filter")))))),
             (`ExFun (loc, (`McArr (loc, p, (`ExNil loc), b)))))), l)
  else
    `ExApp
      (loc,
        (`ExApp
           (loc,
             (`ExId
                (loc,
                  (`IdAcc (loc, (`Uid (loc, "List")), (`Lid (loc, "filter")))))),
             (`ExFun
                (loc,
                  (`McOr
                     (loc,
                       (`McArr
                          (loc, p, (`ExId (loc, (`Lid (loc, "true")))), b)),
                       (`McArr
                          (loc, (`PaAny loc), (`ExNil loc),
                            (`ExId (loc, (`Lid (loc, "false")))))))))))), l)
let concat _loc l =
  `ExApp
    (_loc,
      (`ExId
         (_loc,
           (`IdAcc (_loc, (`Uid (_loc, "List")), (`Lid (_loc, "concat")))))),
      l)
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
    | `ExApp (_loc,e1,e2) -> `PaApp (loc, (loop e1), (loop e2))
    | `ExNil _loc -> `PaNil loc
    | `ExId (_loc,`Lid (_,x)) ->
        (try List.assoc x env
         with | Not_found  -> `PaId (loc, (`Lid (loc, x))))
    | `ExId (_loc,`Uid (_,x)) ->
        (try List.assoc x env
         with | Not_found  -> `PaId (loc, (`Uid (loc, x))))
    | `ExInt (_loc,x) -> `PaInt (loc, x)
    | `ExStr (_loc,s) -> `PaStr (loc, s)
    | `ExTup (_loc,x) -> `PaTup (loc, (loop x))
    | `ExCom (_loc,x1,x2) -> `PaCom (loc, (loop x1), (loop x2))
    | `ExRec (_loc,bi,`ExNil _) ->
        let rec substbi =
          function
          | `RbSem (_loc,b1,b2) -> `PaSem (loc, (substbi b1), (substbi b2))
          | `RbEq (_loc,i,e) -> `PaEq (loc, i, (loop e))
          | _ -> bad_patt _loc in
        `PaRec (loc, (substbi bi))
    | _ -> bad_patt loc in
  loop
class subst loc env =
  object 
    inherit  (FanAst.reloc loc) as super
    method! expr =
      function
      | `ExId (_loc,`Lid (_,x))|`ExId (_loc,`Uid (_,x)) as e ->
          (try List.assoc x env with | Not_found  -> super#expr e)
      | `ExApp (_loc,`ExId (_,`Uid (_,"LOCATION_OF")),`ExId (_,`Lid (_,x)))|
          `ExApp (_loc,`ExId (_,`Uid (_,"LOCATION_OF")),`ExId (_,`Uid (_,x)))
          as e ->
          (try
             let loc = FanAst.loc_of_expr (List.assoc x env) in
             let (a,b,c,d,e,f,g,h) = FanLoc.to_tuple loc in
             `ExApp
               (_loc,
                 (`ExId
                    (_loc,
                      (`IdAcc
                         (_loc, (`Uid (_loc, "FanLoc")),
                           (`Lid (_loc, "of_tuple")))))),
                 (`ExTup
                    (_loc,
                      (`ExCom
                         (_loc,
                           (`ExStr (_loc, (FanAst.safe_string_escaped a))),
                           (`ExCom
                              (_loc,
                                (`ExCom
                                   (_loc,
                                     (`ExCom
                                        (_loc,
                                          (`ExCom
                                             (_loc,
                                               (`ExCom
                                                  (_loc,
                                                    (`ExCom
                                                       (_loc,
                                                         (`ExInt
                                                            (_loc,
                                                              (string_of_int
                                                                 b))),
                                                         (`ExInt
                                                            (_loc,
                                                              (string_of_int
                                                                 c))))),
                                                    (`ExInt
                                                       (_loc,
                                                         (string_of_int d))))),
                                               (`ExInt
                                                  (_loc, (string_of_int e))))),
                                          (`ExInt (_loc, (string_of_int f))))),
                                     (`ExInt (_loc, (string_of_int g))))),
                                (if h
                                 then `ExId (_loc, (`Lid (_loc, "true")))
                                 else `ExId (_loc, (`Lid (_loc, "false")))))))))))
           with | Not_found  -> super#expr e)
      | e -> super#expr e
    method! patt =
      function
      | `PaId (_loc,`Lid (_,x))|`PaId (_loc,`Uid (_,x)) as p ->
          (try substp loc [] (List.assoc x env)
           with | Not_found  -> super#patt p)
      | p -> super#patt p
  end
class type antiquot_filter
  =
  object 
    inherit FanAst.map
    method get_captured_variables : (expr* expr) list
    method clear_captured_variables : unit
  end
let capture_antiquot: antiquot_filter =
  object 
    inherit  FanAst.map as super
    val mutable constraints = []
    method! patt =
      function
      | `Ant (_loc,s)|`PaStr (_loc,s) as p when is_antiquot s ->
          (match view_antiquot s with
           | Some (_name,code) ->
               let cons = `ExId (_loc, (`Lid (_loc, code))) in
               let code' = "__fan__" ^ code in
               let cons' = `ExId (_loc, (`Lid (_loc, code'))) in
               let () = constraints <- (cons, cons') :: constraints in
               `PaId (_loc, (`Lid (_loc, code')))
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
let fun_args _loc args body =
  if args = []
  then
    `ExFun
      (_loc,
        (`McArr
           (_loc, (`PaId (_loc, (`Uid (_loc, "()")))), (`ExNil _loc), body)))
  else
    List.fold_right
      (fun arg  body  ->
         `ExFun (_loc, (`McArr (_loc, arg, (`ExNil _loc), body)))) args body
let _loc = FanLoc.ghost
let app a b = `ExApp (_loc, a, b)
let comma a b = `ExCom (_loc, a, b)
let (<$) = app
let rec apply acc = function | [] -> acc | x::xs -> apply (app acc x) xs
let sem a b =
  let _loc = FanLoc.merge (FanAst.loc_of_expr a) (FanAst.loc_of_expr b) in
  `ExSem (_loc, a, b)
let list_of_app ty =
  let rec loop t acc =
    match t with
    | `ExApp (_loc,t1,t2) -> loop t1 (t2 :: acc)
    | `ExNil _loc -> acc
    | i -> i :: acc in
  loop ty []
let list_of_com ty =
  let rec loop t acc =
    match t with
    | `ExCom (_loc,t1,t2) -> t1 :: (loop t2 acc)
    | `ExNil _loc -> acc
    | i -> i :: acc in
  loop ty []
let list_of_sem ty =
  let rec loop t acc =
    match t with
    | `ExSem (_loc,t1,t2) -> t1 :: (loop t2 acc)
    | `ExNil _loc -> acc
    | i -> i :: acc in
  loop ty []
let rec view_app acc =
  function | `ExApp (_loc,f,a) -> view_app (a :: acc) f | f -> (f, acc)
let app_of_list = function | [] -> `ExNil _loc | l -> List.reduce_left app l
let com_of_list =
  function | [] -> `ExNil _loc | l -> List.reduce_right comma l
let sem_of_list = function | [] -> `ExNil _loc | l -> List.reduce_right sem l
let tuple_of_list =
  function
  | [] -> invalid_arg "tuple_of_list while list is empty"
  | x::[] -> x
  | xs -> `ExTup (_loc, (com_of_list xs))
let mklist loc =
  let rec loop top =
    function
    | [] -> `ExId (_loc, (`Uid (_loc, "[]")))
    | e1::el ->
        let _loc =
          if top then loc else FanLoc.merge (FanAst.loc_of_expr e1) loc in
        `ExApp
          (_loc, (`ExApp (_loc, (`ExId (_loc, (`Uid (_loc, "::")))), e1)),
            (loop false el)) in
  loop true
let rec apply accu =
  function
  | [] -> accu
  | x::xs ->
      let _loc = FanAst.loc_of_expr x in apply (`ExApp (_loc, accu, x)) xs
let mkarray loc arr =
  let rec loop top =
    function
    | [] -> `ExId (_loc, (`Uid (_loc, "[]")))
    | e1::el ->
        let _loc =
          if top then loc else FanLoc.merge (FanAst.loc_of_expr e1) loc in
        `ExArr (_loc, (`ExSem (_loc, e1, (loop false el)))) in
  let items = arr |> Array.to_list in loop true items
let of_str s =
  let len = String.length s in
  if len = 0
  then invalid_arg "[expr|patt]_of_str len=0"
  else
    (match s.[0] with
     | '`' -> `ExVrn (_loc, (String.sub s 1 (len - 1)))
     | x when Char.is_uppercase x -> `ExId (_loc, (`Uid (_loc, s)))
     | _ -> `ExId (_loc, (`Lid (_loc, s))))
let of_ident_number cons n =
  apply (`ExId (_loc, cons)) (List.init n (fun i  -> `ExId (_loc, (xid i))))
let (+>) f names =
  apply f (List.map (fun lid  -> `ExId (_loc, (`Lid (_loc, lid)))) names)
let gen_tuple_first ~number  ~off  =
  match number with
  | 1 -> `ExId (_loc, (xid ~off 0))
  | n when n > 1 ->
      let lst =
        zfold_left ~start:1 ~until:(number - 1)
          ~acc:(`ExId (_loc, (xid ~off 0)))
          (fun acc  i  -> comma acc (`ExId (_loc, (xid ~off i)))) in
      `ExTup (_loc, lst)
  | _ -> invalid_arg "n < 1 in gen_tuple_first"
let gen_tuple_second ~number  ~off  =
  match number with
  | 1 -> `ExId (_loc, (xid ~off:0 off))
  | n when n > 1 ->
      let lst =
        zfold_left ~start:1 ~until:(number - 1)
          ~acc:(`ExId (_loc, (xid ~off:0 off)))
          (fun acc  i  -> comma acc (`ExId (_loc, (xid ~off:i off)))) in
      `ExTup (_loc, lst)
  | _ -> invalid_arg "n < 1 in gen_tuple_first "
let tuple_of_number ast n =
  let res =
    zfold_left ~start:1 ~until:(n - 1) ~acc:ast
      (fun acc  _  -> comma acc ast) in
  if n > 1 then `ExTup (_loc, res) else res
let tuple_of_list lst =
  let len = List.length lst in
  match len with
  | 1 -> List.hd lst
  | n when n > 1 -> `ExTup (_loc, (List.reduce_left comma lst))
  | _ -> invalid_arg "tuple_of_list n < 1"
let of_vstr_number name i =
  let items = List.init i (fun i  -> `ExId (_loc, (xid i))) in
  if items = []
  then `ExVrn (_loc, name)
  else
    (let item = items |> tuple_of_list in
     `ExApp (_loc, (`ExVrn (_loc, name)), item))
let gen_tuple_n ?(cons_transform= fun x  -> x)  ~arity  cons n =
  let args =
    List.init arity
      (fun i  -> List.init n (fun j  -> `ExId (_loc, (xid ~off:i j)))) in
  let pat = of_str (cons_transform cons) in
  (List.map (fun lst  -> apply pat lst) args) |> tuple_of_list
let tuple _loc =
  function
  | [] -> `ExId (_loc, (`Uid (_loc, "()")))
  | p::[] -> p
  | e::es -> `ExTup (_loc, (`ExCom (_loc, e, (FanAst.exCom_of_list es))))
let mkumin loc prefix arg =
  match arg with
  | `ExInt (_loc,n) -> `ExInt (loc, (String.neg n))
  | `ExInt32 (_loc,n) -> `ExInt32 (loc, (String.neg n))
  | `ExInt64 (_loc,n) -> `ExInt64 (loc, (String.neg n))
  | `ExNativeInt (_loc,n) -> `ExNativeInt (loc, (String.neg n))
  | `ExFlo (_loc,n) -> `ExFlo (loc, (String.neg n))
  | _ -> `ExApp (loc, (`ExId (loc, (`Lid (loc, ("~" ^ prefix))))), arg)
let mk_assert =
  function
  | `ExId (_loc,`Lid (_,"false")) -> `ExAsf _loc
  | e -> `ExAsr (_loc, e)
let mk_record label_exprs =
  let rec_bindings =
    List.map (fun (label,expr)  -> `RbEq (_loc, (`Lid (_loc, label)), expr))
      label_exprs in
  `ExRec (_loc, (FanAst.rbSem_of_list rec_bindings), (`ExNil _loc))
let failure =
  `ExApp
    (_loc, (`ExId (_loc, (`Lid (_loc, "raise")))),
      (`ExApp
         (_loc, (`ExId (_loc, (`Uid (_loc, "Failure")))),
           (`ExStr (_loc, "metafilter: Cannot handle that kind of types ")))))
let (<+) names acc =
  List.fold_right
    (fun name  acc  ->
       `ExFun
         (_loc,
           (`McArr
              (_loc, (`PaId (_loc, (`Lid (_loc, name)))), (`ExNil _loc), acc))))
    names acc
let (<+<) patts acc =
  List.fold_right
    (fun p  acc  -> `ExFun (_loc, (`McArr (_loc, p, (`ExNil _loc), acc))))
    patts acc
let mep_comma x y =
  `ExApp
    (_loc, (`ExVrn (_loc, "PaCom")),
      (`ExTup
         (_loc,
           (`ExCom
              (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                (`ExCom (_loc, x, y)))))))
let mvep_comma x y =
  `ExApp
    (_loc, (`ExVrn (_loc, "PaCom")),
      (`ExTup
         (_loc,
           (`ExCom
              (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                (`ExCom (_loc, x, y)))))))
let mee_comma x y =
  `ExApp
    (_loc, (`ExVrn (_loc, "ExCom")),
      (`ExTup
         (_loc,
           (`ExCom
              (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                (`ExCom (_loc, x, y)))))))
let mvee_comma x y =
  `ExApp
    (_loc, (`ExVrn (_loc, "ExCom")),
      (`ExTup
         (_loc,
           (`ExCom
              (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                (`ExCom (_loc, x, y)))))))
let mee_app x y =
  `ExApp
    (_loc, (`ExVrn (_loc, "ExApp")),
      (`ExTup
         (_loc,
           (`ExCom
              (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                (`ExCom (_loc, x, y)))))))
let vee_app x y =
  `ExApp
    (_loc, (`ExVrn (_loc, "ExApp")),
      (`ExTup
         (_loc,
           (`ExCom
              (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                (`ExCom (_loc, x, y)))))))
let mep_app x y =
  `ExApp
    (_loc, (`ExVrn (_loc, "PaApp")),
      (`ExTup
         (_loc,
           (`ExCom
              (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                (`ExCom (_loc, x, y)))))))
let vep_app x y =
  `ExApp
    (_loc, (`ExVrn (_loc, "PaApp")),
      (`ExTup
         (_loc,
           (`ExCom
              (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                (`ExCom (_loc, x, y)))))))
let mep_of_str s =
  let len = String.length s in
  if (s.[0]) = '`'
  then
    let s = String.sub s 1 (len - 1) in
    `ExApp
      (_loc, (`ExVrn (_loc, "PaVrn")),
        (`ExTup
           (_loc,
             (`ExCom
                (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                  (`ExStr (_loc, s)))))))
  else
    (let u =
       `ExApp
         (_loc, (`ExVrn (_loc, "Uid")),
           (`ExTup
              (_loc,
                (`ExCom
                   (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                     (`ExStr (_loc, s))))))) in
     `ExApp
       (_loc, (`ExVrn (_loc, "PaId")),
         (`ExTup
            (_loc, (`ExCom (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))), u))))))
let mee_of_str s =
  let len = String.length s in
  if (s.[0]) = '`'
  then
    let s = String.sub s 1 (len - 1) in
    `ExApp
      (_loc, (`ExVrn (_loc, "ExVrn")),
        (`ExTup
           (_loc,
             (`ExCom
                (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                  (`ExStr (_loc, s)))))))
  else
    (let u =
       `ExApp
         (_loc, (`ExVrn (_loc, "Uid")),
           (`ExTup
              (_loc,
                (`ExCom
                   (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                     (`ExStr (_loc, s))))))) in
     `ExApp
       (_loc, (`ExVrn (_loc, "ExId")),
         (`ExTup
            (_loc, (`ExCom (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))), u))))))
let vee_of_str s =
  `ExApp
    (_loc, (`ExVrn (_loc, "ExVrn")),
      (`ExTup
         (_loc,
           (`ExCom
              (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                (`ExStr (_loc, s)))))))
let vep_of_str s =
  `ExApp
    (_loc, (`ExVrn (_loc, "PaVrn")),
      (`ExTup
         (_loc,
           (`ExCom
              (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                (`ExStr (_loc, s)))))))
let meee_of_str s =
  let u =
    `ExApp
      (_loc, (`ExVrn (_loc, "ExApp")),
        (`ExTup
           (_loc,
             (`ExCom
                (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                  (`ExCom
                     (_loc,
                       (`ExApp
                          (_loc, (`ExVrn (_loc, "ExVrn")),
                            (`ExTup
                               (_loc,
                                 (`ExCom
                                    (_loc,
                                      (`ExId (_loc, (`Lid (_loc, "_loc")))),
                                      (`ExStr (_loc, "Uid")))))))),
                       (`ExApp
                          (_loc, (`ExVrn (_loc, "ExTup")),
                            (`ExTup
                               (_loc,
                                 (`ExCom
                                    (_loc,
                                      (`ExId (_loc, (`Lid (_loc, "_loc")))),
                                      (`ExApp
                                         (_loc, (`ExVrn (_loc, "ExCom")),
                                           (`ExTup
                                              (_loc,
                                                (`ExCom
                                                   (_loc,
                                                     (`ExId
                                                        (_loc,
                                                          (`Lid
                                                             (_loc, "_loc")))),
                                                     (`ExCom
                                                        (_loc,
                                                          (`ExApp
                                                             (_loc,
                                                               (`ExVrn
                                                                  (_loc,
                                                                    "ExId")),
                                                               (`ExTup
                                                                  (_loc,
                                                                    (
                                                                    `ExCom
                                                                    (_loc,
                                                                    (`ExId
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "_loc")))),
                                                                    (`ExApp
                                                                    (_loc,
                                                                    (`ExVrn
                                                                    (_loc,
                                                                    "Lid")),
                                                                    (`ExTup
                                                                    (_loc,
                                                                    (`ExCom
                                                                    (_loc,
                                                                    (`ExId
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "_loc")))),
                                                                    (`ExStr
                                                                    (_loc,
                                                                    "_loc")))))))))))))),
                                                          (`ExApp
                                                             (_loc,
                                                               (`ExVrn
                                                                  (_loc,
                                                                    "ExStr")),
                                                               (`ExTup
                                                                  (_loc,
                                                                    (
                                                                    `ExCom
                                                                    (_loc,
                                                                    (`ExId
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "_loc")))),
                                                                    (`ExStr
                                                                    (_loc, s))))))))))))))))))))))))))))) in
  `ExApp
    (_loc, (`ExVrn (_loc, "ExApp")),
      (`ExTup
         (_loc,
           (`ExCom
              (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                (`ExCom
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExVrn")),
                          (`ExTup
                             (_loc,
                               (`ExCom
                                  (_loc,
                                    (`ExId (_loc, (`Lid (_loc, "_loc")))),
                                    (`ExStr (_loc, "ExId")))))))),
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExTup")),
                          (`ExTup
                             (_loc,
                               (`ExCom
                                  (_loc,
                                    (`ExId (_loc, (`Lid (_loc, "_loc")))),
                                    (`ExApp
                                       (_loc, (`ExVrn (_loc, "ExCom")),
                                         (`ExTup
                                            (_loc,
                                              (`ExCom
                                                 (_loc,
                                                   (`ExId
                                                      (_loc,
                                                        (`Lid (_loc, "_loc")))),
                                                   (`ExCom
                                                      (_loc,
                                                        (`ExApp
                                                           (_loc,
                                                             (`ExVrn
                                                                (_loc,
                                                                  "ExId")),
                                                             (`ExTup
                                                                (_loc,
                                                                  (`ExCom
                                                                    (_loc,
                                                                    (`ExId
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "_loc")))),
                                                                    (`ExApp
                                                                    (_loc,
                                                                    (`ExVrn
                                                                    (_loc,
                                                                    "Lid")),
                                                                    (`ExTup
                                                                    (_loc,
                                                                    (`ExCom
                                                                    (_loc,
                                                                    (`ExId
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "_loc")))),
                                                                    (`ExStr
                                                                    (_loc,
                                                                    "_loc")))))))))))))),
                                                        u)))))))))))))))))))))
let mk_tuple_ee =
  function
  | [] -> invalid_arg "mktupee arity is zero "
  | x::[] -> x
  | xs ->
      `ExApp
        (_loc, (`ExVrn (_loc, "ExTup")),
          (`ExTup
             (_loc,
               (`ExCom
                  (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                    (List.reduce_right mee_comma xs))))))
let mk_tuple_vee =
  function
  | [] -> invalid_arg "mktupee arity is zero "
  | x::[] -> x
  | xs ->
      `ExApp
        (_loc, (`ExVrn (_loc, "ExTup")),
          (`ExTup
             (_loc,
               (`ExCom
                  (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                    (List.reduce_right mvee_comma xs))))))
let mk_tuple_ep =
  function
  | [] -> assert false
  | x::[] -> x
  | xs ->
      `ExApp
        (_loc, (`ExVrn (_loc, "PaTup")),
          (`ExTup
             (_loc,
               (`ExCom
                  (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                    (List.reduce_right mep_comma xs))))))
let mk_tuple_vep =
  function
  | [] -> assert false
  | x::[] -> x
  | xs ->
      `ExApp
        (_loc, (`ExVrn (_loc, "PaTup")),
          (`ExTup
             (_loc,
               (`ExCom
                  (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                    (List.reduce_right mvep_comma xs))))))
let mee_record_col label expr =
  `ExApp
    (_loc, (`ExVrn (_loc, "RbEq")),
      (`ExTup
         (_loc,
           (`ExCom
              (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                (`ExCom
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "Lid")),
                          (`ExTup
                             (_loc,
                               (`ExCom
                                  (_loc,
                                    (`ExId (_loc, (`Lid (_loc, "_loc")))),
                                    (`ExStr (_loc, label)))))))), expr)))))))
let mep_record_col label expr =
  `ExApp
    (_loc, (`ExVrn (_loc, "PaEq")),
      (`ExTup
         (_loc,
           (`ExCom
              (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                (`ExCom
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "Lid")),
                          (`ExTup
                             (_loc,
                               (`ExCom
                                  (_loc,
                                    (`ExId (_loc, (`Lid (_loc, "_loc")))),
                                    (`ExStr (_loc, label)))))))), expr)))))))
let mee_record_semi a b =
  `ExApp
    (_loc, (`ExVrn (_loc, "RbSem")),
      (`ExTup
         (_loc,
           (`ExCom
              (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                (`ExCom (_loc, a, b)))))))
let mep_record_semi a b =
  `ExApp
    (_loc, (`ExVrn (_loc, "PaSem")),
      (`ExTup
         (_loc,
           (`ExCom
              (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                (`ExCom (_loc, a, b)))))))
let mk_record_ee label_exprs =
  (label_exprs |> (List.map (fun (label,expr)  -> mee_record_col label expr)))
    |>
    (fun es  ->
       `ExApp
         (_loc, (`ExVrn (_loc, "ExRec")),
           (`ExTup
              (_loc,
                (`ExCom
                   (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                     (`ExCom
                        (_loc, (List.reduce_right mee_record_semi es),
                          (`ExApp
                             (_loc, (`ExVrn (_loc, "ExNil")),
                               (`ExId (_loc, (`Lid (_loc, "_loc"))))))))))))))
let mk_record_ep label_exprs =
  let open List in
    (label_exprs |> (map (fun (label,expr)  -> mep_record_col label expr)))
      |>
      (fun es  ->
         `ExApp
           (_loc, (`ExVrn (_loc, "PaRec")),
             (`ExTup
                (_loc,
                  (`ExCom
                     (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))),
                       (List.reduce_right mep_record_semi es)))))))
let eta_expand expr number =
  let names = List.init number (fun i  -> x ~off:0 i) in
  names <+ (expr +> names)
let gen_curry_n acc ~arity  cons n =
  let args =
    List.init arity
      (fun i  -> List.init n (fun j  -> `PaId (_loc, (xid ~off:i j)))) in
  let pat = Patt.of_str cons in
  List.fold_right
    (fun p  acc  -> `ExFun (_loc, (`McArr (_loc, p, (`ExNil _loc), acc))))
    (List.map (fun lst  -> Patt.apply pat lst) args) acc
let currying match_cases ~arity  =
  if arity >= 2
  then
    let names = List.init arity (fun i  -> x ~off:i 0) in
    let exprs = List.map (fun s  -> `ExId (_loc, (`Lid (_loc, s)))) names in
    names <+
      (`ExMat
         (_loc, (tuple_of_list exprs), (FanAst.mcOr_of_list match_cases)))
  else `ExFun (_loc, (FanAst.mcOr_of_list match_cases))
let unknown len =
  if len = 0
  then `ExSnd (_loc, (`ExId (_loc, (`Lid (_loc, "self")))), "unknown")
  else
    `ExApp
      (_loc, (`ExId (_loc, (`Lid (_loc, "failwith")))),
        (`ExStr (_loc, "not implemented!")))