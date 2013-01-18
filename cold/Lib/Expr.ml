open Ast
open LibUtil
open Basic
open FanUtil
module Ast = FanAst
let rec sep_dot_expr acc =
  function
  | `ExAcc (_loc,e1,e2) -> sep_dot_expr (sep_dot_expr acc e2) e1
  | `Id (loc,`Uid (_,s)) as e ->
      (match acc with
       | [] -> [(loc, [], e)]
       | (loc',sl,e)::l -> ((FanLoc.merge loc loc'), (s :: sl), e) :: l)
  | `Id (_loc,(`IdAcc (_l,_,_) as i)) ->
      sep_dot_expr acc (Ident.normalize_acc i)
  | e -> ((FanAst.loc_of_expr e), [], e) :: acc
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
let mkassert loc =
  function | `Id (_loc,`Lid (_,"false")) -> `ExAsf loc | e -> `ExAsr (loc, e)
let bigarray_get loc arr arg =
  let coords =
    match arg with
    | `Tup (_loc,`Com (_,e1,e2))|`Com (_loc,e1,e2) ->
        FanAst.list_of_expr e1 (FanAst.list_of_expr e2 [])
    | _ -> [arg] in
  match coords with
  | [] -> failwith "bigarray_get null list"
  | c1::[] ->
      `ExApp
        (loc,
          (`ExApp
             (loc,
               (`Id
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
                    (`Id
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
                         (`Id
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
               (`Id
                  (loc,
                    (`IdAcc
                       (loc, (`Uid (loc, "Bigarray")),
                         (`IdAcc
                            (loc, (`Uid (loc, "Genarray")),
                              (`Lid (loc, "get")))))))), arr)),
          (`Array
             (loc,
               (`Sem
                  (loc, c1,
                    (`Sem
                       (loc, c2,
                         (`Sem (loc, c3, (FanAst.exSem_of_list coords))))))))))
let bigarray_set loc var newval =
  match var with
  | `ExApp
      (_loc,`ExApp
              (_,`Id
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
                       (`Id
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
                   (_,`Id
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
                            (`Id
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
                        (_,`Id
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
                                      (`Id
                                         (loc,
                                           (`IdAcc
                                              (loc, (`Uid (loc, "Bigarray")),
                                                (`IdAcc
                                                   (loc,
                                                     (`Uid (loc, "Array3")),
                                                     (`Lid (loc, "get")))))))),
                                      arr)), c1)), c2)), c3)),
                  (`Id (loc, (`Lid (loc, "contents")))))), newval))
  | `ExApp
      (_loc,`ExApp
              (_,`Id
                   (_,`IdAcc
                        (_,`Uid (_,"Bigarray"),`IdAcc
                                                 (_,`Uid (_,"Genarray"),
                                                  `Lid (_,"get")))),arr),
       `Array (_,coords))
      ->
      Some
        (`ExApp
           (loc,
             (`ExApp
                (loc,
                  (`ExApp
                     (loc,
                       (`Id
                          (loc,
                            (`IdAcc
                               (loc, (`Uid (loc, "Bigarray")),
                                 (`IdAcc
                                    (loc, (`Uid (loc, "Genarray")),
                                      (`Lid (loc, "set")))))))), arr)),
                  (`Array (loc, coords)))), newval))
  | _ -> None
let rec pattern_eq_expression p e =
  match (p, e) with
  | (`Id (_loc,`Lid (_,a)),`Id (_,`Lid (_,b)))
    |(`Id (_loc,`Uid (_,a)),`Id (_,`Uid (_,b))) -> a = b
  | (`PaApp (_loc,p1,p2),`ExApp (_,e1,e2)) ->
      (pattern_eq_expression p1 e1) && (pattern_eq_expression p2 e2)
  | _ -> false
let map loc (p : patt) (e : expr) (l : expr) =
  (match (p, e) with
   | (`Id (_loc,`Lid (_,x)),`Id (_,`Lid (_,y))) when x = y -> l
   | _ ->
       if FanAst.is_irrefut_patt p
       then
         `ExApp
           (loc,
             (`ExApp
                (loc,
                  (`Id
                     (loc,
                       (`IdAcc
                          (loc, (`Uid (loc, "List")), (`Lid (loc, "map")))))),
                  (`Fun (loc, (`Case (loc, p, (`Nil loc), e)))))), l)
       else
         `ExApp
           (loc,
             (`ExApp
                (loc,
                  (`ExApp
                     (loc,
                       (`Id
                          (loc,
                            (`IdAcc
                               (loc, (`Uid (loc, "List")),
                                 (`Lid (loc, "fold_right")))))),
                       (`Fun
                          (loc,
                            (`Or
                               (loc,
                                 (`Case
                                    (loc, p,
                                      (`Id (loc, (`Lid (loc, "true")))),
                                      (`ExApp
                                         (loc,
                                           (`Fun
                                              (loc,
                                                (`Case
                                                   (loc,
                                                     (`Id
                                                        (loc,
                                                          (`Lid (loc, "x")))),
                                                     (`Nil loc),
                                                     (`Fun
                                                        (loc,
                                                          (`Case
                                                             (loc,
                                                               (`Id
                                                                  (loc,
                                                                    (
                                                                    `Lid
                                                                    (loc,
                                                                    "xs")))),
                                                               (`Nil loc),
                                                               (`ExApp
                                                                  (loc,
                                                                    (
                                                                    `ExApp
                                                                    (loc,
                                                                    (`Id
                                                                    (loc,
                                                                    (`Uid
                                                                    (loc,
                                                                    "::")))),
                                                                    (`Id
                                                                    (loc,
                                                                    (`Lid
                                                                    (loc,
                                                                    "x")))))),
                                                                    (
                                                                    `Id
                                                                    (loc,
                                                                    (`Lid
                                                                    (loc,
                                                                    "xs")))))))))))))),
                                           e)))),
                                 (`Case
                                    (loc, (`Any loc), (`Nil loc),
                                      (`Fun
                                         (loc,
                                           (`Case
                                              (loc,
                                                (`Id (loc, (`Lid (loc, "l")))),
                                                (`Nil loc),
                                                (`Id (loc, (`Lid (loc, "l")))))))))))))))),
                  l)), (`Id (loc, (`Uid (loc, "[]"))))) : expr )
let filter loc p b l =
  if FanAst.is_irrefut_patt p
  then
    `ExApp
      (loc,
        (`ExApp
           (loc,
             (`Id
                (loc,
                  (`IdAcc (loc, (`Uid (loc, "List")), (`Lid (loc, "filter")))))),
             (`Fun (loc, (`Case (loc, p, (`Nil loc), b)))))), l)
  else
    `ExApp
      (loc,
        (`ExApp
           (loc,
             (`Id
                (loc,
                  (`IdAcc (loc, (`Uid (loc, "List")), (`Lid (loc, "filter")))))),
             (`Fun
                (loc,
                  (`Or
                     (loc,
                       (`Case (loc, p, (`Id (loc, (`Lid (loc, "true")))), b)),
                       (`Case
                          (loc, (`Any loc), (`Nil loc),
                            (`Id (loc, (`Lid (loc, "false")))))))))))), l)
let concat _loc l =
  `ExApp
    (_loc,
      (`Id
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
    | `Nil _loc -> `Nil loc
    | `Id (_loc,`Lid (_,x)) ->
        (try List.assoc x env with | Not_found  -> `Id (loc, (`Lid (loc, x))))
    | `Id (_loc,`Uid (_,x)) ->
        (try List.assoc x env with | Not_found  -> `Id (loc, (`Uid (loc, x))))
    | `Int (_loc,x) -> `Int (loc, x)
    | `Str (_loc,s) -> `Str (loc, s)
    | `Tup (_loc,x) -> `Tup (loc, (loop x))
    | `Com (_loc,x1,x2) -> `Com (loc, (loop x1), (loop x2))
    | `Record (_loc,bi,`Nil _) ->
        let rec substbi =
          function
          | `Sem (_loc,b1,b2) -> `Sem (loc, (substbi b1), (substbi b2))
          | `RecBind (_loc,i,e) -> `PaEq (loc, i, (loop e))
          | _ -> bad_patt _loc in
        `PaRec (loc, (substbi bi))
    | _ -> bad_patt loc in
  loop
class subst loc env =
  object 
    inherit  (FanAst.reloc loc) as super
    method! expr =
      function
      | `Id (_loc,`Lid (_,x))|`Id (_loc,`Uid (_,x)) as e ->
          (try List.assoc x env with | Not_found  -> super#expr e)
      | `ExApp (_loc,`Id (_,`Uid (_,"LOCATION_OF")),`Id (_,`Lid (_,x)))
        |`ExApp (_loc,`Id (_,`Uid (_,"LOCATION_OF")),`Id (_,`Uid (_,x))) as e
          ->
          (try
             let loc = FanAst.loc_of_expr (List.assoc x env) in
             let (a,b,c,d,e,f,g,h) = FanLoc.to_tuple loc in
             `ExApp
               (_loc,
                 (`Id
                    (_loc,
                      (`IdAcc
                         (_loc, (`Uid (_loc, "FanLoc")),
                           (`Lid (_loc, "of_tuple")))))),
                 (`Tup
                    (_loc,
                      (`Com
                         (_loc,
                           (`Str (_loc, (FanAst.safe_string_escaped a))),
                           (`Com
                              (_loc,
                                (`Com
                                   (_loc,
                                     (`Com
                                        (_loc,
                                          (`Com
                                             (_loc,
                                               (`Com
                                                  (_loc,
                                                    (`Com
                                                       (_loc,
                                                         (`Int
                                                            (_loc,
                                                              (string_of_int
                                                                 b))),
                                                         (`Int
                                                            (_loc,
                                                              (string_of_int
                                                                 c))))),
                                                    (`Int
                                                       (_loc,
                                                         (string_of_int d))))),
                                               (`Int
                                                  (_loc, (string_of_int e))))),
                                          (`Int (_loc, (string_of_int f))))),
                                     (`Int (_loc, (string_of_int g))))),
                                (if h
                                 then `Id (_loc, (`Lid (_loc, "true")))
                                 else `Id (_loc, (`Lid (_loc, "false")))))))))))
           with | Not_found  -> super#expr e)
      | e -> super#expr e
    method! patt =
      function
      | `Id (_loc,`Lid (_,x))|`Id (_loc,`Uid (_,x)) as p ->
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
      | `Ant (_loc,s)|`Str (_loc,s) as p when is_antiquot s ->
          (match view_antiquot s with
           | Some (_name,code) ->
               let cons = `Id (_loc, (`Lid (_loc, code))) in
               let code' = "__fan__" ^ code in
               let cons' = `Id (_loc, (`Lid (_loc, code'))) in
               let () = constraints <- (cons, cons') :: constraints in
               `Id (_loc, (`Lid (_loc, code')))
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
    `Fun
      (_loc,
        (`Case (_loc, (`Id (_loc, (`Uid (_loc, "()")))), (`Nil _loc), body)))
  else
    List.fold_right
      (fun arg  body  -> `Fun (_loc, (`Case (_loc, arg, (`Nil _loc), body))))
      args body
let _loc = FanLoc.ghost
let app a b = `ExApp (_loc, a, b)
let comma a b = `Com (_loc, a, b)
let rec apply acc = function | [] -> acc | x::xs -> apply (app acc x) xs
let sem a b =
  let _loc = FanLoc.merge (FanAst.loc_of a) (FanAst.loc_of b) in
  `Sem (_loc, a, b)
let list_of_app ty =
  let rec loop t acc =
    match t with
    | `ExApp (_loc,t1,t2) -> loop t1 (t2 :: acc)
    | `Nil _loc -> acc
    | i -> i :: acc in
  loop ty []
let list_of_com ty =
  let rec loop t acc =
    match t with
    | `Com (_loc,t1,t2) -> t1 :: (loop t2 acc)
    | `Nil _loc -> acc
    | i -> i :: acc in
  loop ty []
let list_of_sem ty =
  let rec loop t acc =
    match t with
    | `Sem (_loc,t1,t2) -> t1 :: (loop t2 acc)
    | `Nil _loc -> acc
    | i -> i :: acc in
  loop ty []
let rec view_app acc =
  function | `ExApp (_loc,f,a) -> view_app (a :: acc) f | f -> (f, acc)
let app_of_list = function | [] -> `Nil _loc | l -> List.reduce_left app l
let com_of_list = function | [] -> `Nil _loc | l -> List.reduce_right comma l
let sem_of_list = function | [] -> `Nil _loc | l -> List.reduce_right sem l
let tuple_of_list =
  function
  | [] -> invalid_arg "tuple_of_list while list is empty"
  | x::[] -> x
  | xs -> `Tup (_loc, (com_of_list xs))
let mklist loc =
  let rec loop top =
    function
    | [] -> `Id (_loc, (`Uid (_loc, "[]")))
    | e1::el ->
        let _loc = if top then loc else FanLoc.merge (FanAst.loc_of e1) loc in
        `ExApp
          (_loc, (`ExApp (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e1)),
            (loop false el)) in
  loop true
let rec apply accu =
  function
  | [] -> accu
  | x::xs -> let _loc = FanAst.loc_of x in apply (`ExApp (_loc, accu, x)) xs
let mkarray loc arr =
  let rec loop top =
    function
    | [] -> `Id (_loc, (`Uid (_loc, "[]")))
    | e1::el ->
        let _loc = if top then loc else FanLoc.merge (FanAst.loc_of e1) loc in
        `Array (_loc, (`Sem (_loc, e1, (loop false el)))) in
  let items = arr |> Array.to_list in loop true items
let of_str s =
  let len = String.length s in
  if len = 0
  then invalid_arg "[expr|patt]_of_str len=0"
  else
    (match s.[0] with
     | '`' -> `ExVrn (_loc, (String.sub s 1 (len - 1)))
     | x when Char.is_uppercase x -> `Id (_loc, (`Uid (_loc, s)))
     | _ -> `Id (_loc, (`Lid (_loc, s))))
let of_ident_number cons n =
  apply (`Id (_loc, cons)) (List.init n (fun i  -> `Id (_loc, (xid i))))
let (+>) f names =
  apply f (List.map (fun lid  -> `Id (_loc, (`Lid (_loc, lid)))) names)
let gen_tuple_first ~number  ~off  =
  match number with
  | 1 -> `Id (_loc, (xid ~off 0))
  | n when n > 1 ->
      let lst =
        zfold_left ~start:1 ~until:(number - 1)
          ~acc:(`Id (_loc, (xid ~off 0)))
          (fun acc  i  -> comma acc (`Id (_loc, (xid ~off i)))) in
      `Tup (_loc, lst)
  | _ -> invalid_arg "n < 1 in gen_tuple_first"
let gen_tuple_second ~number  ~off  =
  match number with
  | 1 -> `Id (_loc, (xid ~off:0 off))
  | n when n > 1 ->
      let lst =
        zfold_left ~start:1 ~until:(number - 1)
          ~acc:(`Id (_loc, (xid ~off:0 off)))
          (fun acc  i  -> comma acc (`Id (_loc, (xid ~off:i off)))) in
      `Tup (_loc, lst)
  | _ -> invalid_arg "n < 1 in gen_tuple_first "
let tuple_of_number ast n =
  let res =
    zfold_left ~start:1 ~until:(n - 1) ~acc:ast
      (fun acc  _  -> comma acc ast) in
  if n > 1 then `Tup (_loc, res) else res
let tuple_of_list lst =
  let len = List.length lst in
  match len with
  | 1 -> List.hd lst
  | n when n > 1 -> `Tup (_loc, (List.reduce_left comma lst))
  | _ -> invalid_arg "tuple_of_list n < 1"
let of_vstr_number name i =
  let items = List.init i (fun i  -> `Id (_loc, (xid i))) in
  if items = []
  then `ExVrn (_loc, name)
  else
    (let item = items |> tuple_of_list in
     `ExApp (_loc, (`ExVrn (_loc, name)), item))
let gen_tuple_n ?(cons_transform= fun x  -> x)  ~arity  cons n =
  let args =
    List.init arity
      (fun i  -> List.init n (fun j  -> `Id (_loc, (xid ~off:i j)))) in
  let pat = of_str (cons_transform cons) in
  (List.map (fun lst  -> apply pat lst) args) |> tuple_of_list
let tuple _loc =
  function
  | [] -> `Id (_loc, (`Uid (_loc, "()")))
  | p::[] -> p
  | e::es -> `Tup (_loc, (`Com (_loc, e, (FanAst.exCom_of_list es))))
let mkumin loc prefix arg =
  match arg with
  | `Int (_loc,n) -> `Int (loc, (String.neg n))
  | `Int32 (_loc,n) -> `Int32 (loc, (String.neg n))
  | `Int64 (_loc,n) -> `Int64 (loc, (String.neg n))
  | `NativeInt (_loc,n) -> `NativeInt (loc, (String.neg n))
  | `Flo (_loc,n) -> `Flo (loc, (String.neg n))
  | _ -> `ExApp (loc, (`Id (loc, (`Lid (loc, ("~" ^ prefix))))), arg)
let mk_assert =
  function
  | `Id (_loc,`Lid (_,"false")) -> `ExAsf _loc
  | e -> `ExAsr (_loc, e)
let mk_record label_exprs =
  let rec_bindings =
    List.map
      (fun (label,expr)  -> `RecBind (_loc, (`Lid (_loc, label)), expr))
      label_exprs in
  `Record (_loc, (FanAst.rbSem_of_list rec_bindings), (`Nil _loc))
let failure =
  `ExApp
    (_loc, (`Id (_loc, (`Lid (_loc, "raise")))),
      (`ExApp
         (_loc, (`Id (_loc, (`Uid (_loc, "Failure")))),
           (`Str (_loc, "metafilter: Cannot handle that kind of types ")))))
let (<+) names acc =
  List.fold_right
    (fun name  acc  ->
       `Fun
         (_loc,
           (`Case (_loc, (`Id (_loc, (`Lid (_loc, name)))), (`Nil _loc), acc))))
    names acc
let (<+<) patts acc =
  List.fold_right
    (fun p  acc  -> `Fun (_loc, (`Case (_loc, p, (`Nil _loc), acc)))) patts
    acc
let mep_comma x y =
  `ExApp
    (_loc,
      (`ExApp
         (_loc,
           (`ExApp
              (_loc, (`ExVrn (_loc, "Com")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))), x)), y)
let mvep_comma x y =
  `ExApp
    (_loc, (`ExVrn (_loc, "PaCom")),
      (`Tup
         (_loc,
           (`Com
              (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                (`Com (_loc, x, y)))))))
let mee_comma x y =
  `ExApp
    (_loc,
      (`ExApp
         (_loc,
           (`ExApp
              (_loc, (`ExVrn (_loc, "Com")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))), x)), y)
let mvee_comma x y =
  `ExApp
    (_loc, (`ExVrn (_loc, "Com")),
      (`Tup
         (_loc,
           (`Com
              (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                (`Com (_loc, x, y)))))))
let mee_app x y =
  `ExApp
    (_loc,
      (`ExApp
         (_loc,
           (`ExApp
              (_loc, (`ExVrn (_loc, "ExApp")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))), x)), y)
let vee_app x y =
  `ExApp
    (_loc, (`ExVrn (_loc, "ExApp")),
      (`Tup
         (_loc,
           (`Com
              (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                (`Com (_loc, x, y)))))))
let mep_app x y =
  `ExApp
    (_loc,
      (`ExApp
         (_loc,
           (`ExApp
              (_loc, (`ExVrn (_loc, "PaApp")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))), x)), y)
let vep_app x y =
  `ExApp
    (_loc, (`ExVrn (_loc, "PaApp")),
      (`Tup
         (_loc,
           (`Com
              (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                (`Com (_loc, x, y)))))))
let mep_of_str s =
  let len = String.length s in
  if (s.[0]) = '`'
  then
    let s = String.sub s 1 (len - 1) in
    `ExApp
      (_loc, (`ExVrn (_loc, "PaVrn")),
        (`Tup
           (_loc,
             (`Com
                (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))), (`Str (_loc, s)))))))
  else
    (let u =
       `ExApp
         (_loc, (`ExVrn (_loc, "Uid")),
           (`Tup
              (_loc,
                (`Com
                   (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                     (`Str (_loc, s))))))) in
     `ExApp
       (_loc,
         (`ExApp
            (_loc, (`ExVrn (_loc, "Id")),
              (`Id (_loc, (`Lid (_loc, "_loc")))))), u))
let mee_of_str s =
  let len = String.length s in
  if (s.[0]) = '`'
  then
    let s = String.sub s 1 (len - 1) in
    `ExApp
      (_loc, (`ExVrn (_loc, "ExVrn")),
        (`Tup
           (_loc,
             (`Com
                (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))), (`Str (_loc, s)))))))
  else
    (let u =
       `ExApp
         (_loc, (`ExVrn (_loc, "Uid")),
           (`Tup
              (_loc,
                (`Com
                   (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                     (`Str (_loc, s))))))) in
     `ExApp
       (_loc,
         (`ExApp
            (_loc, (`ExVrn (_loc, "Id")),
              (`Id (_loc, (`Lid (_loc, "_loc")))))), u))
let vee_of_str s =
  `ExApp
    (_loc, (`ExVrn (_loc, "ExVrn")),
      (`Tup
         (_loc,
           (`Com
              (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))), (`Str (_loc, s)))))))
let vep_of_str s =
  `ExApp
    (_loc, (`ExVrn (_loc, "PaVrn")),
      (`Tup
         (_loc,
           (`Com
              (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))), (`Str (_loc, s)))))))
let meee_of_str s =
  let u =
    `ExApp
      (_loc,
        (`ExApp
           (_loc,
             (`ExApp
                (_loc, (`ExVrn (_loc, "ExApp")),
                  (`Id (_loc, (`Lid (_loc, "_loc")))))),
             (`ExApp
                (_loc,
                  (`ExApp
                     (_loc, (`ExVrn (_loc, "ExVrn")),
                       (`Id (_loc, (`Lid (_loc, "_loc")))))),
                  (`Str (_loc, "Uid")))))),
        (`ExApp
           (_loc,
             (`ExApp
                (_loc, (`ExVrn (_loc, "Tup")),
                  (`Id (_loc, (`Lid (_loc, "_loc")))))),
             (`ExApp
                (_loc,
                  (`ExApp
                     (_loc,
                       (`ExApp
                          (_loc, (`ExVrn (_loc, "Com")),
                            (`Id (_loc, (`Lid (_loc, "_loc")))))),
                       (`ExApp
                          (_loc,
                            (`ExApp
                               (_loc, (`ExVrn (_loc, "Id")),
                                 (`Id (_loc, (`Lid (_loc, "_loc")))))),
                            (`ExApp
                               (_loc,
                                 (`ExApp
                                    (_loc, (`ExVrn (_loc, "Lid")),
                                      (`Id (_loc, (`Lid (_loc, "_loc")))))),
                                 (`Str (_loc, "_loc")))))))),
                  (`ExApp
                     (_loc, (`ExVrn (_loc, "Str")),
                       (`Tup
                          (_loc,
                            (`Com
                               (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                                 (`Str (_loc, s))))))))))))) in
  `ExApp
    (_loc,
      (`ExApp
         (_loc,
           (`ExApp
              (_loc, (`ExVrn (_loc, "ExApp")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))),
           (`ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExApp")),
                          (`Id (_loc, (`Lid (_loc, "_loc")))))),
                     (`ExApp
                        (_loc,
                          (`ExApp
                             (_loc, (`ExVrn (_loc, "ExVrn")),
                               (`Id (_loc, (`Lid (_loc, "_loc")))))),
                          (`Str (_loc, "Id")))))),
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "Id")),
                          (`Id (_loc, (`Lid (_loc, "_loc")))))),
                     (`ExApp
                        (_loc,
                          (`ExApp
                             (_loc, (`ExVrn (_loc, "Lid")),
                               (`Id (_loc, (`Lid (_loc, "_loc")))))),
                          (`Str (_loc, "_loc")))))))))), u)
let mk_tuple_ee =
  function
  | [] -> invalid_arg "mktupee arity is zero "
  | x::[] -> x
  | xs ->
      `ExApp
        (_loc, (`ExVrn (_loc, "Tup")),
          (`Tup
             (_loc,
               (`Com
                  (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                    (List.reduce_right mee_comma xs))))))
let mk_tuple_vee =
  function
  | [] -> invalid_arg "mktupee arity is zero "
  | x::[] -> x
  | xs ->
      `ExApp
        (_loc, (`ExVrn (_loc, "Tup")),
          (`Tup
             (_loc,
               (`Com
                  (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                    (List.reduce_right mvee_comma xs))))))
let mk_tuple_ep =
  function
  | [] -> assert false
  | x::[] -> x
  | xs ->
      `ExApp
        (_loc, (`ExVrn (_loc, "Tup")),
          (`Tup
             (_loc,
               (`Com
                  (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                    (List.reduce_right mep_comma xs))))))
let mk_tuple_vep =
  function
  | [] -> assert false
  | x::[] -> x
  | xs ->
      `ExApp
        (_loc, (`ExVrn (_loc, "PaTup")),
          (`Tup
             (_loc,
               (`Com
                  (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                    (List.reduce_right mvep_comma xs))))))
let mee_record_col label expr =
  `ExApp
    (_loc,
      (`ExApp
         (_loc,
           (`ExApp
              (_loc, (`ExVrn (_loc, "RecBind")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))),
           (`ExApp
              (_loc, (`ExVrn (_loc, "Lid")),
                (`Tup
                   (_loc,
                     (`Com
                        (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                          (`Str (_loc, label)))))))))), expr)
let mep_record_col label expr =
  `ExApp
    (_loc,
      (`ExApp
         (_loc,
           (`ExApp
              (_loc, (`ExVrn (_loc, "PaEq")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))),
           (`ExApp
              (_loc, (`ExVrn (_loc, "Lid")),
                (`Tup
                   (_loc,
                     (`Com
                        (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                          (`Str (_loc, label)))))))))), expr)
let mee_record_semi a b =
  `ExApp
    (_loc,
      (`ExApp
         (_loc,
           (`ExApp
              (_loc, (`ExVrn (_loc, "Sem")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))), a)), b)
let mep_record_semi a b =
  `ExApp
    (_loc,
      (`ExApp
         (_loc,
           (`ExApp
              (_loc, (`ExVrn (_loc, "Sem")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))), a)), b)
let mk_record_ee label_exprs =
  (label_exprs |> (List.map (fun (label,expr)  -> mee_record_col label expr)))
    |>
    (fun es  ->
       `ExApp
         (_loc,
           (`ExApp
              (_loc,
                (`ExApp
                   (_loc, (`ExVrn (_loc, "Record")),
                     (`Id (_loc, (`Lid (_loc, "_loc")))))),
                (List.reduce_right mee_record_semi es))),
           (`ExApp
              (_loc, (`ExVrn (_loc, "Nil")),
                (`Id (_loc, (`Lid (_loc, "_loc"))))))))
let mk_record_ep label_exprs =
  let open List in
    (label_exprs |> (map (fun (label,expr)  -> mep_record_col label expr)))
      |>
      (fun es  ->
         `ExApp
           (_loc,
             (`ExApp
                (_loc, (`ExVrn (_loc, "PaRec")),
                  (`Id (_loc, (`Lid (_loc, "_loc")))))),
             (List.reduce_right mep_record_semi es)))
let eta_expand expr number =
  let names = List.init number (fun i  -> x ~off:0 i) in
  names <+ (expr +> names)
let gen_curry_n acc ~arity  cons n =
  let args =
    List.init arity
      (fun i  -> List.init n (fun j  -> `Id (_loc, (xid ~off:i j)))) in
  let pat = Patt.of_str cons in
  List.fold_right
    (fun p  acc  -> `Fun (_loc, (`Case (_loc, p, (`Nil _loc), acc))))
    (List.map (fun lst  -> Patt.apply pat lst) args) acc
let currying match_cases ~arity  =
  if arity >= 2
  then
    let names = List.init arity (fun i  -> x ~off:i 0) in
    let exprs = List.map (fun s  -> `Id (_loc, (`Lid (_loc, s)))) names in
    names <+
      (`Match
         (_loc, (tuple_of_list exprs), (FanAst.mcOr_of_list match_cases)))
  else `Fun (_loc, (FanAst.mcOr_of_list match_cases))
let unknown len =
  if len = 0
  then
    `Send
      (_loc, (`Id (_loc, (`Lid (_loc, "self")))), (`Lid (_loc, "unknown")))
  else
    `ExApp
      (_loc, (`Id (_loc, (`Lid (_loc, "failwith")))),
        (`Str (_loc, "not implemented!")))