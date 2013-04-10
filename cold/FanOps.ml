open LibUtil

open Ast

open AstLoc

let list_of_list (loc : loc) =
  let rec loop top =
    function
    | [] -> `Id (ghost, (`Uid (ghost, "[]")))
    | e1::el ->
        let _loc = if top then loc else FanLoc.merge (loc_of e1) loc in
        `App
          (_loc, (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e1)),
            (loop false el)) in
  loop true

let meta_int _loc i = `Int (_loc, (string_of_int i))

let meta_int32 _loc i = `Int32 (_loc, (Int32.to_string i))

let meta_int64 _loc i = `Int64 (_loc, (Int64.to_string i))

let meta_nativeint _loc i = `Nativeint (_loc, (Nativeint.to_string i))

let meta_float _loc i = `Flo (_loc, (FanUtil.float_repres i))

let meta_string _loc i = `Str (_loc, (String.escaped i))

let meta_char _loc i = `Chr (_loc, (Char.escaped i))

let meta_unit _loc _ = `Id (_loc, (`Uid (_loc, "()")))

let meta_bool _loc =
  function | true  -> `Lid (_loc, "true") | false  -> `Lid (_loc, "false")

let meta_ref mf_a _loc i =
  `Record
    (_loc,
      (`RecBind (_loc, (`Lid (_loc, "contents")), (mf_a _loc i.contents))))

let mklist loc =
  let rec loop top =
    function
    | [] -> `Id (loc, (`Uid (loc, "[]")))
    | e1::el ->
        let _loc = if top then loc else FanLoc.merge (loc_of e1) loc in
        `App
          (_loc, (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e1)),
            (loop false el)) in
  loop true

let meta_list mf_a _loc ls =
  mklist _loc (List.map (fun x  -> mf_a _loc x) ls)

let meta_option mf_a _loc =
  function
  | None  -> `Uid (_loc, "None")
  | Some x -> `App (_loc, (`Uid (_loc, "Some")), (mf_a _loc x))

let meta_arrow (type t) (_mf_a : FanLoc.t -> 'a -> t)
  (_mf_b : FanLoc.t -> 'b -> t) (_loc : FanLoc.t) (_x : 'a -> 'b) =
  invalid_arg "meta_arrow not implemented"

let rec is_module_longident (x : ident) =
  match x with
  | `Dot (_,_,i) -> is_module_longident i
  | `App (_,i1,i2) -> (is_module_longident i1) && (is_module_longident i2)
  | `Uid _ -> true
  | _ -> false

let ident_of_exp: exp -> ident =
  let error () =
    invalid_arg "ident_of_exp: this expession is not an identifier" in
  let rec self (x : exp) =
    (match x with
     | `App (_loc,e1,e2) -> `App (_loc, (self e1), (self e2))
     | `Field (_loc,e1,e2) -> `Dot (_loc, (self e1), (self e2))
     | `Id (_loc,`Lid _) -> error ()
     | `Id (_loc,i) -> if is_module_longident i then i else error ()
     | _ -> error () : ident ) in
  function | `Id (_loc,i) -> i | `App _ -> error () | t -> self t

let ident_of_ctyp =
  let error () = invalid_arg "ident_of_ctyp: this type is not an identifier" in
  let rec self (x : ctyp) =
    match x with
    | `App (_loc,t1,t2) -> `App (_loc, (self t1), (self t2))
    | `Id (_loc,`Lid _) -> error ()
    | `Id (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () in
  function | `Id (_loc,i) -> i | t -> self t

let ident_of_pat =
  let error () =
    invalid_arg "ident_of_pat: this pattern is not an identifier" in
  let rec self =
    function
    | `App (_loc,p1,p2) -> `App (_loc, (self p1), (self p2))
    | `Lid (_loc,_) -> error ()
    | i -> if is_module_longident i then i else error ()
    | _ -> error () in
  function | i -> i | p -> self p

let ty_of_stl =
  function
  | (_loc,s,[]) -> `Id (_loc, (`Uid (_loc, s)))
  | (_loc,s,tl) ->
      `Of (_loc, (`Id (_loc, (`Uid (_loc, s)))), (and_of_list tl))

let ty_of_sbt (_loc,s,v,t) =
  if v
  then `TyColMut (_loc, (`Id (_loc, (`Lid (_loc, s)))), t)
  else `TyCol (_loc, (`Id (_loc, (`Lid (_loc, s)))), t)

let bi_of_pe (p,e) = let _loc = loc_of p in `Bind (_loc, p, e)

let sum_type_of_list l = bar_of_list (List.map ty_of_stl l)

let record_type_of_list l = sem_of_list (List.map ty_of_sbt l)

let binding_of_pel l = and_of_list (List.map bi_of_pe l)

let rec is_irrefut_pat (x : pat) =
  match x with
  | `Lid _|`Dot _|`Uid _ -> true
  | `ArrayEmpty _loc|`LabelS (_loc,_)|(`Lid (_loc,_) : Ast.pat) -> true
  | (`Id (_loc,`Uid (_,"()")) : Ast.pat) -> true
  | (`Any _loc : Ast.pat) -> true
  | (`Alias (_loc,x,_) : Ast.pat) -> is_irrefut_pat x
  | (`Record (_loc,p) : Ast.pat) ->
      List.for_all
        (function | `RecBind (_,_,p) -> is_irrefut_pat p | _ -> true)
        (list_of_sem p [])
  | `Sem (_,p1,p2) -> (is_irrefut_pat p1) && (is_irrefut_pat p2)
  | `Com (_,p1,p2) -> (is_irrefut_pat p1) && (is_irrefut_pat p2)
  | `Bar (_,p1,p2) -> (is_irrefut_pat p1) && (is_irrefut_pat p2)
  | `App (_,p1,p2) -> (is_irrefut_pat p1) && (is_irrefut_pat p2)
  | `Constraint (_,p,_) -> is_irrefut_pat p
  | `Par (_,p) -> is_irrefut_pat p
  | `OptLablS _ -> true
  | `OptLabl (_,_,p)|`OptLablExpr (_,_,p,_) -> is_irrefut_pat p
  | `Label (_,_,p)|`Lazy (_,p) -> is_irrefut_pat p
  | (_ : Ast.pat) -> false
  | `ModuleUnpack _|`ModuleConstraint _ -> true
  | `Ant _ -> false
  | `Vrn (_loc,_)|(`Str (_loc,_) : Ast.pat)|(`PaRng (_loc,_,_) : Ast.pat)
    |(`Flo (_loc,_) : Ast.pat)|(`Nativeint (_loc,_) : Ast.pat)
    |(`Int64 (_loc,_) : Ast.pat)|(`Int32 (_loc,_) : Ast.pat)
    |(`Int (_loc,_) : Ast.pat)|(`Chr (_loc,_) : Ast.pat)
    |(`ClassPath (_loc,_) : Ast.pat)|(`Array (_loc,_) : Ast.pat) -> false

let array_of_array arr =
  match arr with
  | [||] -> `ArrayEmpty FanLoc.ghost
  | _ ->
      let items = (arr |> Array.to_list) |> sem_of_list in
      let _loc = loc_of items in `Array (_loc, items)

let meta_array mf_a _loc ls =
  array_of_array (Array.map (fun x  -> mf_a _loc x) ls)

let bigarray_get loc arr arg =
  let coords =
    match arg with
    | (`Par (_loc,`Com (_,e1,e2)) : Ast.exp)|(`Com (_loc,e1,e2) : Ast.exp) ->
        list_of_com e1 (list_of_com e2 [])
    | _ -> [arg] in
  match coords with
  | [] -> failwith "bigarray_get null list"
  | c1::[] ->
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
                               (`Lid (loc, "get")))))))), arr)), c1) : 
      Ast.exp )
  | c1::c2::[] ->
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
                                    (`Lid (loc, "get")))))))), arr)), c1)),
           c2) : Ast.exp )
  | c1::c2::c3::[] ->
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
                                       (loc, (`Uid (loc, "Array3")),
                                         (`Lid (loc, "get")))))))), arr)),
                     c1)), c2)), c3) : Ast.exp )
  | c1::c2::c3::coords ->
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
                               (`Lid (loc, "get")))))))), arr)),
           (`Array
              (loc,
                (`Sem
                   (loc, c1,
                     (`Sem (loc, c2, (`Sem (loc, c3, (sem_of_list coords)))))))))) : 
      Ast.exp )

let bigarray_set loc var newval =
  match var with
  | (`App
       (_loc,`App
               (_,`Id
                    (_,`Dot
                         (_,`Uid (_,"Bigarray"),`Dot
                                                  (_,`Uid (_,"Array1"),
                                                   `Lid (_,"get")))),arr),c1)
      : Ast.exp) ->
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
             newval) : Ast.exp )
  | (`App
       (_loc,`App
               (_,`App
                    (_,`Id
                         (_,`Dot
                              (_,`Uid (_,"Bigarray"),`Dot
                                                       (_,`Uid (_,"Array2"),
                                                        `Lid (_,"get")))),arr),c1),c2)
      : Ast.exp) ->
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
                       c1)), c2)), newval) : Ast.exp )
  | (`App
       (_loc,`App
               (_,`App
                    (_,`App
                         (_,`Id
                              (_,`Dot
                                   (_,`Uid (_,"Bigarray"),`Dot
                                                            (_,`Uid
                                                                 (_,"Array3"),
                                                             `Lid (_,"get")))),arr),c1),c2),c3)
      : Ast.exp) ->
      Some
        (`Assign
           (loc,
             (`Field
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
                  (`Lid (loc, "contents")))), newval) : Ast.exp )
  | (`App
       (_loc,`App
               (_,`Dot
                    (_,`Uid (_,"Bigarray"),`Dot
                                             (_,`Uid (_,"Genarray"),`Lid
                                                                    (_,"get"))),arr),
        `Array (_,coords))
      : Ast.exp) ->
      Some
        (`App
           (loc,
             (`App
                (loc,
                  (`App
                     (loc,
                       (`Dot
                          (loc, (`Uid (loc, "Bigarray")),
                            (`Dot
                               (loc, (`Uid (loc, "Genarray")),
                                 (`Lid (loc, "set")))))), arr)),
                  (`Array (loc, coords)))), newval) : Ast.exp )
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
  | (`Int (_loc,n) : Ast.exp) -> (`Int (loc, (String.neg n)) : Ast.exp )
  | (`Int32 (_loc,n) : Ast.exp) -> (`Int32 (loc, (String.neg n)) : Ast.exp )
  | (`Int64 (_loc,n) : Ast.exp) -> (`Int64 (loc, (String.neg n)) : Ast.exp )
  | (`Nativeint (_loc,n) : Ast.exp) ->
      (`Nativeint (loc, (String.neg n)) : Ast.exp )
  | (`Flo (_loc,n) : Ast.exp) -> (`Flo (loc, (String.neg n)) : Ast.exp )
  | _ -> (`App (loc, (`Lid (loc, ("~" ^ prefix))), arg) : Ast.exp )

let rec to_generalized x =
  match x with
  | `Arrow (_loc,t1,t2) ->
      let (tl,rt) = to_generalized t2 in ((t1 :: tl), rt)
  | t -> ([], t)