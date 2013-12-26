open Util
open Astf
open Ast_gen
let list_of_list =
  let rec loop =
    function
    | [] -> let ghost = Locf.ghost in (`Uid (ghost, "[]") : Astf.exp )
    | e1::el ->
        let v = loop el in
        let _loc = Locf.merge (loc_of e1) (loc_of v) in
        (`App (_loc, (`App (_loc, (`Uid (_loc, "::")), e1)), v) : Astf.exp ) in
  loop
let meta_int _loc i = (`Int (string_of_int i) : Astfn.exp )
let meta_int32 _loc i = (`Int32 (Int32.to_string i) : Astfn.exp )
let meta_int64 _loc i = (`Int64 (Int64.to_string i) : Astfn.exp )
let meta_nativeint _loc i = (`Nativeint (Nativeint.to_string i) : Astfn.exp )
let meta_float _loc i = (`Flo (string_of_float i) : Astfn.exp )
let meta_string _loc i = (`Str (String.escaped i) : Astfn.exp )
let meta_char _loc i = (`Chr (Char.escaped i) : Astfn.exp )
let meta_unit _loc _ = (`Uid "()" : Astfn.exp )
let meta_bool _loc =
  function
  | true  -> (`Bool true : Astfn.exp )
  | false  -> (`Bool false : Astfn.exp )
let meta_ref mf_a _loc i =
  (`Record (`RecBind ((`Lid "contents"), (mf_a _loc (!i)))) : Astfn.exp )
let mklist loc =
  let rec loop top =
    function
    | [] -> `Uid (loc, "[]")
    | e1::el ->
        let _loc = if top then loc else Locf.merge (loc_of e1) loc in
        `App (_loc, (`App (_loc, (`Uid (_loc, "::")), e1)), (loop false el)) in
  loop true
let meta_list mf_a _loc ls =
  mklist _loc (List.map (fun x  -> mf_a _loc x) ls)
let meta_option mf_a _loc =
  function
  | None  -> (`Uid "None" : Astfn.exp )
  | Some x -> (`App ((`Uid "Some"), (mf_a _loc x)) : Astfn.exp )
let meta_arrow (type t) (_mf_a : Locf.t -> 'a -> t)
  (_mf_b : Locf.t -> 'b -> t) (_loc : Locf.t) (_x : 'a -> 'b) =
  invalid_argf "%s not implemented" "meta_arrow"
let rec is_module_longident (x : ident) =
  match x with
  | `Dot (_,_,i) -> is_module_longident i
  | `Apply (_,i1,i2) -> (is_module_longident i1) && (is_module_longident i2)
  | `Uid _ -> true
  | _ -> false
let ident_of_ctyp: ctyp -> ident =
  let error x =
    invalid_argf "ident_of_ctyp: this type %s is not an identifier"
      (Objs.dump_ctyp x) in
  let rec self (x : ctyp) =
    match x with
    | `Apply (_loc,t1,t2) ->
        `Apply (_loc, (self (t1 :>ctyp)), (self (t2 :>ctyp)))
    | `Lid _ -> error x
    | #ident' as i -> if is_module_longident i then i else error x
    | _ -> error x in
  function | #ident as i -> i | t -> self t
let rec is_irrefut_pat (x : pat) =
  match x with
  | `Lid _ -> true
  | `Bool _ -> false
  | `ArrayEmpty _loc|`LabelS (_loc,_)|(`Uid (_loc,"()") : Astf.pat) -> true
  | (`Any _loc : Astf.pat) -> true
  | `Dot (_,_,y) -> is_irrefut_pat (y : vid  :>pat)
  | (`Alias (_loc,x,_) : Astf.pat) -> is_irrefut_pat x
  | (`Record (_loc,p) : Astf.pat) ->
      List.for_all
        (function | `RecBind (_,_,p) -> is_irrefut_pat p | _ -> true)
        (Ast_basic.list_of_sem p [])
  | `Sem (_,p1,p2) -> (is_irrefut_pat p1) && (is_irrefut_pat p2)
  | `Com (_,p1,p2) -> (is_irrefut_pat p1) && (is_irrefut_pat p2)
  | `Bar (_,p1,p2) -> (is_irrefut_pat p1) && (is_irrefut_pat p2)
  | `App (_,p1,p2) -> (is_irrefut_pat p1) && (is_irrefut_pat p2)
  | `Constraint (_,p,_) -> is_irrefut_pat p
  | `Par (_,p) -> is_irrefut_pat p
  | `OptLablS _ -> true
  | `OptLabl (_,_,p)|`OptLablExpr (_,_,p,_) -> is_irrefut_pat p
  | `Label (_,_,p)|`Lazy (_,p) -> is_irrefut_pat p
  | `Uid _ -> false
  | `ModuleUnpack _|`ModuleConstraint _ -> true
  | `Ant _ -> false
  | `Vrn (_loc,_)|(`Str (_loc,_) : Astf.pat)|(`PaRng (_loc,_,_) : Astf.pat)
    |(`Flo (_loc,_) : Astf.pat)|(`Nativeint (_loc,_) : Astf.pat)
    |(`Int64 (_loc,_) : Astf.pat)|(`Int32 (_loc,_) : Astf.pat)
    |(`Int (_loc,_) : Astf.pat)|(`Chr (_loc,_) : Astf.pat)
    |(`ClassPath (_loc,_) : Astf.pat)|(`Array (_loc,_) : Astf.pat) -> false
let array_of_array arr =
  match arr with
  | [||] -> `ArrayEmpty Locf.ghost
  | _ ->
      let items = (arr |> Array.to_list) |> sem_of_list in
      let _loc = loc_of items in `Array (_loc, items)
let meta_array mf_a _loc ls =
  array_of_array (Array.map (fun x  -> mf_a _loc x) ls)
let bigarray_get loc arr arg =
  let coords =
    match arg with
    | (`Par (_loc,`Com (_,e1,e2)) : Astf.exp)|(`Com (_loc,e1,e2) : Astf.exp)
        -> Ast_basic.list_of_com e1 (Ast_basic.list_of_com e2 [])
    | _ -> [arg] in
  match coords with
  | [] -> failwith "bigarray_get null list"
  | c1::[] ->
      (`App
         (loc,
           (`App
              (loc,
                (`Dot
                   (loc, (`Uid (loc, "Bigarray")),
                     (`Dot (loc, (`Uid (loc, "Array1")), (`Lid (loc, "get")))))),
                arr)), c1) : Astf.exp )
  | c1::c2::[] ->
      (`App
         (loc,
           (`App
              (loc,
                (`App
                   (loc,
                     (`Dot
                        (loc, (`Uid (loc, "Bigarray")),
                          (`Dot
                             (loc, (`Uid (loc, "Array2")),
                               (`Lid (loc, "get")))))), arr)), c1)), c2) : 
      Astf.exp )
  | c1::c2::c3::[] ->
      (`App
         (loc,
           (`App
              (loc,
                (`App
                   (loc,
                     (`App
                        (loc,
                          (`Dot
                             (loc, (`Uid (loc, "Bigarray")),
                               (`Dot
                                  (loc, (`Uid (loc, "Array3")),
                                    (`Lid (loc, "get")))))), arr)), c1)), c2)),
           c3) : Astf.exp )
  | c1::c2::c3::coords ->
      (`App
         (loc,
           (`App
              (loc,
                (`Dot
                   (loc, (`Uid (loc, "Bigarray")),
                     (`Dot
                        (loc, (`Uid (loc, "Genarray")), (`Lid (loc, "get")))))),
                arr)),
           (`Array
              (loc,
                (`Sem
                   (loc, c1,
                     (`Sem (loc, c2, (`Sem (loc, c3, (sem_of_list coords)))))))))) : 
      Astf.exp )
let bigarray_set loc var newval =
  match var with
  | (`App
       (_loc,`App
               (_,`Dot
                    (_,`Uid (_,"Bigarray"),`Dot
                                             (_,`Uid (_,"Array1"),`Lid
                                                                    (_,"get"))),arr),c1)
      : Astf.exp) ->
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
                               (loc, (`Uid (loc, "Array1")),
                                 (`Lid (loc, "set")))))), arr)), c1)),
             newval) : Astf.exp )
  | (`App
       (_loc,`App
               (_,`App
                    (_,`Dot
                         (_,`Uid (_,"Bigarray"),`Dot
                                                  (_,`Uid (_,"Array2"),
                                                   `Lid (_,"get"))),arr),c1),c2)
      : Astf.exp) ->
      Some
        (`App
           (loc,
             (`App
                (loc,
                  (`App
                     (loc,
                       (`App
                          (loc,
                            (`Dot
                               (loc, (`Uid (loc, "Bigarray")),
                                 (`Dot
                                    (loc, (`Uid (loc, "Array2")),
                                      (`Lid (loc, "set")))))), arr)), c1)),
                  c2)), newval) : Astf.exp )
  | (`App
       (_loc,`App
               (_,`App
                    (_,`App
                         (_,`Dot
                              (_,`Uid (_,"Bigarray"),`Dot
                                                       (_,`Uid (_,"Array3"),
                                                        `Lid (_,"get"))),arr),c1),c2),c3)
      : Astf.exp) ->
      Some
        (`App
           (loc,
             (`App
                (loc, (`Lid (loc, ":=")),
                  (`App
                     (loc,
                       (`App
                          (loc,
                            (`App
                               (loc,
                                 (`App
                                    (loc,
                                      (`Dot
                                         (loc, (`Uid (loc, "Bigarray")),
                                           (`Dot
                                              (loc, (`Uid (loc, "Array3")),
                                                (`Lid (loc, "get")))))), arr)),
                                 c1)), c2)), c3)))), newval) : Astf.exp )
  | (`App
       (_loc,`App
               (_,`Dot
                    (_,`Uid (_,"Bigarray"),`Dot
                                             (_,`Uid (_,"Genarray"),`Lid
                                                                    (_,"get"))),arr),
        `Array (_,coords))
      : Astf.exp) ->
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
                  (`Array (loc, coords)))), newval) : Astf.exp )
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
  | (`Int (_loc,n) : Astf.exp) -> (`Int (loc, (Stringf.neg n)) : Astf.exp )
  | (`Int32 (_loc,n) : Astf.exp) ->
      (`Int32 (loc, (Stringf.neg n)) : Astf.exp )
  | (`Int64 (_loc,n) : Astf.exp) ->
      (`Int64 (loc, (Stringf.neg n)) : Astf.exp )
  | (`Nativeint (_loc,n) : Astf.exp) ->
      (`Nativeint (loc, (Stringf.neg n)) : Astf.exp )
  | (`Flo (_loc,n) : Astf.exp) -> (`Flo (loc, (Stringf.neg n)) : Astf.exp )
  | _ -> (`App (loc, (`Lid (loc, ("~" ^ prefix))), arg) : Astf.exp )
