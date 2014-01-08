open Astfn
open Astn_util
open Util
open Sigs_util
let mk_variant _cons =
  (function
   | [] -> (`Bool true :>Astfn.exp)
   | ls ->
       Listf.reduce_left_with
         ~compose:(fun x  y  ->
                     (`App
                        ((`App ((`Lid "&&"), (x :>Astfn.exp))),
                          (y :>Astfn.exp)) :>Astfn.exp))
         ~project:(fun (x : Ctyp.ty_info)  -> x.info_exp) ls : Ctyp.ty_info
                                                                 list -> 
                                                                 exp )
let mk_record cols =
  (cols |> (List.map (fun (x : Ctyp.record_col)  -> x.info))) |>
    (mk_variant None)
let gen_eqobj =
  Derive_obj.mk ~kind:Iter ~mk_record ~base:"eqbase" ~class_name:"eq"
    ~mk_variant ~arity:2 ~default:(`Bool false :>Astfn.exp) ()
let some f x = Some (f x)
let _ =
  Derive_stru.register
    {
      id = (`Pre "eq_");
      arity = 2;
      mk_record = (Some mk_record);
      mk_variant = (Some mk_variant);
      default = (Some (Atom (`Bool false :>Astfn.exp)));
      plugin_name = "Eq";
      excludes = [];
      names = [];
      annot = None;
      builtin_tbl =
        [("int",
           (`Constraint
              ((`Dot ((`Uid "Pervasives"), (`Lid "="))),
                (`Arrow
                   ((`Lid "int"), (`Arrow ((`Lid "int"), (`Lid "bool")))))) :>
           Astfn.exp));
        ("char",
          (`Constraint
             ((`Dot ((`Uid "Pervasives"), (`Lid "="))),
               (`Arrow
                  ((`Lid "char"), (`Arrow ((`Lid "char"), (`Lid "bool")))))) :>
          Astfn.exp));
        ("bool",
          (`Constraint
             ((`Dot ((`Uid "Pervasives"), (`Lid "="))),
               (`Arrow
                  ((`Lid "bool"), (`Arrow ((`Lid "bool"), (`Lid "bool")))))) :>
          Astfn.exp));
        ("string",
          (`Constraint
             ((`Dot ((`Uid "Pervasives"), (`Lid "="))),
               (`Arrow
                  ((`Lid "string"),
                    (`Arrow ((`Lid "string"), (`Lid "bool")))))) :>Astfn.exp));
        ("nativeint",
          (`Constraint
             ((`Dot ((`Uid "Pervasives"), (`Lid "="))),
               (`Arrow
                  ((`Lid "nativeint"),
                    (`Arrow ((`Lid "nativeint"), (`Lid "bool")))))) :>
          Astfn.exp));
        ("unit",
          (`Constraint
             ((`Dot ((`Uid "Pervasives"), (`Lid "="))),
               (`Arrow
                  ((`Lid "unit"), (`Arrow ((`Lid "unit"), (`Lid "bool")))))) :>
          Astfn.exp));
        ("int32",
          (`Constraint
             ((`Dot ((`Uid "Pervasives"), (`Lid "="))),
               (`Arrow
                  ((`Lid "int32"), (`Arrow ((`Lid "int32"), (`Lid "bool")))))) :>
          Astfn.exp))]
    };
  List.iter Typehook.register [("OEq", (some gen_eqobj))]
let (gen_fold,gen_fold2) =
  let mk_variant _cons params =
    (params |> (List.map (fun (x : Ctyp.ty_info)  -> x.info_exp))) |>
      (function
       | [] -> (`Lid "self" :>Astfn.exp)
       | ls ->
           Listf.reduce_right
             (fun v  acc  ->
                (`LetIn
                   (`Negative, (`Bind ((`Lid "self"), (v :>Astfn.exp))),
                     (acc :>Astfn.exp)) :>Astfn.exp)) ls) in
  let mk_record cols =
    (cols |> (List.map (fun (x : Ctyp.record_col)  -> x.info))) |>
      (mk_variant None) in
  ((Derive_obj.mk ~kind:Fold ~mk_record ~base:"foldbase" ~class_name:"fold"
      ~mk_variant ()),
    (Derive_obj.mk ~kind:Fold ~mk_record ~base:"foldbase2"
       ~class_name:"fold2" ~mk_variant ~arity:2
       ~default:(`App ((`Lid "invalid_arg"), (`Str "fold2 failure")) :>
       Astfn.exp) ()))
let _ =
  List.iter Typehook.register
    [("Fold", (some gen_fold)); ("Fold2", (some gen_fold2))]
let (gen_map,gen_map2) =
  let mk_variant cons params =
    let result =
      match cons with
      | Some cons ->
          appl_of_list ((of_str cons) ::
            (params |> (List.map (fun (x : Ctyp.ty_info)  -> x.ep0))))
      | None  ->
          (params |> (List.map (fun (x : Ctyp.ty_info)  -> x.ep0))) |>
            tuple_com in
    List.fold_right
      (fun (x : Ctyp.ty_info)  res  ->
         (`LetIn
            (`Negative,
              (`Bind ((x.ep0 :>Astfn.pat), (x.info_exp :>Astfn.exp))),
              (res :>Astfn.exp)) :>Astfn.exp)) params (result :>exp) in
  let mk_record cols =
    let result =
      (cols |>
         (List.map
            (fun (x : Ctyp.record_col)  ->
               match x with
               | { label; info = { ep0;_};_} -> (label, (ep0 :>exp)))))
        |> Expn_util.mk_record in
    List.fold_right
      (fun ({ info = { info_exp = exp; ep0;_};_} : Ctyp.record_col)  res  ->
         (`LetIn
            (`Negative, (`Bind ((ep0 :>Astfn.pat), (exp :>Astfn.exp))),
              (res :>Astfn.exp)) :>Astfn.exp)) cols result in
  ((Derive_obj.mk ~kind:Map ~mk_record ~base:"mapbase" ~class_name:"map"
      ~mk_variant ()),
    (Derive_obj.mk ~kind:Map ~mk_record ~base:"mapbase2" ~class_name:"map2"
       ~mk_variant ~arity:2
       ~default:(`App ((`Lid "invalid_arg"), (`Str "map2 failure")) :>
       Astfn.exp) ()))
let _ =
  [("Map", (some gen_map)); ("Map2", (some gen_map2))] |>
    (List.iter Typehook.register)
let _ =
  let mk_variant cons params =
    let params' =
      List.filter (fun (x : Ctyp.ty_info)  -> x.ty <> (`Lid "loc")) params in
    match cons with
    | Some cons ->
        let result =
          (appl_of_list ((of_str cons) ::
             (params' |> (List.map (fun (x : Ctyp.ty_info)  -> x.ep0)))) :>
          exp) in
        List.fold_right
          (fun (x : Ctyp.ty_info)  res  ->
             match x.ty with
             | `Lid
                 ("int"|"char"|"string"|"int32"|"unit"|"nativeint"|"bool"
                  |"loc")|(`Dot (`Uid "Tokenf",`Lid "quot") : Astfn.ctyp)
               |(`Dot (`Uid "Tokenf",`Lid "ant") : Astfn.ctyp) -> res
             | _ ->
                 (`LetIn
                    (`Negative,
                      (`Bind ((x.ep0 :>Astfn.pat), (x.info_exp :>Astfn.exp))),
                      (res :>Astfn.exp)) :>Astfn.exp)) params' result
    | None  -> assert false in
  Derive_stru.register
    {
      id = `Same;
      arity = 1;
      mk_record = None;
      mk_variant = (Some mk_variant);
      default = None;
      plugin_name = "Strip";
      excludes = ["loc"; "ant"; "quot"];
      names = [];
      builtin_tbl = [];
      annot =
        (Some
           (fun x  ->
              ((`Arrow
                  ((`Dot ((`Uid "Astf"), (`Lid x))),
                    (`Dot ((`Uid "Astfn"), (`Lid x)))) :>Astfn.ctyp),
                (`Dot ((`Uid "Astfn"), (`Lid x)) :>Astfn.ctyp))))
    }
let gen_fill =
  let mk_variant cons params =
    match cons with
    | Some cons ->
        let result =
          (appl_of_list ((of_str cons) :: (`Lid "loc" :>Astfn.ep) ::
             (params |> (List.map (fun (x : Ctyp.ty_info)  -> x.ep0)))) :>
          exp) in
        List.fold_right
          (fun (x : Ctyp.ty_info)  res  ->
             match x.ty with
             | `Lid
                 ("int"|"char"|"string"|"int32"|"unit"|"nativeint"|"bool"
                  |"loc"|"ant")
               |(`Dot (`Uid "Tokenf",`Lid "ant") : Astfn.ctyp)
               |(`Dot (`Uid "Tokenf",`Lid "quot") : Astfn.ctyp) -> res
             | _ ->
                 (`LetIn
                    (`Negative,
                      (`Bind ((x.ep0 :>Astfn.pat), (x.info_exp :>Astfn.exp))),
                      (res :>Astfn.exp)) :>Astfn.exp)) params result
    | None  -> assert false in
  Derive_stru.register
    {
      id = `Same;
      mk_record = None;
      arity = 1;
      default = None;
      mk_variant = (Some mk_variant);
      names = ["loc"];
      annot =
        (Some
           (fun x  ->
              ((`Arrow
                  ((`Dot ((`Uid "Locf"), (`Lid "t"))),
                    (`Arrow
                       ((`Dot ((`Uid "Astfn"), (`Lid x))),
                         (`Dot ((`Uid "Astf"), (`Lid x)))))) :>Astfn.ctyp),
                (`Dot ((`Uid "Astf"), (`Lid x)) :>Astfn.ctyp))));
      plugin_name = "Fill";
      excludes = ["loc"; "ant"; "quot"];
      builtin_tbl = []
    }
let mk_variant cons params =
  let len = List.length params in
  match cons with
  | Some cons when Stringf.ends_with cons "Ant" ->
      (Id_epn.of_vstr_number "Ant" len :>exp)
  | Some cons ->
      let params =
        params |> (List.map (fun (x : Ctyp.ty_info)  -> x.info_exp)) in
      let a = Expn_util.mee_of_str cons in
      (match params with
       | [] -> a
       | _ -> Expn_util.mee_app a (Expn_util.mk_tuple_ee params))
  | None  ->
      (params |> (List.map (fun (x : Ctyp.ty_info)  -> x.info_exp))) |>
        Expn_util.mk_tuple_ee
let mk_record cols =
  (cols |>
     (List.map
        (fun (x : Ctyp.record_col)  -> ((x.label), ((x.info).info_exp)))))
    |> Expn_util.mk_record_ee
let mk_tuple params =
  (params |> (List.map (fun (x : Ctyp.ty_info)  -> x.info_exp))) |>
    Expn_util.mk_tuple_ee
let gen_meta =
  Derive_obj.mk
    ~kind:(Concrete (`Dot ((`Uid "Astf"), (`Lid "ep")) :>Astfn.ctyp))
    ~mk_record ~base:"primitive" ~class_name:"meta" ~mk_variant
    ~names:["_loc"] ()
let _ =
  Typehook.register
    ~filter:(fun s  -> not (List.mem s ["loc"; "ant"; "quot"]))
    ("MetaObj", (some gen_meta))
let extract info =
  info |>
    (Listf.concat_map
       (fun (x : Ctyp.ty_info)  -> [x.name_exp; (x.id_ep :>exp)]))
let mkfmt pre sep post fields =
  let s = pre ^ ((String.concat sep fields) ^ post) in
  (`App
     ((`App ((`Dot ((`Uid "Format"), (`Lid "fprintf"))), (`Lid "fmt"))),
       (`Str s)) :>Astfn.exp)
let mk_variant cons params =
  let len = List.length params in
  let pre =
    match cons with
    | Some cons when len >= 1 ->
        (mkfmt ("@[<1>(" ^ (cons ^ "@ ")) "@ " ")@]") @@
          (Listf.init len (fun _  -> "%a"))
    | Some cons -> mkfmt cons "" "" []
    | None  ->
        (mkfmt "@[<1>(" ",@," ")@]") @@ (Listf.init len (fun _  -> "%a")) in
  appl_of_list (pre :: (extract params))
let mk_record_print cols =
  let pre =
    (cols |> (List.map (fun (x : Ctyp.record_col)  -> x.label ^ ":%a"))) |>
      (mkfmt "@[<hv 1>{" ";@," "}@]") in
  appl_of_list (pre ::
    ((cols |> (List.map (fun (x : Ctyp.record_col)  -> x.info))) |> extract))
let gen_print_obj =
  Derive_obj.mk ~kind:(Concrete (`Lid "unit" :>Astfn.ctyp)) ~base:"printbase"
    ~class_name:"print" ~names:["fmt"] ~mk_record:mk_record_print ~mk_variant
    ()
let () =
  Derive_stru.register
    {
      arity = 1;
      default = None;
      id = (`Pre "pp_print_");
      names = ["fmt"];
      mk_record = (Some mk_record_print);
      annot =
        (Some
           (fun s  ->
              ((`Arrow
                  ((`Dot ((`Uid "Format"), (`Lid "formatter"))),
                    (`Arrow ((`Lid s), (`Lid "unit")))) :>Astfn.ctyp),
                (`Lid "unit" :>Astfn.ctyp))));
      mk_variant = (Some mk_variant);
      plugin_name = "Print";
      excludes = [];
      builtin_tbl = []
    };
  [("OPrint", (some gen_print_obj))] |> (List.iter Typehook.register)
let mk_variant_iter _cons params =
  (match params with
   | [] -> (unit :>exp)
   | _ ->
       let lst =
         params |>
           (List.map
              (fun (x : Ctyp.ty_info)  ->
                 (`App ((x.name_exp :>Astfn.exp), (x.id_ep :>Astfn.exp)) :>
                 Astfn.exp))) in
       seq_sem lst : exp )
let mk_record_iter cols =
  (cols |>
     (List.map
        (fun (x : Ctyp.record_col)  ->
           (`App
              (((x.info).name_exp :>Astfn.exp), ((x.info).id_ep :>Astfn.exp)) :>
           Astfn.exp))))
    |> seq_sem
let gen_iter =
  Derive_obj.mk ~kind:Iter ~base:"iterbase" ~class_name:"iter" ~names:[]
    ~mk_record:mk_record_iter ~mk_variant:mk_variant_iter ()
let _ = ("OIter", (some gen_iter)) |> Typehook.register
let generate (mtyps : mtyps) =
  (let tbl = Hashtbl.create 30 in
   let aux (_,ty) =
     match (ty : decl ) with
     | `TyDcl (_,_,`TyEq (_,`PolyEq t),_) ->
         let branches = Ctyp.view_variant t in
         List.iter
           (function
            | `variant (s,ls) ->
                let arity = List.length ls in
                ((try
                    let v = Hashtbl.find tbl s in
                    fun ()  ->
                      if v <> arity
                      then failwithf "%s has diffireent arities" s
                  with | Not_found  -> (fun ()  -> Hashtbl.add tbl s arity)))
                  ()
            | _ -> ()) branches
     | _ -> failwithf "generate mtyps %s" (Astfn_print.dump_decl ty) in
   let _ =
     List.iter
       (function | `Mutual tys -> List.iter aux tys | `Single t -> aux t)
       mtyps in
   let case =
     Hashtbl.fold
       (fun key  arity  acc  ->
          if arity = 1
          then
            let case =
              (`Case ((`App ((`Vrn key), (`Lid "_loc"))), (`Lid "_loc")) :>
              Astfn.case) in
            match acc with
            | None  -> Some case
            | Some acc -> Some (`Bar (case, acc) :>Astfn.case)
          else
            if arity > 1
            then
              (let pats = (`Lid "_loc" :>Astfn.pat) ::
                 (Listf.init (arity - 1) (const (`Any :>Astfn.pat))) in
               let case =
                 (`Case
                    ((`App ((`Vrn key), (tuple_com pats :>Astfn.pat))),
                      (`Lid "_loc")) :>Astfn.case) in
               match acc with
               | None  -> Some case
               | Some acc -> Some (`Bar (case, acc) :>Astfn.case))
            else failwithf "arity=0 key:%s" key) tbl None in
   match case with
   | Some case ->
       (`Value (`Negative, (`Bind ((`Lid "loc_of"), (`Fun case)))) :>
       Astfn.stru)
   | None  -> failwithf "PluginsN.generate null case" : stru )
let _ =
  Typehook.register ~filter:(fun s  -> not (List.mem s ["loc"]))
    ("GenLoc", (some generate))
let generate (mtyps : mtyps) =
  (let tys: string list =
     Listf.concat_map
       (fun x  ->
          match x with
          | `Mutual tys -> List.map (fun ((x,_) : named_type)  -> x) tys
          | `Single (x,_) -> [x]) mtyps in
   let decl =
     let x =
       (tys |> (List.map (fun x  -> uid @@ (String.capitalize x)))) |>
         bar_of_list in
     (`Type
        (`TyDcl
           ((`Lid "t"), (`Some (`Quote (`Normal, (`Lid "a")))),
             (`TyRepr (`Negative, (`Sum (x :>Astfn.or_ctyp)))), `None)) :>
       Astfn.stru) in
   let to_string =
     let case =
       bar_of_list
         (List.map
            (fun x  ->
               let u = String.capitalize x in
               (`Case ((`Uid u), (`Str x)) :>Astfn.case)) tys) in
     (`Value (`Negative, (`Bind ((`Lid "to_string"), (`Fun case)))) :>
       Astfn.stru) in
   let of_string =
     let case =
       (tys |>
          (List.map
             (fun x  ->
                let u = String.capitalize x in
                (`Case ((`Str x), (`Uid u)) :>Astfn.case))))
         |> bar_of_list in
     (`Value
        (`Negative,
          (`Bind
             ((`Lid "of_string"),
               (`Fun
                  (`Bar
                     (case,
                       (`Case
                          (`Any,
                            (`App
                               ((`Lid "failwith"),
                                 (`App
                                    ((`App ((`Lid "^"), (`Lid "__MODULE__"))),
                                      (`App
                                         ((`App ((`Lid "^"), (`Str "."))),
                                           (`Lid "__BIND__"))))))))))))))) :>
       Astfn.stru) in
   let tags =
     List.map
       (fun x  ->
          let u = String.capitalize x in
          (`Value
             (`Negative,
               (`Bind
                  ((`Lid x),
                    (`Constraint ((`Uid u), (`App ((`Lid "t"), (`Lid x)))))))) :>
            Astfn.stru)) tys in
   sem_of_list (decl :: to_string :: of_string :: tags) : stru )
let _ =
  Typehook.register
    ~filter:(fun s  -> not @@ (List.mem s ["loc"; "ant"; "quot"]))
    ("DynAst", (some generate))
let generate (mtyps : mtyps) =
  (let aux (f : string) =
     ((`Value
         (`Negative,
           (`Bind
              ((`Lid ("map_" ^ f)),
                (`Fun
                   (`Case
                      ((`Lid "f"),
                        (`Obj
                           (`Sem
                              ((`InheritAs
                                  (`Negative, (`Lid "map"), (`Lid "super"))),
                                (`CrMthS
                                   ((`Lid f), `Positive, `Negative,
                                     (`Fun
                                        (`Case
                                           ((`Lid "x"),
                                             (`App
                                                ((`Lid "f"),
                                                  (`App
                                                     ((`Send
                                                         ((`Lid "super"),
                                                           (`Lid f))),
                                                       (`Lid "x"))))))))))))))))))) :>
     Astfn.stru) : stru ) in
   stru_from_ty ~f:aux mtyps : stru )
let _ =
  Typehook.register ~filter:(fun _  -> true) ("MapWrapper", (some generate))
let generate (mtyps : mtyps) =
  (let aux (f : string) =
     ((`Value
         (`Negative,
           (`Bind
              ((`Lid ("dump_" ^ f)),
                (`App
                   ((`Dot ((`Uid "Formatf"), (`Lid "to_string"))),
                     (`Send ((`Lid "dump"), (`Lid f)))))))) :>Astfn.stru) : 
     stru ) in
   sem
     (`Value (`Negative, (`Bind ((`Lid "dump"), (`New (`Lid "print"))))) :>
     Astfn.stru) (stru_from_ty ~f:aux mtyps) : stru )
let _ =
  Typehook.register
    ~filter:(fun s  -> not (List.mem s ["loc"; "ant"; "quot"]))
    ("PrintWrapper", (some generate))
let generate (mtyps : mtyps) =
  (let f (name,ty) =
     if name <> "ant"
     then
       let obj =
         Astfn_map.map_row_field @@
           (function
            | (`TyVrnOf (x,`Lid "loc") : Astfn.row_field) ->
                (`TyVrn (x :>Astfn.astring) :>Astfn.row_field)
            | (`TyVrnOf (x,`Par `Sta (`Lid "loc",y)) : Astfn.row_field) ->
                (match y with
                 | (`Sta (_loc,_) : Astfn.ctyp) ->
                     (`TyVrnOf ((x :>Astfn.astring), (`Par y)) :>Astfn.row_field)
                 | _ ->
                     (`TyVrnOf ((x :>Astfn.astring), (y :>Astfn.ctyp)) :>
                     Astfn.row_field))
            | x -> x) in
       obj#decl ty
     else ty in
   (fun x  -> stru_from_mtyps ~f x) mtyps : stru option )
let _ = Typehook.register ~filter:(fun _  -> true) ("LocType", generate)
