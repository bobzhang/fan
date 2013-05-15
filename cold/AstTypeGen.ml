open Ast

open AstLib

open LibUtil

open Frame

open FSig

let _loc = FanLoc.ghost

let mk_variant _cons =
  (function
   | [] -> (`Lid (_loc, "true") : Ast.exp )
   | ls ->
       List.reduce_left_with
         ~compose:(fun x  y  ->
                     (`App (_loc, (`App (_loc, (`Lid (_loc, "&&")), x)), y) : 
                     Ast.exp ))
         ~project:(fun { FSig.info_exp = info_exp;_}  -> info_exp) ls : 
  FSig.ty_info list -> exp )

let mk_tuple exps = mk_variant "" exps

let mk_record: FSig.record_col list -> exp =
  fun cols  ->
    (cols |> (List.map (fun { FSig.re_info = re_info;_}  -> re_info))) |>
      (mk_variant "")

let (gen_eq,gen_eqobj) =
  ((gen_stru ~id:(`Pre "eq_") ~arity:2 ~mk_tuple ~mk_record ~mk_variant
      ~default:(`Lid (_loc, "false") : Ast.exp ) ()),
    (gen_object ~kind:FSig.Iter ~mk_tuple ~mk_record ~base:"eqbase"
       ~class_name:"eq" ~mk_variant ~arity:2
       ~default:(`Lid (_loc, "false") : Ast.exp ) ()))

let some f x = Some (f x)

let _ =
  [("Eq", (some gen_eq)); ("OEq", (some gen_eqobj))] |>
    (List.iter Typehook.register)

let (gen_fold,gen_fold2) =
  let mk_variant _cons params =
    (params |> (List.map (fun { FSig.info_exp = info_exp;_}  -> info_exp)))
      |>
      (function
       | [] -> (`Lid (_loc, "self") : Ast.exp )
       | ls ->
           List.reduce_right
             (fun v  acc  ->
                (`LetIn
                   (_loc, (`Negative _loc),
                     (`Bind (_loc, (`Lid (_loc, "self")), v)), acc) : 
                Ast.exp )) ls) in
  let mk_tuple = mk_variant "" in
  let mk_record cols =
    (cols |> (List.map (fun { FSig.re_info = re_info;_}  -> re_info))) |>
      (mk_variant "") in
  ((gen_object ~kind:FSig.Fold ~mk_tuple ~mk_record ~base:"foldbase"
      ~class_name:"fold" ~mk_variant ()),
    (gen_object ~kind:FSig.Fold ~mk_tuple ~mk_record ~base:"foldbase2"
       ~class_name:"fold2" ~mk_variant ~arity:2
       ~default:(`App
                   (_loc, (`Lid (_loc, "invalid_arg")),
                     (`Str (_loc, "fold2 failure"))) : Ast.exp ) ()))

let _ =
  [("Fold", (some gen_fold)); ("Fold2", (some gen_fold2))] |>
    (List.iter Typehook.register)

let (gen_map,gen_map2) =
  let mk_variant cons params =
    let result =
      appl_of_list ((EP.of_str cons) ::
        (params |> (List.map (fun { FSig.ep0 = ep0;_}  -> ep0)))) in
    List.fold_right
      (fun { FSig.info_exp = info_exp; ep0;_}  res  ->
         (`LetIn
            (_loc, (`Negative _loc), (`Bind (_loc, (ep0 :>pat), info_exp)),
              res) : Ast.exp )) params (result :>exp) in
  let mk_tuple params =
    let result =
      (params |> (List.map (fun { FSig.ep0 = ep0;_}  -> ep0))) |> tuple_com in
    List.fold_right
      (fun { FSig.info_exp = exp; ep0;_}  res  ->
         (`LetIn
            (_loc, (`Negative _loc), (`Bind (_loc, (ep0 :>pat), exp)), res) : 
         Ast.exp )) params (result :>exp) in
  let mk_record cols =
    let result =
      (cols |>
         (List.map
            (fun { FSig.re_label = re_label; re_info = { FSig.ep0 = ep0;_};_}
                -> (re_label, (ep0 :>exp)))))
        |> Exp.mk_record in
    List.fold_right
      (fun { FSig.re_info = { FSig.info_exp = exp; ep0;_};_}  res  ->
         let pat0 = (ep0 :>pat) in
         (`LetIn (_loc, (`Negative _loc), (`Bind (_loc, pat0, exp)), res) : 
           Ast.exp )) cols result in
  ((gen_object ~kind:FSig.Map ~mk_tuple ~mk_record ~base:"mapbase"
      ~class_name:"map" ~mk_variant ()),
    (gen_object ~kind:FSig.Map ~mk_tuple ~mk_record ~base:"mapbase2"
       ~class_name:"map2" ~mk_variant ~arity:2
       ~default:(`App
                   (_loc, (`Lid (_loc, "invalid_arg")),
                     (`Str (_loc, "map2 failure"))) : Ast.exp ) ()))

let _ =
  [("Map", (some gen_map)); ("Map2", (some gen_map2))] |>
    (List.iter Typehook.register)

let gen_strip =
  let mk_variant cons params =
    let params' =
      List.filter
        (function | { FSig.ty = `Lid (_,"loc");_} -> false | _ -> true)
        params in
    let result =
      (appl_of_list ((EP.of_str cons) ::
         (params' |> (List.map (fun { FSig.ep0 = ep0;_}  -> ep0)))) :>
      exp) in
    List.fold_right
      (fun { FSig.info_exp = exp; ep0; ty;_}  res  ->
         match (ty : ctyp ) with
         | `Lid (_,("int"|"string"|"int32"|"nativeint"|"loc"))
           |`Dot (_,`Uid (_,"FanUtil"),`Lid (_,"anti_cxt")) -> res
         | _ ->
             let pat0 = (ep0 :>pat) in
             (`LetIn (_loc, (`Negative _loc), (`Bind (_loc, pat0, exp)), res) : 
               Ast.exp )) params' result in
  let mk_tuple params =
    let result =
      ((params |> (List.map (fun { FSig.ep0 = ep0;_}  -> ep0))) |> tuple_com :>
      exp) in
    List.fold_right
      (fun { FSig.info_exp = exp; ep0; ty;_}  res  ->
         match ty with
         | `Lid (_,("int"|"string"|"int32"|"nativeint"|"loc"))
           |`Dot (_,`Uid (_,"FanUtil"),`Lid (_,"anti_cxt")) -> res
         | _ ->
             let pat0 = (ep0 :>pat) in
             (`LetIn (_loc, (`Negative _loc), (`Bind (_loc, pat0, exp)), res) : 
               Ast.exp )) params result in
  let mk_record _ = assert false in
  gen_stru ~id:(`Pre "strip_loc_") ~mk_tuple ~mk_record ~mk_variant
    ~annot:(fun x  ->
              ((`Arrow
                  (_loc,
                    (`Dot (_loc, (`Uid (_loc, "Ast")), (`Lid (_loc, x)))),
                    (`Dot (_loc, (`Uid (_loc, "AstN")), (`Lid (_loc, x))))) : 
                Ast.ctyp ),
                (`Dot (_loc, (`Uid (_loc, "AstN")), (`Lid (_loc, x))) : 
                Ast.ctyp ))) ()

let _ =
  Typehook.register ~filter:(fun s  -> not (List.mem s ["loc"; "ant"]))
    ("Strip", (some gen_strip))

let gen_fill =
  let mk_variant cons params =
    let result =
      (appl_of_list ((EP.of_str cons) :: (`Lid (_loc, "loc") : Ast.ep ) ::
         (params |> (List.map (fun { FSig.ep0 = ep0;_}  -> ep0)))) :>
      exp) in
    List.fold_right
      (fun { FSig.info_exp = exp; ep0; ty;_}  res  ->
         match (ty : ctyp ) with
         | `Lid (_,("int"|"string"|"int32"|"nativeint"|"loc"|"ant"))
           |`Dot (_,`Uid (_,"FanUtil"),`Lid (_,"anti_cxt")) -> res
         | _ ->
             let pat0 = (ep0 :>pat) in
             (`LetIn (_loc, (`Negative _loc), (`Bind (_loc, pat0, exp)), res) : 
               Ast.exp )) params result in
  let mk_tuple params =
    let result =
      ((params |> (List.map (fun { FSig.ep0 = ep0;_}  -> ep0))) |> tuple_com :>
      exp) in
    List.fold_right
      (fun { FSig.info_exp = exp; ep0; ty;_}  res  ->
         match ty with
         | `Lid (_,("int"|"string"|"int32"|"nativeint"|"loc"|"ant"))
           |`Dot (_,`Uid (_,"FanUtil"),`Lid (_,"anti_cxt")) -> res
         | _ ->
             let pat0 = (ep0 :>pat) in
             (`LetIn (_loc, (`Negative _loc), (`Bind (_loc, pat0, exp)), res) : 
               Ast.exp )) params result in
  let mk_record _cols = assert false in
  gen_stru ~id:(`Pre "fill_loc_") ~mk_tuple ~mk_record ~mk_variant
    ~names:["loc"]
    ~annot:(fun x  ->
              ((`Arrow
                  (_loc,
                    (`Dot (_loc, (`Uid (_loc, "FanLoc")), (`Lid (_loc, "t")))),
                    (`Arrow
                       (_loc,
                         (`Dot
                            (_loc, (`Uid (_loc, "AstN")), (`Lid (_loc, x)))),
                         (`Dot (_loc, (`Uid (_loc, "Ast")), (`Lid (_loc, x))))))) : 
                Ast.ctyp ),
                (`Dot (_loc, (`Uid (_loc, "Ast")), (`Lid (_loc, x))) : 
                Ast.ctyp ))) ()

let _ =
  Typehook.register ~filter:(fun s  -> not (List.mem s ["loc"; "ant"]))
    ("Fill", (some gen_fill))

let mk_variant cons params =
  let len = List.length params in
  if String.ends_with cons "Ant"
  then (EP.of_vstr_number "Ant" len :>exp)
  else
    (params |> (List.map (fun { info_exp = exp;_}  -> exp))) |>
      (List.fold_left Exp.mee_app (Exp.mee_of_str cons))

let mk_record cols =
  (cols |>
     (List.map
        (fun { re_label; re_info = { info_exp = exp;_};_}  -> (re_label, exp))))
    |> Exp.mk_record_ee

let mk_tuple params =
  (params |> (List.map (fun { info_exp = exp;_}  -> exp))) |> Exp.mk_tuple_ee

let gen_meta_exp =
  gen_stru ~id:(`Pre "meta_") ~names:["_loc"] ~mk_tuple ~mk_record
    ~mk_variant ()

let _ =
  Typehook.register ~position:"__MetaExpr__" ~filter:(fun s  -> s <> "loc")
    ("MetaExpr", (some gen_meta_exp))

let gen_meta =
  gen_object
    ~kind:(Concrete
             (`Dot (_loc, (`Uid (_loc, "Ast")), (`Lid (_loc, "ep"))) : 
             Ast.ctyp )) ~mk_tuple ~mk_record ~base:"primitive"
    ~class_name:"meta" ~mk_variant ~names:["_loc"] ()

let _ =
  Typehook.register ~filter:(fun s  -> not (List.mem s ["loc"; "ant"]))
    ("MetaObj", (some gen_meta))

let extract info =
  (info |>
     (List.map (fun { name_exp; id_ep;_}  -> [name_exp; (id_ep :>exp)])))
    |> List.concat

let mkfmt pre sep post fields =
  (`App
     (_loc,
       (`App
          (_loc,
            (`Dot (_loc, (`Uid (_loc, "Format")), (`Lid (_loc, "fprintf")))),
            (`Lid (_loc, "fmt")))),
       (`Str (_loc, (pre ^ ((String.concat sep fields) ^ post))))) : 
  Ast.exp )

let mk_variant_print cons params =
  let len = List.length params in
  let pre =
    if len >= 1
    then
      mkfmt ("@[<1>(" ^ (cons ^ "@ ")) "@ " ")@]"
        (List.init len (fun _  -> "%a"))
    else mkfmt cons "" "" [] in
  appl_of_list (pre :: (extract params))

let mk_tuple_print params =
  let len = List.length params in
  let pre = mkfmt "@[<1>(" ",@," ")@]" (List.init len (fun _  -> "%a")) in
  appl_of_list (pre :: (extract params))

let mk_record_print cols =
  let pre =
    (cols |> (List.map (fun { re_label;_}  -> re_label ^ ":%a"))) |>
      (mkfmt "@[<hv 1>{" ";@," "}@]") in
  appl_of_list (pre ::
    ((cols |> (List.map (fun { re_info;_}  -> re_info))) |> extract))

let gen_print =
  gen_stru ~id:(`Pre "pp_print_") ~names:["fmt"] ~mk_tuple:mk_tuple_print
    ~mk_record:mk_record_print
    ~annot:(fun s  ->
              ((`Arrow
                  (_loc,
                    (`Dot
                       (_loc, (`Uid (_loc, "Format")),
                         (`Lid (_loc, "formatter")))),
                    (`Arrow (_loc, (`Lid (_loc, s)), (`Lid (_loc, "unit"))))) : 
                Ast.ctyp ), (`Lid (_loc, "unit") : Ast.ctyp )))
    ~mk_variant:mk_variant_print ()

let gen_print_obj =
  gen_object ~kind:(Concrete (`Lid (_loc, "unit") : Ast.ctyp ))
    ~mk_tuple:mk_tuple_print ~base:"printbase" ~class_name:"print"
    ~names:["fmt"] ~mk_record:mk_record_print ~mk_variant:mk_variant_print ()

let _ =
  [("Print", (some gen_print)); ("OPrint", (some gen_print_obj))] |>
    (List.iter Typehook.register)

let mk_variant_iter _cons params =
  (match params with
   | [] -> unit _loc
   | _ ->
       let lst =
         params |>
           (List.map
              (fun { name_exp; id_ep;_}  ->
                 let id_exp = (id_ep :>exp) in
                 (`App (_loc, name_exp, id_exp) : Ast.exp ))) in
       seq_sem lst : exp )

let mk_tuple_iter params = (mk_variant_iter "" params : exp )

let mk_record_iter cols =
  let lst =
    cols |>
      (List.map
         (fun { re_info = { name_exp; id_ep;_};_}  ->
            let id_exp = (id_ep :>exp) in
            (`App (_loc, name_exp, id_exp) : Ast.exp ))) in
  seq_sem lst

let gen_iter =
  gen_object ~kind:Iter ~base:"iterbase" ~class_name:"iter" ~names:[]
    ~mk_tuple:mk_tuple_iter ~mk_record:mk_record_iter
    ~mk_variant:mk_variant_iter ()

let _ = ("OIter", (some gen_iter)) |> Typehook.register

let generate (mtyps : FSig.mtyps) =
  (let tbl = Hashtbl.create 30 in
   let aux (_,ty) =
     match (ty : typedecl ) with
     | `TyDcl (_,_,_,`TyEq (_,_,`PolyEq (_,t)),_) ->
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
     | _ ->
         FanLoc.errorf (loc_of ty) "generate mtyps %s"
           (Objs.dump_typedecl ty) in
   let _ =
     List.iter
       (function | `Mutual tys -> List.iter aux tys | `Single t -> aux t)
       mtyps in
   let case =
     Hashtbl.fold
       (fun key  arity  acc  ->
          if arity = 1
          then
            let case: Ast.case =
              `Case
                (_loc,
                  (`App (_loc, (`Vrn (_loc, key)), (`Lid (_loc, "_loc")))),
                  (`Lid (_loc, "_loc"))) in
            match acc with
            | None  -> Some case
            | Some acc -> Some (`Bar (_loc, case, acc))
          else
            if arity > 1
            then
              (let pats = (`Lid (_loc, "_loc") : Ast.pat ) ::
                 (List.init (arity - 1) (fun _  -> (`Any _loc : Ast.pat ))) in
               let case: Ast.case =
                 `Case
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, key)), (tuple_com pats))),
                     (`Lid (_loc, "_loc"))) in
               match acc with
               | None  -> Some case
               | Some acc -> Some (`Bar (_loc, case, acc)))
            else failwithf "arity=0 key:%s" key) tbl None in
   match case with
   | Some case ->
       (`Value
          (_loc, (`Negative _loc),
            (`Bind (_loc, (`Lid (_loc, "loc_of")), (`Fun (_loc, case))))) : 
       Ast.stru )
   | None  -> failwithf "AstTypeGen.generate null case" : stru )

let _ =
  Typehook.register ~filter:(fun s  -> not (List.mem s ["loc"]))
    ("GenLoc", (some generate))

let generate (mtyps : FSig.mtyps) =
  (let tys: string list =
     List.concat_map
       (fun x  ->
          match x with
          | `Mutual tys -> List.map (fun ((x,_) : named_type)  -> x) tys
          | `Single (x,_) -> [x]) mtyps in
   let typedecl =
     let x =
       bar_of_list (List.map (fun x  -> uid _loc (String.capitalize x)) tys) in
     (`Type
        ((FanLoc.of_tuple
            ("src/AstTypeGen.ml", 409, 13840, 13857, 409, 13840, 13875,
              false)),
          (`TyDcl
             ((FanLoc.of_tuple
                 ("src/AstTypeGen.ml", 409, 13840, 13862, 409, 13840, 13875,
                   false)),
               (`Lid
                  ((FanLoc.of_tuple
                      ("src/AstTypeGen.ml", 409, 13840, 13865, 409, 13840,
                        13868, false)), "tag")),
               (`Some
                  ((FanLoc.of_tuple
                      ("src/AstTypeGen.ml", 409, 13840, 13862, 409, 13840,
                        13868, false)),
                    (`Quote
                       ((FanLoc.of_tuple
                           ("src/AstTypeGen.ml", 409, 13840, 13862, 409,
                             13840, 13864, false)),
                         (`Normal
                            (FanLoc.of_tuple
                               ("src/AstTypeGen.ml", 409, 13840, 13862, 409,
                                 13840, 13864, false))),
                         (`Lid
                            ((FanLoc.of_tuple
                                ("src/AstTypeGen.ml", 409, 13840, 13863, 409,
                                  13840, 13864, false)), "a")))))),
               (`TyRepr
                  ((FanLoc.of_tuple
                      ("src/AstTypeGen.ml", 409, 13840, 13871, 409, 13840,
                        13875, false)),
                    (`Negative
                       (FanLoc.of_tuple
                          ("src/AstTypeGen.ml", 409, 13840, 13871, 409,
                            13840, 13875, false))),
                    (`Sum
                       ((FanLoc.of_tuple
                           ("src/AstTypeGen.ml", 409, 13840, 13871, 409,
                             13840, 13875, false)), x)))),
               (`None
                  (FanLoc.of_tuple
                     ("src/AstTypeGen.ml", 409, 13840, 13862, 409, 13840,
                       13875, false)))))) : Ast.stru ) in
   let to_string =
     let case =
       bar_of_list
         (List.map
            (fun x  ->
               (`Case
                  (_loc, (`Uid (_loc, (String.capitalize x))),
                    (`Str (_loc, x))) : Ast.case )) tys) in
     (`Value
        (_loc, (`Negative _loc),
          (`Bind (_loc, (`Lid (_loc, "string_of_tag")), (`Fun (_loc, case))))) : 
       Ast.stru ) in
   let tags =
     List.map
       (fun x  ->
          (`Value
             (_loc, (`Negative _loc),
               (`Bind
                  (_loc, (`Lid (_loc, (x ^ "_tag"))),
                    (`Constraint
                       (_loc, (`Uid (_loc, (String.capitalize x))),
                         (`App (_loc, (`Lid (_loc, "tag")), (`Lid (_loc, x))))))))) : 
          Ast.stru )) tys in
   sem_of_list (typedecl :: to_string :: tags) : stru )

let _ =
  Typehook.register
    ~filter:(fun s  -> not (List.mem s ["loc"; "ant"; "nil"]))
    ("DynAst", (some generate))

let generate (mtyps : FSig.mtyps) =
  (let aux (f : string) =
     ((`Value
         (_loc, (`Negative _loc),
           (`Bind
              (_loc, (`Lid (_loc, ("map_" ^ f))),
                (`Fun
                   (_loc,
                     (`Case
                        (_loc, (`Lid (_loc, "f")),
                          (`Obj
                             (_loc,
                               (`Sem
                                  (_loc,
                                    (`InheritAs
                                       (_loc, (`Negative _loc),
                                         (`Lid (_loc, "map")),
                                         (`Lid (_loc, "super")))),
                                    (`CrMthS
                                       (_loc, (`Lid (_loc, f)),
                                         (`Positive _loc), (`Negative _loc),
                                         (`Fun
                                            (_loc,
                                              (`Case
                                                 (_loc, (`Lid (_loc, "x")),
                                                   (`App
                                                      (_loc,
                                                        (`Lid (_loc, "f")),
                                                        (`App
                                                           (_loc,
                                                             (`Send
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "super")),
                                                                  (`Lid
                                                                    (_loc, f)))),
                                                             (`Lid
                                                                (_loc, "x"))))))))))))))))))))))) : 
     Ast.stru ) : stru ) in
   FSigUtil.stru_from_ty ~f:aux mtyps : stru )

let _ =
  Typehook.register ~filter:(fun _  -> true) ("MapWrapper", (some generate))

let generate (mtyps : FSig.mtyps) =
  (let aux (f : string) =
     ((`Value
         (_loc, (`Negative _loc),
           (`Bind
              (_loc, (`Lid (_loc, ("dump_" ^ f))),
                (`App
                   (_loc,
                     (`Dot
                        (_loc, (`Uid (_loc, "LibUtil")),
                          (`Lid (_loc, "to_string_of_printer")))),
                     (`Send (_loc, (`Lid (_loc, "dump")), (`Lid (_loc, f))))))))) : 
     Ast.stru ) : stru ) in
   sem
     (`Value
        (_loc, (`Negative _loc),
          (`Bind
             (_loc, (`Lid (_loc, "dump")),
               (`New (_loc, (`Lid (_loc, "print"))))))) : Ast.stru )
     (FSigUtil.stru_from_ty ~f:aux mtyps) : stru )

let _ =
  Typehook.register
    ~filter:(fun s  -> not (List.mem s ["loc"; "ant"; "nil"]))
    ("PrintWrapper", (some generate))

let generate (mtyps : FSig.mtyps) =
  (let f (name,ty) =
     if name <> "ant"
     then
       let obj =
         Objs.map_row_field
           (function
            | (`TyVrnOf (_loc,x,`Lid (_,"loc")) : Ast.row_field) ->
                (`TyVrn (_loc, x) : Ast.row_field )
            | (`TyVrnOf (_loc,x,`Par (_,`Sta (_,`Lid (_,"loc"),y))) :
                Ast.row_field) ->
                (match y with
                 | (`Sta (_loc,_,_) : Ast.ctyp) ->
                     (`TyVrnOf (_loc, x, (`Par (_loc, y))) : Ast.row_field )
                 | _ -> (`TyVrnOf (_loc, x, y) : Ast.row_field ))
            | x -> x) in
       obj#typedecl ty
     else ty in
   (fun x  -> FSigUtil.stru_from_mtyps ~f x) mtyps : stru option )

let _ = Typehook.register ~filter:(fun _  -> true) ("LocType", generate)