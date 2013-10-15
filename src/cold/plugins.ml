open FAstN
open Astn_util
open Util
open Sig_util
let gen_stru = Derive.gen_stru
let gen_object = Derive.gen_object
let mk_variant _cons =
  (function
   | [] -> (`Lid "true" : FAstN.exp )
   | ls ->
       Listf.reduce_left_with
         ~compose:(fun x  y  ->
                     (`App ((`App ((`Lid "&&"), x)), y) : FAstN.exp ))
         ~project:(fun (x : Ctyp.ty_info)  -> x.info_exp) ls : Ctyp.ty_info
                                                                 list -> 
                                                                 exp )
let mk_tuple exps = mk_variant "" exps
let mk_record: Ctyp.record_col list -> exp =
  fun cols  ->
    (cols |> (List.map (fun (x : Ctyp.record_col)  -> x.info))) |>
      (mk_variant "")
let (gen_eq,gen_eqobj) =
  ((gen_stru ~id:(`Pre "eq_") ~arity:2 ~mk_tuple ~mk_record ~mk_variant
      ~default:(`Lid "false" : FAstN.exp ) ()),
    (gen_object ~kind:Iter ~mk_tuple ~mk_record ~base:"eqbase"
       ~class_name:"eq" ~mk_variant ~arity:2
       ~default:(`Lid "false" : FAstN.exp ) ()))
let some f x = Some (f x)
let _ =
  [("Eq", (some gen_eq)); ("OEq", (some gen_eqobj))] |>
    (List.iter Typehook.register)
let (gen_fold,gen_fold2) =
  let mk_variant _cons params =
    (params |> (List.map (fun (x : Ctyp.ty_info)  -> x.info_exp))) |>
      (function
       | [] -> (`Lid "self" : FAstN.exp )
       | ls ->
           Listf.reduce_right
             (fun v  acc  ->
                (`LetIn (`Negative, (`Bind ((`Lid "self"), v)), acc) : 
                FAstN.exp )) ls) in
  let mk_tuple = mk_variant "" in
  let mk_record cols =
    (cols |> (List.map (fun (x : Ctyp.record_col)  -> x.info))) |>
      (mk_variant "") in
  ((gen_object ~kind:Fold ~mk_tuple ~mk_record ~base:"foldbase"
      ~class_name:"fold" ~mk_variant ()),
    (gen_object ~kind:Fold ~mk_tuple ~mk_record ~base:"foldbase2"
       ~class_name:"fold2" ~mk_variant ~arity:2
       ~default:(`App ((`Lid "invalid_arg"), (`Str "fold2 failure")) : 
       FAstN.exp ) ()))
let _ =
  [("Fold", (some gen_fold)); ("Fold2", (some gen_fold2))] |>
    (List.iter Typehook.register)
let (gen_map,gen_map2) =
  let mk_variant cons params =
    let result =
      appl_of_list ((EpN.of_str cons) ::
        (params |> (List.map (fun (x : Ctyp.ty_info)  -> x.ep0)))) in
    List.fold_right
      (fun (x : Ctyp.ty_info)  res  ->
         (`LetIn (`Negative, (`Bind ((x.ep0 :>pat), (x.info_exp))), res) : 
         FAstN.exp )) params (result :>exp) in
  let mk_tuple params =
    let result =
      (params |> (List.map (fun (x : Ctyp.ty_info)  -> x.ep0))) |> tuple_com in
    List.fold_right
      (fun (x : Ctyp.ty_info)  res  ->
         (`LetIn (`Negative, (`Bind ((x.ep0 :>pat), (x.info_exp))), res) : 
         FAstN.exp )) params (result :>exp) in
  let mk_record cols =
    let result =
      (cols |>
         (List.map
            (fun (x : Ctyp.record_col)  ->
               match x with
               | { label; info = { ep0;_};_} -> (label, (ep0 :>exp)))))
        |> ExpN.mk_record in
    List.fold_right
      (fun ({ info = { info_exp = exp; ep0;_};_} : Ctyp.record_col)  res  ->
         let pat0 = (ep0 :>pat) in
         (`LetIn (`Negative, (`Bind (pat0, exp)), res) : FAstN.exp )) cols
      result in
  ((gen_object ~kind:Map ~mk_tuple ~mk_record ~base:"mapbase"
      ~class_name:"map" ~mk_variant ()),
    (gen_object ~kind:Map ~mk_tuple ~mk_record ~base:"mapbase2"
       ~class_name:"map2" ~mk_variant ~arity:2
       ~default:(`App ((`Lid "invalid_arg"), (`Str "map2 failure")) : 
       FAstN.exp ) ()))
let _ =
  [("Map", (some gen_map)); ("Map2", (some gen_map2))] |>
    (List.iter Typehook.register)
let gen_strip =
  let mk_variant cons params =
    let params' =
      List.filter (fun (x : Ctyp.ty_info)  -> x.ty <> (`Lid "loc")) params in
    let result =
      (appl_of_list ((EpN.of_str cons) ::
         (params' |> (List.map (fun (x : Ctyp.ty_info)  -> x.ep0)))) :>
      exp) in
    List.fold_right
      (fun (x : Ctyp.ty_info)  res  ->
         match x.ty with
         | `Lid ("int"|"string"|"int32"|"nativeint"|"loc")
           |(`Dot (`Uid "FanUtil",`Lid "anti_cxt") : FAstN.ctyp) -> res
         | _ ->
             let pat0 = (x.ep0 :>pat) in
             (`LetIn (`Negative, (`Bind (pat0, (x.info_exp))), res) : 
               FAstN.exp )) params' result in
  let mk_tuple params =
    let result =
      ((params |> (List.map (fun (x : Ctyp.ty_info)  -> x.ep0))) |> tuple_com :>
      exp) in
    List.fold_right
      (fun (x : Ctyp.ty_info)  res  ->
         match x.ty with
         | `Lid ("int"|"string"|"int32"|"nativeint"|"loc")
           |`Dot (`Uid "FanUtil",`Lid "anti_cxt") -> res
         | _ ->
             let pat0 = (x.ep0 :>pat) in
             (`LetIn (`Negative, (`Bind (pat0, (x.info_exp))), res) : 
               FAstN.exp )) params result in
  let mk_record _ = assert false in
  gen_stru ~id:(`Pre "strip_") ~mk_tuple ~mk_record ~mk_variant
    ~annot:(fun x  ->
              ((`Arrow
                  ((`Dot ((`Uid "FAst"), (`Lid x))),
                    (`Dot ((`Uid "FAstN"), (`Lid x)))) : FAstN.ctyp ),
                (`Dot ((`Uid "FAstN"), (`Lid x)) : FAstN.ctyp ))) ()
let _ =
  Typehook.register ~filter:(fun s  -> not (List.mem s ["loc"; "ant"]))
    ("Strip", (some gen_strip))
let gen_fill =
  let mk_variant cons params =
    let result =
      (appl_of_list ((EpN.of_str cons) :: (`Lid "loc" : FAstN.ep ) ::
         (params |> (List.map (fun (x : Ctyp.ty_info)  -> x.ep0)))) :>
      exp) in
    List.fold_right
      (fun (x : Ctyp.ty_info)  res  ->
         match x.ty with
         | `Lid ("int"|"string"|"int32"|"nativeint"|"loc"|"ant")
           |`Dot (`Uid "FanUtil",`Lid "anti_cxt") -> res
         | _ ->
             let pat0 = (x.ep0 :>pat) in
             (`LetIn (`Negative, (`Bind (pat0, (x.info_exp))), res) : 
               FAstN.exp )) params result in
  let mk_tuple params =
    let result =
      ((params |> (List.map (fun (x : Ctyp.ty_info)  -> x.ep0))) |> tuple_com :>
      exp) in
    List.fold_right
      (fun (x : Ctyp.ty_info)  res  ->
         match x.ty with
         | `Lid ("int"|"string"|"int32"|"nativeint"|"loc"|"ant")
           |`Dot (`Uid "FanUtil",`Lid "anti_cxt") -> res
         | _ ->
             let pat0 = (x.ep0 :>pat) in
             (`LetIn (`Negative, (`Bind (pat0, (x.info_exp))), res) : 
               FAstN.exp )) params result in
  let mk_record _cols = assert false in
  gen_stru ~id:(`Pre "fill_") ~mk_tuple ~mk_record ~mk_variant ~names:
    ["loc"]
    ~annot:(fun x  ->
              ((`Arrow
                  ((`Dot ((`Uid "Locf"), (`Lid "t"))),
                    (`Arrow
                       ((`Dot ((`Uid "FAstN"), (`Lid x))),
                         (`Dot ((`Uid "FAst"), (`Lid x)))))) : FAstN.ctyp ),
                (`Dot ((`Uid "FAst"), (`Lid x)) : FAstN.ctyp ))) ()
let _ =
  Typehook.register ~filter:(fun s  -> not (List.mem s ["loc"; "ant"]))
    ("Fill", (some gen_fill))
let mk_variant cons params =
  let len = List.length params in
  if Fstring.ends_with cons "Ant"
  then (EpN.of_vstr_number "Ant" len :>exp)
  else
    (params |> (List.map (fun (x : Ctyp.ty_info)  -> x.info_exp))) |>
      (List.fold_left ExpN.mee_app (ExpN.mee_of_str cons))
let mk_record cols =
  (cols |>
     (List.map
        (fun (x : Ctyp.record_col)  -> ((x.label), ((x.info).info_exp)))))
    |> ExpN.mk_record_ee
let mk_tuple params =
  (params |> (List.map (fun (x : Ctyp.ty_info)  -> x.info_exp))) |>
    ExpN.mk_tuple_ee
let gen_meta_exp =
  gen_stru ~id:(`Pre "meta_") ~names:["_loc"] ~mk_tuple ~mk_record
    ~mk_variant ()
let gen_meta =
  gen_object
    ~kind:(Concrete (`Dot ((`Uid "FAst"), (`Lid "ep")) : FAstN.ctyp ))
    ~mk_tuple ~mk_record ~base:"primitive" ~class_name:"meta" ~mk_variant
    ~names:["_loc"] ()
let _ =
  Typehook.register ~filter:(fun s  -> not (List.mem s ["loc"; "ant"]))
    ("MetaObj", (some gen_meta))
let extract info =
  (info |>
     (List.map (fun (x : Ctyp.ty_info)  -> [x.name_exp; (x.id_ep :>exp)])))
    |> List.concat
let mkfmt pre sep post fields =
  let s = pre ^ ((String.concat sep fields) ^ post) in
  (`App
     ((`App ((`Dot ((`Uid "Format"), (`Lid "fprintf"))), (`Lid "fmt"))),
       (`Str s)) : FAstN.exp )
let mk_variant_print cons params =
  let len = List.length params in
  let pre =
    if len >= 1
    then
      (mkfmt ("@[<1>(" ^ (cons ^ "@ ")) "@ " ")@]") @@
        (Listf.init len (fun _  -> "%a"))
    else mkfmt cons "" "" [] in
  appl_of_list (pre :: (extract params))
let mk_tuple_print params =
  let len = List.length params in
  let pre = (mkfmt "@[<1>(" ",@," ")@]") @@ (Listf.init len (fun _  -> "%a")) in
  appl_of_list (pre :: (extract params))
let mk_record_print cols =
  let pre =
    (cols |> (List.map (fun (x : Ctyp.record_col)  -> x.label ^ ":%a"))) |>
      (mkfmt "@[<hv 1>{" ";@," "}@]") in
  appl_of_list (pre ::
    ((cols |> (List.map (fun (x : Ctyp.record_col)  -> x.info))) |> extract))
let gen_print =
  gen_stru ~id:(`Pre "pp_print_") ~names:["fmt"] ~mk_tuple:mk_tuple_print
    ~mk_record:mk_record_print
    ~annot:(fun s  ->
              ((`Arrow
                  ((`Dot ((`Uid "Format"), (`Lid "formatter"))),
                    (`Arrow ((`Lid s), (`Lid "unit")))) : FAstN.ctyp ),
                (`Lid "unit" : FAstN.ctyp ))) ~mk_variant:mk_variant_print ()
let gen_print_obj =
  gen_object ~kind:(Concrete (`Lid "unit" : FAstN.ctyp ))
    ~mk_tuple:mk_tuple_print ~base:"printbase" ~class_name:"print"
    ~names:["fmt"] ~mk_record:mk_record_print ~mk_variant:mk_variant_print ()
let _ =
  [("Print", (some gen_print)); ("OPrint", (some gen_print_obj))] |>
    (List.iter Typehook.register)
let mk_variant_iter _cons params =
  (match params with
   | [] -> (unit :>exp)
   | _ ->
       let lst =
         params |>
           (List.map
              (fun (x : Ctyp.ty_info)  ->
                 (`App ((x.name_exp), (x.id_ep : ep  :>exp)) : FAstN.exp ))) in
       seq_sem lst : exp )
let mk_tuple_iter params = (mk_variant_iter "" params : exp )
let mk_record_iter cols =
  let lst =
    cols |>
      (List.map
         (fun (x : Ctyp.record_col)  ->
            let id_exp = ((x.info).id_ep :>exp) in
            (`App (((x.info).name_exp), id_exp) : FAstN.exp ))) in
  seq_sem lst
let gen_iter =
  gen_object ~kind:Iter ~base:"iterbase" ~class_name:"iter" ~names:[]
    ~mk_tuple:mk_tuple_iter ~mk_record:mk_record_iter
    ~mk_variant:mk_variant_iter ()
let _ = ("OIter", (some gen_iter)) |> Typehook.register
let generate (mtyps : mtyps) =
  (let tbl = Hashtbl.create 30 in
   let aux (_,ty) =
     match (ty : typedecl ) with
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
     | _ -> failwithf "generate mtyps %s" (ObjsN.dump_typedecl ty) in
   let _ =
     List.iter
       (function | `Mutual tys -> List.iter aux tys | `Single t -> aux t)
       mtyps in
   let case =
     Hashtbl.fold
       (fun key  arity  acc  ->
          if arity = 1
          then
            let case: FAstN.case =
              `Case ((`App ((`Vrn key), (`Lid "_loc"))), (`Lid "_loc")) in
            match acc with
            | None  -> Some case
            | Some acc -> Some (`Bar (case, acc) : FAstN.case )
          else
            if arity > 1
            then
              (let pats = (`Lid "_loc" : FAstN.pat ) ::
                 (Listf.init (arity - 1) (const (`Any : FAstN.pat ))) in
               let case: FAstN.case =
                 `Case ((`App ((`Vrn key), (tuple_com pats))), (`Lid "_loc")) in
               match acc with
               | None  -> Some case
               | Some acc -> Some (`Bar (case, acc) : FAstN.case ))
            else failwithf "arity=0 key:%s" key) tbl None in
   match case with
   | Some case ->
       (`Value (`Negative, (`Bind ((`Lid "loc_of"), (`Fun case)))) : 
       FAstN.stru )
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
   let typedecl =
     let x = bar_of_list (List.map (fun x  -> uid (String.capitalize x)) tys) in
     (`Type
        (`TyDcl
           ((`Lid "tag"), (`Some (`Quote (`Normal, (`Lid "a")))),
             (`TyRepr (`Negative, (`Sum x))), `None)) : FAstN.stru ) in
   let to_string =
     let case =
       bar_of_list
         (List.map
            (fun x  ->
               (`Case ((`Uid (String.capitalize x)), (`Str x)) : FAstN.case ))
            tys) in
     (`Value (`Negative, (`Bind ((`Lid "string_of_tag"), (`Fun case)))) : 
       FAstN.stru ) in
   let tags =
     List.map
       (fun x  ->
          (`Value
             (`Negative,
               (`Bind
                  ((`Lid (x ^ "_tag")),
                    (`Constraint
                       ((`Uid (String.capitalize x)),
                         (`App ((`Lid "tag"), (`Lid x)))))))) : FAstN.stru ))
       tys in
   sem_of_list (typedecl :: to_string :: tags) : stru )
let _ =
  Typehook.register
    ~filter:(fun s  -> not (List.mem s ["loc"; "ant"; "nil"]))
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
                                                       (`Lid "x"))))))))))))))))))) : 
     FAstN.stru ) : stru ) in
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
                     (`Send ((`Lid "dump"), (`Lid f)))))))) : FAstN.stru ) : 
     stru ) in
   sem
     (`Value (`Negative, (`Bind ((`Lid "dump"), (`New (`Lid "print"))))) : 
     FAstN.stru ) (stru_from_ty ~f:aux mtyps) : stru )
let _ =
  Typehook.register
    ~filter:(fun s  -> not (List.mem s ["loc"; "ant"; "nil"]))
    ("PrintWrapper", (some generate))
let generate (mtyps : mtyps) =
  (let f (name,ty) =
     if name <> "ant"
     then
       let obj =
         ObjsN.map_row_field
           (function
            | (`TyVrnOf (x,`Lid "loc") : FAstN.row_field) ->
                (`TyVrn x : FAstN.row_field )
            | (`TyVrnOf (x,`Par `Sta (`Lid "loc",y)) : FAstN.row_field) ->
                (match y with
                 | (`Sta (_loc,_) : FAstN.ctyp) ->
                     (`TyVrnOf (x, (`Par y)) : FAstN.row_field )
                 | _ -> (`TyVrnOf (x, y) : FAstN.row_field ))
            | x -> x) in
       obj#typedecl ty
     else ty in
   (fun x  -> stru_from_mtyps ~f x) mtyps : stru option )
let _ = Typehook.register ~filter:(fun _  -> true) ("LocType", generate)