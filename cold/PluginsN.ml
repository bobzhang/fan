open AstN

open AstLibN

open LibUtil

open DeriveN

open CtypN

let mk_variant _cons =
  (function
   | [] -> (`Lid "true" : AstN.exp )
   | ls ->
       List.reduce_left_with
         ~compose:(fun x  y  ->
                     (`App ((`App ((`Lid "&&"), x)), y) : AstN.exp ))
         ~project:(fun { info_exp;_}  -> info_exp) ls : ty_info list -> exp )

let mk_tuple exps = mk_variant "" exps

let mk_record: record_col list -> exp =
  fun cols  ->
    (cols |> (List.map (fun { re_info;_}  -> re_info))) |> (mk_variant "")

let (gen_eq,gen_eqobj) =
  ((gen_stru ~id:(`Pre "eq_") ~arity:2 ~mk_tuple ~mk_record ~mk_variant
      ~default:(`Lid "false" : AstN.exp ) ()),
    (gen_object ~kind:Iter ~mk_tuple ~mk_record ~base:"eqbase"
       ~class_name:"eq" ~mk_variant ~arity:2
       ~default:(`Lid "false" : AstN.exp ) ()))

let some f x = Some (f x)

let (gen_fold,gen_fold2) =
  let mk_variant _cons params =
    (params |> (List.map (fun { info_exp;_}  -> info_exp))) |>
      (function
       | [] -> (`Lid "self" : AstN.exp )
       | ls ->
           List.reduce_right
             (fun v  acc  ->
                (`LetIn (`Negative, (`Bind ((`Lid "self"), v)), acc) : 
                AstN.exp )) ls) in
  let mk_tuple = mk_variant "" in
  let mk_record cols =
    (cols |> (List.map (fun { re_info;_}  -> re_info))) |> (mk_variant "") in
  ((gen_object ~kind:Fold ~mk_tuple ~mk_record ~base:"foldbase"
      ~class_name:"fold" ~mk_variant ()),
    (gen_object ~kind:Fold ~mk_tuple ~mk_record ~base:"foldbase2"
       ~class_name:"fold2" ~mk_variant ~arity:2
       ~default:(`App ((`Lid "invalid_arg"), (`Str "fold2 failure")) : 
       AstN.exp ) ()))

let (gen_map,gen_map2) =
  let mk_variant cons params =
    let result =
      appl_of_list ((EPN.of_str cons) ::
        (params |> (List.map (fun { ep0;_}  -> ep0)))) in
    List.fold_right
      (fun { info_exp; ep0;_}  res  ->
         (`LetIn (`Negative, (`Bind ((ep0 :>pat), info_exp)), res) : 
         AstN.exp )) params (result :>exp) in
  let mk_tuple params =
    let result = (params |> (List.map (fun { ep0;_}  -> ep0))) |> tuple_com in
    List.fold_right
      (fun { info_exp = exp; ep0;_}  res  ->
         (`LetIn (`Negative, (`Bind ((ep0 :>pat), exp)), res) : AstN.exp ))
      params (result :>exp) in
  let mk_record cols =
    let result =
      (cols |>
         (List.map
            (fun { re_label; re_info = { ep0;_};_}  ->
               (re_label, (ep0 :>exp)))))
        |> ExpN.mk_record in
    List.fold_right
      (fun { re_info = { info_exp = exp; ep0;_};_}  res  ->
         let pat0 = (ep0 :>pat) in
         (`LetIn (`Negative, (`Bind (pat0, exp)), res) : AstN.exp )) cols
      result in
  ((gen_object ~kind:Map ~mk_tuple ~mk_record ~base:"mapbase"
      ~class_name:"map" ~mk_variant ()),
    (gen_object ~kind:Map ~mk_tuple ~mk_record ~base:"mapbase2"
       ~class_name:"map2" ~mk_variant ~arity:2
       ~default:(`App ((`Lid "invalid_arg"), (`Str "map2 failure")) : 
       AstN.exp ) ()))

let gen_strip =
  let mk_variant cons params =
    let params' =
      List.filter (function | { ty = `Lid "loc";_} -> false | _ -> true)
        params in
    let result =
      (appl_of_list ((EPN.of_str cons) ::
         (params' |> (List.map (fun { ep0;_}  -> ep0)))) :>exp) in
    List.fold_right
      (fun { info_exp = exp; ep0; ty;_}  res  ->
         match (ty : ctyp ) with
         | `Lid ("int"|"string"|"int32"|"nativeint"|"loc")
           |`Dot (`Uid "FanUtil",`Lid "anti_cxt") -> res
         | _ ->
             let pat0 = (ep0 :>pat) in
             (`LetIn (`Negative, (`Bind (pat0, exp)), res) : AstN.exp ))
      params' result in
  let mk_tuple params =
    let result =
      ((params |> (List.map (fun { ep0;_}  -> ep0))) |> tuple_com :>exp) in
    List.fold_right
      (fun { info_exp = exp; ep0; ty;_}  res  ->
         match ty with
         | `Lid ("int"|"string"|"int32"|"nativeint"|"loc")
           |`Dot (`Uid "FanUtil",`Lid "anti_cxt") -> res
         | _ ->
             let pat0 = (ep0 :>pat) in
             (`LetIn (`Negative, (`Bind (pat0, exp)), res) : AstN.exp ))
      params result in
  let mk_record _ = assert false in
  gen_stru ~id:(`Pre "strip_loc_") ~mk_tuple ~mk_record ~mk_variant
    ~annot:(fun x  ->
              ((`Arrow
                  ((`Dot ((`Uid "Ast"), (`Lid x))),
                    (`Dot ((`Uid "AstN"), (`Lid x)))) : AstN.ctyp ),
                (`Dot ((`Uid "AstN"), (`Lid x)) : AstN.ctyp ))) ()

let gen_fill =
  let mk_variant cons params =
    let result =
      (appl_of_list ((EPN.of_str cons) :: (`Lid "loc" : AstN.ep ) ::
         (params |> (List.map (fun { ep0;_}  -> ep0)))) :>exp) in
    List.fold_right
      (fun { info_exp = exp; ep0; ty;_}  res  ->
         match (ty : ctyp ) with
         | `Lid ("int"|"string"|"int32"|"nativeint"|"loc"|"ant")
           |`Dot (`Uid "FanUtil",`Lid "anti_cxt") -> res
         | _ ->
             let pat0 = (ep0 :>pat) in
             (`LetIn (`Negative, (`Bind (pat0, exp)), res) : AstN.exp ))
      params result in
  let mk_tuple params =
    let result =
      ((params |> (List.map (fun { ep0;_}  -> ep0))) |> tuple_com :>exp) in
    List.fold_right
      (fun { info_exp = exp; ep0; ty;_}  res  ->
         match ty with
         | `Lid ("int"|"string"|"int32"|"nativeint"|"loc"|"ant")
           |`Dot (`Uid "FanUtil",`Lid "anti_cxt") -> res
         | _ ->
             let pat0 = (ep0 :>pat) in
             (`LetIn (`Negative, (`Bind (pat0, exp)), res) : AstN.exp ))
      params result in
  let mk_record _cols = assert false in
  gen_stru ~id:(`Pre "fill_loc_") ~mk_tuple ~mk_record ~mk_variant
    ~names:["loc"]
    ~annot:(fun x  ->
              ((`Arrow
                  ((`Dot ((`Uid "FanLoc"), (`Lid "t"))),
                    (`Arrow
                       ((`Dot ((`Uid "AstN"), (`Lid x))),
                         (`Dot ((`Uid "Ast"), (`Lid x)))))) : AstN.ctyp ),
                (`Dot ((`Uid "Ast"), (`Lid x)) : AstN.ctyp ))) ()

let mk_variant cons params =
  let len = List.length params in
  if String.ends_with cons "Ant"
  then (EPN.of_vstr_number "Ant" len :>exp)
  else
    (params |> (List.map (fun { info_exp = exp;_}  -> exp))) |>
      (List.fold_left ExpN.mee_app (ExpN.mee_of_str cons))

let mk_record cols =
  (cols |>
     (List.map
        (fun { re_label; re_info = { info_exp = exp;_};_}  -> (re_label, exp))))
    |> ExpN.mk_record_ee

let mk_tuple params =
  (params |> (List.map (fun { info_exp = exp;_}  -> exp))) |>
    ExpN.mk_tuple_ee

let gen_meta_exp =
  gen_stru ~id:(`Pre "meta_") ~names:["_loc"] ~mk_tuple ~mk_record
    ~mk_variant ()

let gen_meta =
  gen_object ~kind:(Concrete (`Dot ((`Uid "Ast"), (`Lid "ep")) : AstN.ctyp ))
    ~mk_tuple ~mk_record ~base:"primitive" ~class_name:"meta" ~mk_variant
    ~names:["_loc"] ()

let extract info =
  (info |>
     (List.map (fun { name_exp; id_ep;_}  -> [name_exp; (id_ep :>exp)])))
    |> List.concat

let mkfmt pre sep post fields =
  (`App
     ((`App ((`Dot ((`Uid "Format"), (`Lid "fprintf"))), (`Lid "fmt"))),
       (`Str (pre ^ ((String.concat sep fields) ^ post)))) : AstN.exp )

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
                  ((`Dot ((`Uid "Format"), (`Lid "formatter"))),
                    (`Arrow ((`Lid s), (`Lid "unit")))) : AstN.ctyp ), unit))
    ~mk_variant:mk_variant_print ()

let gen_print_obj =
  gen_object ~kind:(Concrete unit) ~mk_tuple:mk_tuple_print ~base:"printbase"
    ~class_name:"print" ~names:["fmt"] ~mk_record:mk_record_print
    ~mk_variant:mk_variant_print ()

let mk_variant_iter _cons params =
  (match params with
   | [] -> unit
   | _ ->
       let lst =
         params |>
           (List.map
              (fun { name_exp; id_ep;_}  ->
                 let id_exp = (id_ep :>exp) in
                 (`App (name_exp, id_exp) : AstN.exp ))) in
       seq_sem lst : exp )

let mk_tuple_iter params = (mk_variant_iter "" params : exp )

let mk_record_iter cols =
  let lst =
    cols |>
      (List.map
         (fun { re_info = { name_exp; id_ep;_};_}  ->
            let id_exp = (id_ep :>exp) in
            (`App (name_exp, id_exp) : AstN.exp ))) in
  seq_sem lst

let gen_iter =
  gen_object ~kind:Iter ~base:"iterbase" ~class_name:"iter" ~names:[]
    ~mk_tuple:mk_tuple_iter ~mk_record:mk_record_iter
    ~mk_variant:mk_variant_iter ()

let generate (mtyps : mtyps) =
  (let tbl = Hashtbl.create 30 in
   let aux (_,ty) =
     match (ty : typedecl ) with
     | `TyDcl (_,_,`TyEq (_,`PolyEq t),_) ->
         let branches = CtypN.view_variant t in
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
            let case: AstN.case =
              `Case ((`App ((`Vrn key), (`Lid "_loc"))), (`Lid "_loc")) in
            match acc with
            | None  -> Some case
            | Some acc -> Some (`Bar (case, acc) : AstN.case )
          else
            if arity > 1
            then
              (let pats = (`Lid "_loc" : AstN.pat ) ::
                 (List.init (arity - 1) (fun _  -> (`Any : AstN.pat ))) in
               let case: AstN.case =
                 `Case ((`App ((`Vrn key), (tuple_com pats))), (`Lid "_loc")) in
               match acc with
               | None  -> Some case
               | Some acc -> Some (`Bar (case, acc) : AstN.case ))
            else failwithf "arity=0 key:%s" key) tbl None in
   match case with
   | Some case ->
       (`Value (`Negative, (`Bind ((`Lid "loc_of"), (`Fun case)))) : 
       AstN.stru )
   | None  -> failwithf "AstTypeGen.generate null case" : stru )

let generate (mtyps : mtyps) =
  (let tys: string list =
     List.concat_map
       (fun x  ->
          match x with
          | `Mutual tys -> List.map (fun ((x,_) : named_type)  -> x) tys
          | `Single (x,_) -> [x]) mtyps in
   let typedecl =
     let x = bar_of_list (List.map (fun x  -> uid (String.capitalize x)) tys) in
     (`Type
        (`TyDcl
           ((`Lid "tag"), (`Some (`Quote (`Normal, (`Lid "a")))),
             (`TyRepr (`Negative, (`Sum x))), `None)) : AstN.stru ) in
   let to_string =
     let case =
       bar_of_list
         (List.map
            (fun x  ->
               (`Case ((`Uid (String.capitalize x)), (`Str x)) : AstN.case ))
            tys) in
     (`Value (`Negative, (`Bind ((`Lid "string_of_tag"), (`Fun case)))) : 
       AstN.stru ) in
   let tags =
     List.map
       (fun x  ->
          (`Value
             (`Negative,
               (`Bind
                  ((`Lid (x ^ "_tag")),
                    (`Constraint
                       ((`Uid (String.capitalize x)),
                         (`App ((`Lid "tag"), (`Lid x)))))))) : AstN.stru ))
       tys in
   sem_of_list (typedecl :: to_string :: tags) : stru )

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
     AstN.stru ) : stru ) in
   stru_from_ty ~f:aux mtyps : stru )

let generate (mtyps : mtyps) =
  (let aux (f : string) =
     ((`Value
         (`Negative,
           (`Bind
              ((`Lid ("dump_" ^ f)),
                (`App
                   ((`Dot ((`Uid "LibUtil"), (`Lid "to_string_of_printer"))),
                     (`Send ((`Lid "dump"), (`Lid f)))))))) : AstN.stru ) : 
     stru ) in
   sem
     (`Value (`Negative, (`Bind ((`Lid "dump"), (`New (`Lid "print"))))) : 
     AstN.stru ) (stru_from_ty ~f:aux mtyps) : stru )

let generate (mtyps : mtyps) =
  (let f (name,ty) =
     if name <> "ant"
     then
       let obj =
         ObjsN.map_row_field
           (function
            | (`TyVrnOf (x,`Lid "loc") : AstN.row_field) ->
                (`TyVrn x : AstN.row_field )
            | (`TyVrnOf (x,`Par `Sta (`Lid "loc",y)) : AstN.row_field) ->
                (match y with
                 | (`Sta (_loc,_) : AstN.ctyp) ->
                     (`TyVrnOf (x, (`Par y)) : AstN.row_field )
                 | _ -> (`TyVrnOf (x, y) : AstN.row_field ))
            | x -> x) in
       obj#typedecl ty
     else ty in
   (fun x  -> stru_from_mtyps ~f x) mtyps : stru option )