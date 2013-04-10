open Ast;
open AstLoc;
open LibUtil;
open Easy;

open FSig;

open Exp;


let _loc = FanLoc.ghost;


(* +-----------------------------------------------------------------+
   | Eq generator                                                    |
   +-----------------------------------------------------------------+ *)

let mk_variant _cons : list FSig.ty_info  -> exp  = with exp' fun 
  [ [] -> {|true|}
  | ls -> List.reduce_left_with
        ~compose:(fun x y -> {| $x && $y|}  )
        ~project:(fun {info_exp;_} -> info_exp) ls ];
  
let mk_tuple exps = mk_variant "" exps ;
let mk_record : list FSig.record_col -> exp  = fun cols -> 
    cols |> List.map (fun [ {re_info;_} -> re_info])
         |> mk_variant "" ;
    
let (gen_eq,gen_eqobj) = with exp'
  (gen_stru ~id:(`Pre "eq_")
    ~arity:2 ~mk_tuple ~mk_record ~mk_variant
    ~default: {|false|} (),
   gen_object ~kind:Iter ~mk_tuple ~mk_record
     ~base:"eqbase" ~class_name:"eq"
     ~mk_variant:mk_variant
     ~arity:2 ~default: {|false|} ()) ;
  
[ ("Eq",gen_eq) ; ("OEq",gen_eqobj) ] |> List.iter Typehook.register;



(* +-----------------------------------------------------------------+
   | Fold generator                                                  |
   +-----------------------------------------------------------------+ *)


let (gen_fold,gen_fold2) = with exp'
  let mk_variant _cons params = 
    params
    |> List.map (fun [{info_exp;_} -> info_exp])
    |> (fun
        [ [] -> {|self|}
        | ls ->
            List.reduce_right (fun v acc -> {| let self = $v in $acc |}) ls ]) in
  let mk_tuple  = mk_variant ""  in 
  let mk_record cols =
    cols |> List.map (fun [ {re_info ; _ } -> re_info ] )
         |> mk_variant "" in 
  (gen_object ~kind:Fold ~mk_tuple ~mk_record
     ~base:"foldbase" ~class_name:"fold" ~mk_variant (),
   gen_object ~kind:Fold ~mk_tuple ~mk_record
     ~base:"foldbase2" ~class_name:"fold2"
     ~mk_variant
     ~arity:2 ~default: {|invalid_arg "fold2 failure" |} () ) ;
begin  
   [("Fold",gen_fold);
    ("Fold2",gen_fold2);] |> List.iter Typehook.register;
end;

(* +-----------------------------------------------------------------+
   | Map generator                                                   |
   +-----------------------------------------------------------------+ *)


let (gen_map,gen_map2) = with exp'
  let mk_variant cons params =
    let result =
      appl_of_list
        [ (EP.of_str cons) ::
          params |> List.map (fun [{exp0;_} -> exp0]) ] in 
    List.fold_right
      (fun {info_exp;pat0;_} res ->
              {|let $pat:pat0 = $info_exp in $res |})  params result in
  let mk_tuple params =
    let result = 
      params |> List.map (fun [{exp0; _ } -> exp0]) |> tuple_com in
    List.fold_right
      (fun {info_exp=exp;pat0;_} res ->
        {| let $pat:pat0 = $exp in $res |}) params result in 
  let mk_record cols =
    (* (->label,info.exp0) *)
    let result = 
    cols |> List.map
      (fun [ {re_label; re_info=({exp0;_ } as info) ; _ }  ->
        let _ = Obj.repr info in
        (re_label,exp0) ] )  |> Exp.mk_record   in
    List.fold_right
      (fun {re_info={info_exp=exp;pat0;_};_} res ->
        {|let $pat:pat0 = $exp in $res |}) cols result in
  (gen_object ~kind:Map ~mk_tuple ~mk_record
     ~base:"mapbase" ~class_name:"map"
     ~mk_variant  (),
   gen_object ~kind:Map ~mk_tuple ~mk_record
     ~base:"mapbase2" ~class_name:"map2" ~mk_variant 
     ~arity:2 ~default: {|  invalid_arg "map2 failure" |} ());

begin
  [("Map",gen_map);
   ("Map2",gen_map2);]
  |> List.iter Typehook.register;
end;

(* +-----------------------------------------------------------------+
   | Strip generator                                                 |
   +-----------------------------------------------------------------+ *)
(* FIXME to be more elegant *)  
let gen_strip = with {pat:ctyp;exp:exp'}
  let mk_variant cons params =
    let params' = (List.filter
               (fun [{ty= `Lid(_,"loc");_} -> false | _  -> true])
               params) in
    let result =
      appl_of_list
         [(EP.of_str cons) :: params' |> List.map (fun [{exp0;_} -> exp0]) ]  in 
    List.fold_right
      (fun {info_exp=exp;pat0;ty;_} res ->
        match (ty:ctyp) with
        [ `Lid(_,"int" | "string" | "int32"| "nativeint" |"loc")
        | `Dot(_,`Uid(_,"FanUtil"),`Lid(_,"anti_cxt")) -> 
             res
        | _ -> {|let $pat:pat0 = $exp in $res |}]) params' result in
  let mk_tuple params =
    let result = 
      params |> List.map (fun [{exp0; _ } -> exp0]) |> tuple_com in
    List.fold_right
      (fun {info_exp=exp;pat0;ty;_} res ->
        match ty with
        [ `Lid(_,"int" | "string" | "int32"| "nativeint" |"loc")
        | `Dot(_,`Uid(_,"FanUtil"),`Lid(_,"anti_cxt")) ->  res
        | _ -> {|let $pat:pat0 = $exp in $res |}]) params result in 
  let mk_record cols =
    let result = 
    cols |> List.map (fun [ {re_label; re_info={exp0;_ } ; _ }  ->
          (re_label,exp0) ] )  |> Exp.mk_record   in
    List.fold_right
      (fun {re_info={info_exp=exp;pat0;ty;_};_} res ->
        match ty with
        [ `Lid(_,"int" | "string" | "int32"| "nativeint" |"loc")
        | `Dot(_,`Uid(_,"FanUtil"),`Lid(_,"anti_cxt")) -> 
          res
        | _ -> {|let $pat:pat0 = $exp in $res |}]) cols result in
  gen_stru ~id:(`Pre "strip_loc_") ~mk_tuple ~mk_record ~mk_variant
    ();
Typehook.register
    ~filter:(fun s -> not (List.mem s ["loc"; "ant"]))
    ("Strip",gen_strip);


  
  
(* +-----------------------------------------------------------------+
   | Meta generator                                                  |
   +-----------------------------------------------------------------+ *)
  
let mk_variant cons params = with exp'
    let len = List.length params in 
    if String.ends_with cons "Ant" then
      EP.of_vstr_number "Ant" len
    else
      params
      |> List.map (fun [ {info_exp=exp;_} -> exp ])
      |> List.fold_left mee_app (mee_of_str cons)  ;
        
let mk_record cols = cols |> List.map
  (fun [ {re_label; re_info={info_exp=exp;_};_} -> (re_label, exp)]) |> mk_record_ee ;

let mk_tuple params =
    params |> List.map (fun [{info_exp=exp;_} -> exp]) |> mk_tuple_ee ;

let gen_meta_exp = 
  gen_stru  ~id:(`Pre "meta_")  ~names:["_loc"]
    ~mk_tuple
    ~mk_record ~mk_variant:mk_variant ();

(* add hock FIXME*)  
Typehook.register
    ~position:"__MetaExpr__"
    ~filter:(fun s -> s<>"loc")
    ("MetaExpr",gen_meta_exp);

(* +-----------------------------------------------------------------+
   | Meta Object Generator                                           |
   +-----------------------------------------------------------------+ *)
let gen_meta =
  gen_object ~kind:(Concrete {:ctyp'|ep|})
    ~mk_tuple
    ~mk_record
    ~base:"primitive" ~class_name:"meta" ~mk_variant:mk_variant
    ~names:["_loc"]
    ();
Typehook.register
    ~filter:(fun s -> not (List.mem s ["loc";"ant"]))
    ("MetaObj", gen_meta);
  

(* +-----------------------------------------------------------------+
   | Format generator                                                |
   +-----------------------------------------------------------------+ *)
  
let extract info = info
    |> List.map (fun [{name_exp;id_exp;_} -> [name_exp;id_exp] ])
    |> List.concat ;

let mkfmt pre sep post fields = with exp'
    {| Format.fprintf fmt  $(str: pre^ String.concat sep fields ^ post) |} ;
  
let mk_variant_print cons params =
    let len = List.length params in
    let pre =
        if len >= 1 then
          mkfmt ("@[<1>("^cons^"@ ")
            "@ " ")@]" (List.init len (fun _ -> "%a"))
        else
          mkfmt cons "" "" [] in
    appl_of_list [pre :: extract params ] ;
    
let mk_tuple_print params =
    let len = List.length params in
    let pre = mkfmt "@[<1>(" ",@," ")@]" (List.init len (fun _ -> "%a")) in
    appl_of_list [pre :: extract params];
    (* params |> extract |> apply pre  ; *)
    
let mk_record_print cols = 
    let pre = cols
       |> List.map (fun [ {re_label;_} -> re_label^":%a" ])
       |>  mkfmt "@[<hv 1>{" ";@," "}@]" in
    appl_of_list [pre :: 
                  (cols |> List.map(fun [ {re_info;_} -> re_info ])
                  |> extract )] (* apply pre *)  ;
  
let gen_print =
  gen_stru  ~id:(`Pre "pp_print_")  ~names:["fmt"] 
    ~mk_tuple:mk_tuple_print  ~mk_record:mk_record_print
    ~mk_variant:mk_variant_print ()
    ;    

let gen_print_obj =
  gen_object ~kind:(Concrete {:ctyp'|unit|}) ~mk_tuple:mk_tuple_print
    ~base:"printbase" ~class_name:"print"
    ~names:["fmt"]  ~mk_record:mk_record_print
    ~mk_variant:mk_variant_print ();

[("Print",gen_print);
 ("OPrint",gen_print_obj)] |> List.iter Typehook.register;

(* +-----------------------------------------------------------------+
   | Iter geneartor                                                  |
   +-----------------------------------------------------------------+ *)
let mk_variant_iter _cons params :exp = with exp'
  match params with
  [ [] -> unit _loc 
  | _ -> 
      let lst = params
        |> List.map (fun [{name_exp; id_exp;_} ->
        {| $name_exp $id_exp |}]) in
        seq_sem lst] ;

let mk_tuple_iter params : exp =
  mk_variant_iter "" params;

let mk_record_iter cols = with exp'
  let lst =
    cols |>
    List.map
    (fun [{re_info={name_exp; id_exp;_};_} -> {| $name_exp $id_exp |}]) in
  seq_sem lst;


let gen_iter =
  gen_object ~kind:Iter
    ~base:"iterbase"
    ~class_name:"iter"
    ~names:[] 
    ~mk_tuple:mk_tuple_iter
    ~mk_record:mk_record_iter
    ~mk_variant:mk_variant_iter
    ();

("OIter",gen_iter) |> Typehook.register;

(* +-----------------------------------------------------------------+
   | Get Location generator                                          |
   +-----------------------------------------------------------------+ *)

let generate (module_types:FSig.module_types) : stru =
  with stru' 
  let tbl = Hashtbl.create 30 in
  let aux (_,ty) =
    match (ty:typedecl) with
    [`TyDcl(_,_,_,`TyEq(_,_,`PolyEq(_,t)),_) ->
      let branches = Ctyp.view_variant t in
      List.iter
        (fun
          [`variant (s,ls) ->
            let arity = List.length ls in
            let try v = Hashtbl.find tbl s in
            if v <> arity then
              failwithf "%s has diffireent arities" s
            with 
              [Not_found -> Hashtbl.add tbl s arity]
          | _ -> ()]) branches
    | _ -> FanLoc.errorf (loc_of ty) "generate module_types %s" (Objs.dump_typedecl ty) ] in   
  let _ =
    List.iter (fun [`Mutual tys ->
      List.iter aux tys
         |`Single t -> aux t]) module_types in
  let case = Hashtbl.fold
    (fun key arity acc ->
      if arity= 1 then
        let case = {:case'| $vrn:key _loc -> _loc |} in
        match acc with
        [None ->   Some case 
        |Some acc ->
          Some (`Bar(_loc,case,acc)) ]
      else if arity > 1 then 
        let pats =
          [ {:pat'| _loc|} :: List.init (arity - 1) (fun _ -> {:pat| _ |}) ] in
        let case = {:case'| $vrn:key $(pat:(tuple_com pats)) -> _loc |} in
        match acc with
        [None -> Some case
        |Some acc -> Some(`Bar(_loc,case,acc))]  
      else failwithf "arity=0 key:%s" key
        
    ) tbl None  in
  match case with
  [Some case ->   
    {| let loc_of  = fun [ $case ]|}
  |None -> failwithf "AstTypeGen.generate null case" ];
Typehook.register
    ~filter:(fun s -> not (List.mem s ["loc"])) ("GenLoc",generate);

(* +-----------------------------------------------------------------+
   | DynAst generator                                                |
   +-----------------------------------------------------------------+ *)
let generate (module_types:FSig.module_types) : stru =
  let tys : list string =
    List.concat_map
      (fun x -> match x with
      [`Mutual tys -> List.map (fun ((x,_):named_type) -> x ) tys
      |`Single (x,_) -> [x] ]) module_types in
  let typedecl =
    let x  = bar_of_list (List.map (fun x -> uid _loc (String.capitalize x)) tys) in (* FIXME *)
    {:stru'@here| type 'a tag = [ $x ]|} (* see PR 5961*) in
  let to_string =
    let case =
      bar_of_list
        (List.map (fun x -> {:case'| $(uid:String.capitalize x) -> $str:x |}) tys) in 
    {:stru'| let string_of_tag = fun [ $case ] |} in
 let tags  =
   List.map
     (fun x->
       {:stru'| let $(lid: x^"_tag") : tag $lid:x = $(uid:String.capitalize x) |}) tys  in
 sem_of_list [typedecl;to_string::tags] ;  
Typehook.register
  ~filter:(fun s -> not (List.mem s ["loc";"ant";"nil"])) ("DynAst",generate);

let generate (module_types:FSig.module_types) : stru =
  let aux (f:string) : stru  =
    {:stru'|
    let $(lid:"map_"^f) f = object
      inherit map as super;
      method! $lid:f x = f (super#$lid:f x);
    end |} in
  FSigUtil.stru_from_ty ~f:aux module_types;  
Typehook.register
  ~filter:(fun _ -> true) ("MapWrapper",generate);
    
let generate (module_types:FSig.module_types) : stru =
  let aux (f:string) : stru  =
    {:stru'|
    let $(lid:"dump_"^f)  = LibUtil.to_string_of_printer dump#$lid:f
  |} in
  sem {:stru'|let dump = new print|}
      (FSigUtil.stru_from_ty ~f:aux module_types);  
Typehook.register
  ~filter:(fun s -> not (List.mem s ["loc";"ant";"nil"]))
      ("PrintWrapper",generate); (* double registration should complain*)


    
(* +-----------------------------------------------------------------+
   | Type Generator                                                  |
   +-----------------------------------------------------------------+ *)
(* remove the loc field *)
let generate (module_types:FSig.module_types) : stru = with stru
  let aux (name,ty) =
    if  name <> "ant" then 
     let obj = Objs.map_row_field begin fun 
       [ {:row_field'| $vrn:x of loc |} -> {:row_field'| $vrn:x |}
       | {:row_field'| $vrn:x of (loc * $y ) |}->
           match y with
          [ {:ctyp'| $_ * $_ |} -> {:row_field| $vrn:x of $par:y |}
          | _ -> {:row_field'| $vrn:x of $y |}]
       | x -> x ]
     end in 
     obj#typedecl ty
  else ty  in
  (fun x ->  FSigUtil.stru_from_module_types ~f:aux x) module_types;

Typehook.register ~filter:(fun _ -> true ) ("LocType",generate);
