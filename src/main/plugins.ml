
%%control{ default "exp-" ; }

open FAstN
open Astn_util
open Util
open Sig_util

%import{
Derive:
  gen_stru
  gen_object
  ;
};;

(* +-----------------------------------------------------------------+
   | Eq generator                                                    |
   +-----------------------------------------------------------------+ *)

let mk_variant _cons : Ctyp.ty_info list   -> exp  =
  function 
  | [] -> %exp-{true}
  | ls -> Listf.reduce_left_with
        ~compose:(fun x y -> %exp-{ $x && $y}  )
        ~project:(fun (x:Ctyp.ty_info) -> x.info_exp) ls
  
let mk_tuple exps = mk_variant "" exps
    
let mk_record : Ctyp.record_col list  -> exp  = fun cols -> 
    cols |> List.map (fun  (x:Ctyp.record_col) -> x.info)
         |> mk_variant "" 
    
let (gen_eq,gen_eqobj) = 
  (gen_stru ~id:(`Pre "eq_")
    ~arity:2 ~mk_tuple ~mk_record ~mk_variant
    ~default: %exp-{false} (),
   gen_object ~kind:Iter ~mk_tuple ~mk_record
     ~base:"eqbase" ~class_name:"eq"
     ~mk_variant:mk_variant
     ~arity:2 ~default: %exp-{false} ()) ;;

let some f  = fun x -> Some (f x)  ;;

[ ("Eq",some gen_eq) ; ("OEq", some gen_eqobj ) ] |>
List.iter Typehook.register;;



(* +-----------------------------------------------------------------+
   | Fold generator                                                  |
   +-----------------------------------------------------------------+ *)


let (gen_fold,gen_fold2) = 
  let mk_variant _cons params = 
    params
    |> List.map (fun (x:Ctyp.ty_info)  -> x.info_exp)
    |> (function
        | [] -> %exp-{self}
        | ls ->
            Listf.reduce_right (fun v acc -> %exp-{ let self = $v in $acc }) ls ) in
  let mk_tuple  = mk_variant ""  in 
  let mk_record cols =
    cols |> List.map (fun  (x:Ctyp.record_col) -> x.info  )
         |> mk_variant "" in 
  (gen_object ~kind:Fold ~mk_tuple ~mk_record
     ~base:"foldbase" ~class_name:"fold" ~mk_variant (),
   gen_object ~kind:Fold ~mk_tuple ~mk_record
     ~base:"foldbase2" ~class_name:"fold2"
     ~mk_variant
     ~arity:2 ~default: %exp-{invalid_arg "fold2 failure" } () ) ;;


begin
   [("Fold",some gen_fold);
    ("Fold2",some gen_fold2);] |> List.iter Typehook.register;
end;;

(* +-----------------------------------------------------------------+
   | Map generator                                                   |
   +-----------------------------------------------------------------+ *)


let (gen_map,gen_map2) = 
  let mk_variant cons params =
    let result =
      appl_of_list
        ( (EpN.of_str cons (* :> exp *)) ::
          (params |> List.map (fun (x:Ctyp.ty_info) -> x.ep0)) ) in 
    List.fold_right
      (fun (x:Ctyp.ty_info) res ->
              %exp-{let $(pat: (x.ep0:>pat)) = $(x.info_exp) in $res })  params (result:>exp) in
  let mk_tuple params =
    let result = 
      params |> List.map (fun (x:Ctyp.ty_info) ->x.ep0) |> tuple_com in
    List.fold_right
      (fun (x:Ctyp.ty_info) res ->
        %exp-{ let $(pat:(x.ep0:>pat)) = $(x.info_exp) in $res }) params (result:>exp) in 
  let mk_record cols =
    (* (->label,info.exp0) *)
    let result = 
    cols |> List.map
      (fun  (x:Ctyp.record_col) ->
        match x with
        | {label; info={ep0;_ }  ; _ }  ->
            (label,(ep0:>exp))  )  |> ExpN.mk_record   in
    List.fold_right
      (fun ({info={info_exp=exp;ep0;_};_} : Ctyp.record_col) res ->
        let pat0 = (ep0 :> pat) in 
        %{let $pat:pat0 = $exp in $res }) cols result in
  (gen_object ~kind:Map ~mk_tuple ~mk_record
     ~base:"mapbase" ~class_name:"map"
     ~mk_variant  (),
   gen_object ~kind:Map ~mk_tuple ~mk_record
     ~base:"mapbase2" ~class_name:"map2" ~mk_variant 
     ~arity:2 ~default: %{  invalid_arg "map2 failure" } ());;

begin
  [("Map",some gen_map);
   ("Map2",some gen_map2);]
  |> List.iter Typehook.register;
end;;

(* +-----------------------------------------------------------------+
   | Strip generator                                                 |
   +-----------------------------------------------------------------+ *)
(* FIXME to be more elegant *)  
let gen_strip =
  let mk_variant cons params =
    let params' =
      List.filter
        (function (x:Ctyp.ty_info) -> x.ty <> `Lid "loc")
        params in
    let result =
      (appl_of_list
         (EpN.of_str cons  :: (params' |> List.map (fun (x:Ctyp.ty_info) -> x.ep0) ))
         :> exp)  in 
    List.fold_right
      (fun (x:Ctyp.ty_info) res ->
        match x.ty with
        | `Lid("int" | "string" | "int32"| "nativeint" |"loc")
        | %ctyp-{FanUtil.anti_cxt} -> (** BOOTSTRAPING *)
             res
        | _ ->
            let pat0 = (x.ep0:>pat) in
            %exp-{let $pat:pat0 = $(x.info_exp) in $res }) params' result in
  let mk_tuple params =
    let result = 
      (params |> List.map (fun (x:Ctyp.ty_info) -> x.ep0) |> tuple_com  :> exp) in
    List.fold_right
      (fun (x:Ctyp.ty_info) res ->
        match x.ty with
        | `Lid("int" | "string" | "int32"| "nativeint" |"loc")
        | `Dot(`Uid "FanUtil",`Lid "anti_cxt") ->  res
        | _ ->
            let pat0 = (x.ep0 :> pat) in  
            %exp-{let $pat:pat0 = $(x.info_exp) in $res }) params result in 
  let mk_record _ = assert false in
  gen_stru ~id:(`Pre "strip_") ~mk_tuple ~mk_record ~mk_variant
    ~annot:(fun  x ->
      (* BOOTSTRAPING *)
      (%ctyp-{ FAst.$lid:x -> FAstN.$lid:x }, %ctyp-{FAstN.$lid:x}))
    
    ();;

Typehook.register
    ~filter:(fun s -> not (List.mem s ["loc"; "ant"]))
    ("Strip",some gen_strip);;

(*************************************************************************)
(* Fill location                                                         *) 
(*************************************************************************)
let gen_fill =
  let mk_variant cons params =
    let result =
      (appl_of_list
         (EpN.of_str cons ::
          %ep-{loc} ::
          (params |> List.map (fun (x:Ctyp.ty_info) -> x.ep0) )) :> exp)  in 
    List.fold_right
      (fun (x:Ctyp.ty_info) res ->
        match x.ty with
        | `Lid("int" | "string" | "int32"| "nativeint" |"loc"|"ant")
        | `Dot(`Uid"FanUtil",`Lid"anti_cxt") -> 
             res
        | _ ->
            let pat0 = (x.ep0:>pat) in
            %exp-{let $pat:pat0 = $(x.info_exp) in $res }) params result in
  let mk_tuple params =
    let result = 
      (params |> List.map (fun (x:Ctyp.ty_info) -> x.ep0) |> tuple_com :> exp) in
    List.fold_right
      (fun (x:Ctyp.ty_info) res ->
        match x.ty with
        | `Lid("int" | "string" | "int32"| "nativeint" |"loc"|"ant")
        | `Dot(`Uid "FanUtil",`Lid "anti_cxt") ->  res
        | _ ->
            let pat0 = (x.ep0 :> pat) in
            %exp-{let $pat:pat0 = $(x.info_exp) in $res }) params result in 
  let mk_record _cols = assert false in
  gen_stru
    ~id:(`Pre "fill_") ~mk_tuple
    ~mk_record ~mk_variant
    ~names:["loc"]
    ~annot:(fun x ->
      (%ctyp-{ Locf.t -> FAstN.$lid:x -> FAst.$lid:x },
       %ctyp-{FAst.$lid:x} ))
    ();;

Typehook.register
    ~filter:(fun s -> not (List.mem s ["loc"; "ant"]))
    ("Fill",some gen_fill);;

  
  
(* +-----------------------------------------------------------------+
   | Meta generator                                                  |
   +-----------------------------------------------------------------+ *)
  
let mk_variant cons params = 
  let len = List.length params in 
  if Fstring.ends_with cons "Ant" then
    (EpN.of_vstr_number "Ant" len :> exp)
  else
    params
    |> List.map (fun  (x:Ctyp.ty_info) -> x.info_exp )
    |> List.fold_left ExpN.mee_app (ExpN.mee_of_str cons)  
    
let mk_record cols = cols |> List.map
  (fun  (x : Ctyp.record_col)
    -> (x.label, x.info.info_exp)) |>
  ExpN.mk_record_ee 

let mk_tuple params =
    params |> List.map (fun (x:Ctyp.ty_info) -> x.info_exp) |> ExpN.mk_tuple_ee 

let gen_meta_exp = 
  gen_stru  ~id:(`Pre "meta_")  ~names:["_loc"]
    ~mk_tuple
    ~mk_record ~mk_variant:mk_variant ();;

(* add hock FIXME*)  
(* Typehook.register *)
(*     ~position:"__MetaExpr__" *)
(*     ~filter:(fun s -> s<>"loc") *)
(*     ("MetaExpr",some gen_meta_exp);; *)

(* +-----------------------------------------------------------------+
   | Meta Object Generator                                           |
   +-----------------------------------------------------------------+ *)
let gen_meta =
  gen_object ~kind:(Concrete %ctyp-{FAst.ep})
    ~mk_tuple
    ~mk_record
    ~base:"primitive" ~class_name:"meta" ~mk_variant:mk_variant
    ~names:["_loc"]
    ();;


Typehook.register
    ~filter:(fun s -> not (List.mem s ["loc";"ant"]))
    ("MetaObj", some gen_meta);;
  

(* +-----------------------------------------------------------------+
   | Format generator                                                |
   +-----------------------------------------------------------------+ *)
  
let extract info = info
    |> List.map (fun (x:Ctyp.ty_info) -> [x.name_exp;(x.id_ep:>exp)] )
    |> List.concat 

let mkfmt pre sep post fields =
  let s =  pre^ String.concat sep fields ^ post in
  %exp-{ Format.fprintf fmt $str:s } 
  
let mk_variant_print cons params =
    let len = List.length params in
    let pre =
        if len >= 1 then
          mkfmt ("@[<1>("^cons^"@ ") "@ " ")@]" @@ Listf.init len (fun _ -> "%a")
        else
          mkfmt cons "" "" [] in
    appl_of_list (pre :: extract params)
    

let mk_tuple_print params =
    let len = List.length params in
    let pre = mkfmt "@[<1>(" ",@," ")@]" @@ Listf.init len (fun _ -> "%a") in
    appl_of_list (pre :: extract params)

    
let mk_record_print cols = 
    let pre = cols
       |> List.map (fun (x:Ctyp.record_col) -> x.label^":%a" )
       |>  mkfmt "@[<hv 1>{" ";@," "}@]" in
    appl_of_list (pre :: 
                  (cols
                  |> List.map(fun  (x:Ctyp.record_col) -> x.info )
                  |> extract )) (* apply pre *)  
  
let gen_print =
  gen_stru  ~id:(`Pre "pp_print_")  ~names:["fmt"] 
    ~mk_tuple:mk_tuple_print  ~mk_record:mk_record_print
    ~annot:(fun s ->
      (%ctyp-{Format.formatter -> $lid:s -> unit}, %ctyp-{unit}))
    ~mk_variant:mk_variant_print ()


let gen_print_obj =
  gen_object ~kind:(Concrete %ctyp-{unit}) ~mk_tuple:mk_tuple_print
    ~base:"printbase" ~class_name:"print"
    ~names:["fmt"]  ~mk_record:mk_record_print
    ~mk_variant:mk_variant_print ();;

[("Print",some gen_print);
 ("OPrint",some gen_print_obj)] |> List.iter Typehook.register;;

(* +-----------------------------------------------------------------+
   | Iter geneartor                                                  |
   +-----------------------------------------------------------------+ *)
let mk_variant_iter _cons params :exp = 
  match params with
  | [] -> (unit:>exp)
  | _ -> 
      let lst = params
        |> List.map (fun (x:Ctyp.ty_info) -> 
            %exp-{ $(x.name_exp) $(x.id_ep : ep :> exp) }) in
        seq_sem lst 

let mk_tuple_iter params : exp =
  mk_variant_iter "" params

let mk_record_iter cols = 
  let lst =
    cols |>
    List.map
    (fun (x:Ctyp.record_col) ->
      let id_exp = (x.info.id_ep :> exp) in
      %exp-{ $(x.info.name_exp) $id_exp }) in
  seq_sem lst


let gen_iter =
  gen_object ~kind:Iter
    ~base:"iterbase"
    ~class_name:"iter"
    ~names:[] 
    ~mk_tuple:mk_tuple_iter
    ~mk_record:mk_record_iter
    ~mk_variant:mk_variant_iter
    ();;

("OIter",some gen_iter) |> Typehook.register;;

(* +-----------------------------------------------------------------+
   | Get Location generator                                          |
   +-----------------------------------------------------------------+ *)

let generate (mtyps:mtyps) : stru =
  let tbl = Hashtbl.create 30 in
  let aux (_,ty) =
    match (ty:typedecl) with
    |`TyDcl(_,_,`TyEq(_,`PolyEq t),_) ->
      let branches = Ctyp.view_variant t in
      List.iter
        (function
          |`variant (s,ls) ->
              (let arity = List.length ls in
              let try v = Hashtbl.find tbl s in
              if v <> arity then
                failwithf "%s has diffireent arities" s
              with 
                Not_found -> Hashtbl.add tbl s arity)
          | _ -> ()) branches
    | _ -> failwithf  "generate mtyps %s" (ObjsN.dump_typedecl ty)  in   
  let _ =
    List.iter
      (function
        |`Mutual tys -> List.iter aux tys
        |`Single t -> aux t) mtyps in
  let case = Hashtbl.fold
    (fun key arity acc ->
      if arity= 1 then
        let case = %case-{ $vrn:key _loc -> _loc } in
        match acc with
        |None ->   Some case 
        |Some acc ->
          Some (%case-{ $case | $acc }) 
      else if arity > 1 then 
        let pats =
          (%pat-{ _loc} :: Listf.init (arity - 1) (const %pat-{_}) ) in
        let case =
          %case-{ $vrn:key $(pat:(tuple_com pats)) -> _loc } in
        match acc with
        |None -> Some case
        |Some acc -> Some(%case-{ $case | $acc })
      else failwithf "arity=0 key:%s" key
    ) tbl None  in
  match case with
  |Some case ->   
    %stru-{ let loc_of  = function | $case }
  |None -> failwithf "PluginsN.generate null case" ;;


Typehook.register
    ~filter:(fun s -> not (List.mem s ["loc"])) ("GenLoc",some generate);;

(* +-----------------------------------------------------------------+
   | DynAst generator                                                |
   +-----------------------------------------------------------------+ *)
let generate (mtyps:mtyps) : stru =
  let tys :  string list =
    Listf.concat_map
      (fun x ->
        match x with
        |`Mutual tys -> List.map (fun ((x,_):named_type) -> x ) tys
        |`Single (x,_) -> [x] ) mtyps in
  let typedecl =
    let x  = bar_of_list (List.map (fun x -> uid  (String.capitalize x)) tys) in (* FIXME *)
    %stru-{ type 'a tag = | $x  }in
  let to_string =
    let case =
      bar_of_list
        (List.map
           (fun x ->
             %case-{ $(uid:String.capitalize x) -> $str:x }) tys) in 
    %stru-{ let string_of_tag = function | $case  } in
 let tags  =
   List.map
     (fun x->
       %stru-{let $(lid: x^"_tag") :  $lid:x tag = $(uid:String.capitalize x) }) tys  in
       sem_of_list (typedecl::to_string::tags) ;;
  
Typehook.register
  ~filter:(fun s -> not (List.mem s ["loc";"ant";"nil"])) ("DynAst",some generate);;


let generate (mtyps:mtyps) : stru =
  let aux (f:string) : stru  =
    %stru-{
    let $(lid:"map_"^f) f = object
      inherit map as super
      method! $lid:f x = f (super#$lid:f x)
    end } in
  stru_from_ty ~f:aux mtyps;;  

Typehook.register
  ~filter:(fun _ -> true) ("MapWrapper",some generate);;




let generate (mtyps:mtyps) : stru =
  let aux (f:string) : stru  =
    %stru-{  (** BOOTSTRAPING*)
    let $(lid:"dump_"^f)  = Formatf.to_string dump#$lid:f  } in
    sem
      %stru-{let dump = new print}
      (stru_from_ty ~f:aux mtyps);;  


Typehook.register
  ~filter:(fun s -> not (List.mem s ["loc";"ant";"nil"]))
      ("PrintWrapper",some generate);; (* double registration should complain*)


    
(* +-----------------------------------------------------------------+
   | Type Generator                                                  |
   +-----------------------------------------------------------------+ *)
(* remove the loc field *)
let generate (mtyps:mtyps) : stru option = 
  let f (name,ty) =
    if  name <> "ant" then 
     let obj = ObjsN.map_row_field begin function
       | %row_field-{ $vrn:x of loc } -> %row_field-{ $vrn:x }
       | %row_field-{ $vrn:x of (loc * $y ) }->
           (match y with
           | %ctyp-{ $_ * $_ } -> %row_field-{ $vrn:x of $par:y }
           | _ -> %row_field-{ $vrn:x of $y })
       | x -> x 
     end in 
     obj#typedecl ty
  else ty  in
  (fun x ->  stru_from_mtyps ~f x) mtyps;;

Typehook.register ~filter:(fun _ -> true ) ("LocType", generate);;

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/pluginsN.cmo" *)
(* end: *)
