
%import{
Derive:
  gen_stru
  gen_object
  ;
};;
open Astfn
open Astn_util
open Util
open Sigs_util



(************************************) 
(* Eq generator                     *)
(************************************) 
(* Unused
   depends on a small runtime [eq_int] and [Tokenf.eq_ant] *)
let mk_variant _cons : Ctyp.ty_info list   -> exp  =
  function 
  | [] ->  %exp-{true}
  | ls -> Listf.reduce_left_with
        ~compose:(fun x y -> %exp-{ $x && $y}  )
        ~project:(fun (x:Ctyp.ty_info) -> x.info_exp) ls
  
let mk_tuple exps = mk_variant "" exps
    
let mk_record : Ctyp.record_col list  -> exp  = fun cols -> 
    cols |> List.map (fun  (x:Ctyp.record_col) -> x.info)
         |> mk_variant "" 


(** it can exist without prefix only when you do not need
    any runtime -- 
 *)             
let (gen_eq,gen_eqobj) = 
  (gen_stru ~id:(`Pre "eq_") 
    ~arity:2 ~mk_tuple ~mk_record ~mk_variant
    ~default: %exp-{false} (),
   gen_object ~kind:Iter ~mk_tuple ~mk_record
     ~base:"eqbase" ~class_name:"eq"
     ~mk_variant:mk_variant
     ~arity:2 ~default:%exp-{false} ()) ;;

let some f  = fun x -> Some (f x)  

let _ = begin
    List.iter Typehook.register
    [ ("Eq",some gen_eq) ; ("OEq", some gen_eqobj ) ]
end



(************************************) 
(* Fold generator                   *)
(************************************)     


(* [Fold2] unused *)
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
     ~arity:2 ~default:%exp-{invalid_arg "fold2 failure" } () )

let _ = 
  begin
    List.iter Typehook.register
      [("Fold",some gen_fold); ("Fold2",some gen_fold2);]
  end


(************************************) 
(* Map generator                   *)
(************************************)     
(* [Map2] unused, [ant] was dispatched as
   [self#tokenf_ant] [self#|Tokenf.ant|]? which was later dispatched to
   [self#unkown] -- or more compatible way
   dumping ast level do the transformation [__Tokenf_ant_]
 *)
let (gen_map,gen_map2) = 
  let mk_variant cons params =
    let result =
      appl_of_list
        (EpN.of_str cons ::
         (params |> List.map (fun (x:Ctyp.ty_info) -> x.ep0)) ) in 
    List.fold_right
      (fun (x:Ctyp.ty_info) res ->
              %exp-{let $pat{x.ep0} = ${x.info_exp} in $res })  params (result:>exp) in
  let mk_tuple params =
    let result = 
      params |> List.map (fun (x:Ctyp.ty_info) ->x.ep0) |> tuple_com in
    List.fold_right
      (fun (x:Ctyp.ty_info) res ->
        %exp-{ let $pat{x.ep0} = ${x.info_exp} in $res }) params (result:>exp) in 
  let mk_record cols =
    (* (->label,info.exp0) *)
    let result = 
    cols
    |> List.map
        (fun  (x:Ctyp.record_col) ->
          match x with
          | {label; info={ep0;_ }  ; _ }  ->
            (label,(ep0:>exp))  )
    |> Expn_util.mk_record   in
    List.fold_right
      (fun ({info={info_exp=exp;ep0;_};_} : Ctyp.record_col) res ->
        %exp-{let $pat{ep0} = $exp in $res }) cols result in
  (gen_object ~kind:Map ~mk_tuple ~mk_record
     ~base:"mapbase" ~class_name:"map"
     ~mk_variant  (),
   gen_object ~kind:Map ~mk_tuple ~mk_record
     ~base:"mapbase2" ~class_name:"map2" ~mk_variant 
     ~arity:2 ~default: %exp-{  invalid_arg "map2 failure" } ());;

begin
  [("Map",some gen_map);
   ("Map2",some gen_map2);]
  |> List.iter Typehook.register;
end;;

(************************************) 
(* Strip generator                   *)
(************************************)     
(* [Astf] transformed to [Astfn].
   Types [loc] and [ant] will not be processed
 *)
let gen_strip =
  let mk_variant cons params =
    let params' =
      List.filter
        (function (x:Ctyp.ty_info) -> x.ty <> `Lid "loc")
        params in
    let result =
      (appl_of_list
         (EpN.of_str cons  :: (params' |> List.map (fun (x:Ctyp.ty_info) -> x.ep0) )) :> exp)  in 
    List.fold_right
      (fun (x:Ctyp.ty_info) res ->
        match x.ty with
        | `Lid("int" |"char"| "string" | "int32"| "unit" | "nativeint" |"bool"|"loc")
        | %ctyp-{Tokenf.quot}(** BOOTSTRAPING, associated with module [Tokenf] *)
        | %ctyp-{Tokenf.ant} -> res
        | _ ->
            %exp-{let $pat{x.ep0} = ${x.info_exp} in $res }) params' result in
  let mk_tuple params =
    let result = 
      (params |> List.map (fun (x:Ctyp.ty_info) -> x.ep0) |> tuple_com  :> exp) in
    List.fold_right
      (fun (x:Ctyp.ty_info) res ->
        match x.ty with
        | `Lid("int"|"char" | "string" | "int32" | "unit"| "nativeint" |"bool"|"loc")
        | %ctyp-{Tokenf.ant} | %ctyp-{Tokenf.quot} ->  res (* BOOTSTRAPING *)
        | _ ->
            %exp-{let $pat{x.ep0} = ${x.info_exp} in $res }) params result
  in 
  let mk_record _ = assert false in
  gen_stru ~id:(`Pre "") ~mk_tuple ~mk_record ~mk_variant
    ~annot:(fun  x ->
      (* BOOTSTRAPING, associated with module [Astf], [Astfn] *)
      (%ctyp-{ Astf.$lid:x -> Astfn.$lid:x }, %ctyp-{Astfn.$lid:x}))
    
    ();;

Typehook.register
    ~filter:(fun s -> not (List.mem s ["loc"; "ant";"quot"]))
    ("Strip",some gen_strip);;


(*******************************)
(* Fill generator              *) 
(*******************************)
(* [fill_loc] types [loc] and [ant]
   are not processed *)
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
        | `Lid("int" |"char"| "string" | "int32" |"unit"| "nativeint"|"bool" |"loc"|"ant")
        | %ctyp-{Tokenf.ant} | %ctyp-{Tokenf.quot} -> res
        | _ ->
            %exp-{let $pat{x.ep0} = ${x.info_exp} in $res }) params result in
  let mk_tuple params =
    let result = 
      (params |> List.map (fun (x:Ctyp.ty_info) -> x.ep0) |> tuple_com :> exp) in
    List.fold_right
      (fun (x:Ctyp.ty_info) res ->
        match x.ty with
        | `Lid("int"|"char" | "string" | "int32" | "unit" | "nativeint" |"bool"|"loc"|"ant")
        | %ctyp-{Tokenf.ant} | %ctyp-{Tokenf.quot} ->  res
        | _ ->
            %exp-{let $pat{x.ep0} = ${x.info_exp} in $res }) params result in 
  let mk_record _cols = assert false in
  gen_stru
    ~id:(`Pre "") ~mk_tuple
    ~mk_record ~mk_variant
    ~names:["loc"]
    ~annot:(fun x ->
      (%ctyp-{ Locf.t -> Astfn.$lid:x -> Astf.$lid:x },
       %ctyp-{Astf.$lid:x} ))
    ();;

Typehook.register
    ~filter:(fun s -> not (List.mem s ["loc"; "ant";"quot"]))
    ("Fill",some gen_fill);;

  
(*******************************)
(* Meta generator              *) 
(*******************************)
(* only object is used here *)
let mk_variant cons params = 
  let len = List.length params in 
  if Stringf.ends_with cons "Ant" then
    (EpN.of_vstr_number "Ant" len :> exp)
  else
    let params =
      params
         |> List.map (fun  (x:Ctyp.ty_info) -> x.info_exp ) in
    let a = (Expn_util.mee_of_str cons) in
    match params with
    | [] -> a
    | _ -> 
        Expn_util.mee_app a  (Expn_util.mk_tuple_ee params ) (* Fusing ?*)
    
let mk_record cols =
  cols
    |> List.map (fun  (x : Ctyp.record_col)    -> (x.label, x.info.info_exp))
    |> Expn_util.mk_record_ee 

let mk_tuple params =
  params
    |> List.map (fun (x:Ctyp.ty_info) -> x.info_exp)
    |> Expn_util.mk_tuple_ee 

let gen_meta_exp = 
  gen_stru  ~id:(`Pre "meta_")  ~names:["_loc"]
    ~mk_tuple
    ~mk_record ~mk_variant:mk_variant ();;

let gen_meta =
  gen_object ~kind:(Concrete %ctyp-{Astf.ep})
    ~mk_tuple
    ~mk_record
    ~base:"primitive" ~class_name:"meta" ~mk_variant:mk_variant
    ~names:["_loc"]
    ();;


Typehook.register
    ~filter:(fun s -> not (List.mem s ["loc";"ant";"quot"]))
    ("MetaObj", some gen_meta);;
  
(*******************************)
(* Format generator            *) 
(*******************************)
(* [OPrint] unused *)  
let extract info =
  info |> Listf.concat_map (fun (x:Ctyp.ty_info) -> [x.name_exp;(x.id_ep:>exp)] )


let mkfmt pre sep post fields =
  let s =  pre^ String.concat sep fields ^ post in
  %exp-{Format.fprintf fmt $str:s } 
  
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



(*******************************)
(* Iter generator              *) 
(*******************************)
let mk_variant_iter _cons params :exp = 
  match params with
  | [] -> (unit:>exp)
  | _ -> 
      let lst = params
        |> List.map (fun (x:Ctyp.ty_info) -> 
            %exp-{ ${x.name_exp} ${x.id_ep} }) in
        seq_sem lst 

let mk_tuple_iter params : exp =
  mk_variant_iter "" params

let mk_record_iter cols = 
  cols
  |> List.map
    (fun (x:Ctyp.record_col) ->
      %exp-{ ${x.info.name_exp} ${x.info.id_ep} })
  |> seq_sem



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

(*******************************)
(* [Locof] generator           *) 
(*******************************)
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
          %case-{ $vrn:key $pat{(tuple_com pats)} -> _loc } in
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



(********************************************)
(* DynAst generator                         *)
(********************************************)

let generate (mtyps:mtyps) : stru =
  let tys :  string list =
    Listf.concat_map
      (fun x ->
        match x with
        |`Mutual tys -> List.map (fun ((x,_):named_type) -> x ) tys
        |`Single (x,_) -> [x] ) mtyps in
  let typedecl =
    let x  =
      tys
      |> List.map (fun x -> uid  @@ String.capitalize x)
      |> bar_of_list in
    %stru-{ type 'a t = | $x  }in
  let to_string =
    let case =
      bar_of_list
        (List.map
           (fun x ->
             let u = String.capitalize x in
             %case-{ $uid:u -> $str:x }) tys) in 
    %stru-{ let to_string = function | $case  } (* BOOTSTRAPING [Dyn_tag.to_string]*)in
 let  of_string =
   let case =
     tys
     |> List.map
         (fun x -> let u = String.capitalize x in
         %case-{$str:x -> $uid:u })
     |> bar_of_list in
   %stru-{let of_string = function
     | $case  | _ -> failwith (__MODULE__ ^ "." ^ __BIND__  )}
   in
   let tags  =
   List.map
     (fun x->
       let u = String.capitalize x in
       %stru-{let $lid:x :  $lid:x t = $uid:u }) tys  in
       sem_of_list (typedecl::to_string::of_string::tags) ;;
  
Typehook.register
  ~filter:(fun s -> not @@ List.mem s ["loc";"ant";"quot"]) ("DynAst",some generate);;


(********************************************)
(* Map wrapper                              *)
(********************************************)
let generate (mtyps:mtyps) : stru =
  let aux (f:string) : stru  =
    %stru-{
    let $lid{"map_"^f} f = object
      inherit map as super
      method! $lid:f x = f (super#$lid:f x)
    end } in
  stru_from_ty ~f:aux mtyps;;  

Typehook.register
  ~filter:(fun _ -> true) ("MapWrapper",some generate);;




let generate (mtyps:mtyps) : stru =
  let aux (f:string) : stru  =
    %stru-{  (** BOOTSTRAPING, associated with module [Formatf] *)
    let $lid{"dump_"^f}  = Formatf.to_string dump#$lid:f  } in
    sem
      %stru-{let dump = new print}
      (stru_from_ty ~f:aux mtyps);;  


Typehook.register
  ~filter:(fun s -> not (List.mem s ["loc";"ant";"quot"]))
      ("PrintWrapper",some generate);; (* double registration should complain*)


(********************************************)
(* Type wrapper                             *)
(********************************************)

(* remove the loc field *)
let generate (mtyps:mtyps) : stru option = 
  let f (name,ty) =
    if  name <> "ant" then 
     let obj = ObjsN.map_row_field @@ function
       | %row_field-{ $vrn:x of loc } -> %row_field-{ $vrn:x }
       | %row_field-{ $vrn:x of (loc * $y ) }->
           (match y with
           | %ctyp-{ $_ * $_ } -> %row_field-{ $vrn:x of $par:y }
           | _ -> %row_field-{ $vrn:x of $y })
       | x -> x in 
     obj#typedecl ty
  else ty  in
  (fun x ->  stru_from_mtyps ~f x) mtyps;;

Typehook.register ~filter:(fun _ -> true ) ("LocType", generate);;

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/plugins.cmo" *)
(* end: *)
