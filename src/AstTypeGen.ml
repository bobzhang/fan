
open FanAst;
open LibUtil;
open Easy;

open FSig;
open Lib;
open Lib.Expr;


let _loc = FanLoc.ghost;


(* +-----------------------------------------------------------------+
   | Eq generator                                                    |
   +-----------------------------------------------------------------+ *)

let mk_variant_eq _cons : list FSig.ty_info  -> expr  = with expr fun 
  [ [] -> {|true|}
  | ls -> List.reduce_left_with
        ~compose:(fun x y -> {| $x && $y|}  )
        ~f:(fun [{(* FSig. *)expr;_} -> expr]) ls ];
  
let mk_tuple_eq exprs = mk_variant_eq "" exprs ;
let mk_record_eq : list FSig.record_col -> expr  = fun cols -> 
    cols |> List.map (fun [ {re_info;_} -> re_info])
         |> mk_variant_eq "" ;
    
let (gen_eq,gen_eqobj) = with expr
  (gen_str_item ~id:(`Pre "eq_")  ~names:[]
    ~arity:2
    ~mk_tuple:mk_tuple_eq
    ~mk_record:mk_record_eq
    ~mk_variant:mk_variant_eq
    ~trail: {|false|} (),
   gen_object ~kind:Iter ~mk_tuple:mk_tuple_eq ~mk_record:mk_record_eq
     ~base:"eqbase" ~class_name:"eq"
     ~mk_variant:mk_variant_eq ~names:[]
     ~arity:2 ~trail: {|false|} ()) ;
  
[ ("Eq",gen_eq) ; ("OEq",gen_eqobj) ] |> List.iter Typehook.register;


(* +-----------------------------------------------------------------+
   | Fold generator                                                  |
   +-----------------------------------------------------------------+ *)


let (gen_fold,gen_fold2) = with expr
  let mk_variant _cons params = 
    params
    |> List.map (fun [{expr;_} -> expr])
    |> (fun
        [ [] -> {|self|}
        | ls ->
            List.reduce_right (fun v acc -> {| let self = $v in $acc |}) ls ]) in
  let mk_tuple  = mk_variant ""  in 
  let mk_record cols =
    cols |> List.map (fun [ {re_info ; _ } -> re_info ] )
         |> mk_variant "" in 
  (gen_object ~kind:Fold ~mk_tuple ~mk_record
     ~base:"foldbase" ~class_name:"fold" ~mk_variant ~names:[] (),
   gen_object ~kind:Fold ~mk_tuple ~mk_record
     ~base:"foldbase2" ~class_name:"fold2"
     ~mk_variant ~names:[]
     ~arity:2 ~trail: {|invalid_arg "fold2 failure" |} () ) ;
begin  
   [("Fold",gen_fold);
    ("Fold2",gen_fold2);] |> List.iter Typehook.register;
end;

(* +-----------------------------------------------------------------+
   | Map generator                                                   |
   +-----------------------------------------------------------------+ *)


let (gen_map,gen_map2) = with expr
  let mk_variant cons params =
    let result =
      appl_of_list
        [ (EP.of_str cons) ::
          params |> List.map (fun [{exp0;_} -> exp0]) ] in 
    List.fold_right
      (fun {expr;pat0;_} res ->
              {|let $pat:pat0 = $expr in $res |})  params result in
  let mk_tuple params =
    let result = 
      params |> List.map (fun [{exp0; _ } -> exp0]) |> tuple_com in
    List.fold_right
      (fun {expr;pat0;_} res ->
        {| let $pat:pat0 = $expr in $res |}) params result in 
  let mk_record cols =
    (* (->label,info.exp0) *)
    let result = 
    cols |> List.map
      (fun [ {re_label; re_info=({exp0;_ } as info) ; _ }  ->
        let _ = Obj.repr info in
        (re_label,exp0) ] )  |> mk_record   in
    List.fold_right
      (fun {re_info={expr;pat0;_};_} res ->
        {|let $pat:pat0 = $expr in $res |}) cols result in
  (gen_object ~kind:Map ~mk_tuple ~mk_record
     ~base:"mapbase" ~class_name:"map"
     ~mk_variant ~names:[] (),
   gen_object ~kind:Map ~mk_tuple ~mk_record
     ~base:"mapbase2" ~class_name:"map2" ~mk_variant ~names:[]
     ~arity:2 ~trail: {|  invalid_arg "map2 failure" |} ());

begin
  [("Map",gen_map);
   ("Map2",gen_map2);]
  |> List.iter Typehook.register;
end;

(* +-----------------------------------------------------------------+
   | Strip generator                                                 |
   +-----------------------------------------------------------------+ *)
(* FIXME to be more elegant *)  
let gen_strip = with {patt:ctyp;expr}
  let mk_variant cons params =
    let params' = (List.filter
               (fun [{ty={|loc|};_} -> false | _  -> true])
               params) in
    let result =
      appl_of_list
         [(EP.of_str cons) :: params' |> List.map (fun [{exp0;_} -> exp0]) ]  in 
    List.fold_right
      (fun {expr;pat0;ty;_} res ->
        match ty with
        [ {|int|} | {|string |} |{|int32|} | {|nativeint|} | {|loc|} |
        {|list string|} |  {|FanUtil.anti_cxt |} | {| meta_list string|} ->
          res
        | _ -> {|let $pat:pat0 = $expr in $res |}] 
              )  params' result in
  let mk_tuple params =
    let result = 
      params |> List.map (fun [{exp0; _ } -> exp0]) |> tuple_com in
    List.fold_right
      (fun {expr;pat0;ty;_} res ->
        match ty with
        [ {|int|} | {|string |} |{|int32|} | {|nativeint|} |
        {|loc|} | {|list string|} |  {| FanUtil.anti_cxt |} | {| meta_list string|}->
          res
        | _ -> {|let $pat:pat0 = $expr in $res |}]) params result in 
  let mk_record cols =
    let result = 
    cols |> List.map (fun [ {re_label; re_info={exp0;_ } ; _ }  ->
          (re_label,exp0) ] )  |> mk_record   in
    List.fold_right
      (fun {re_info={expr;pat0;ty;_};_} res ->
        match ty with
        [ {|int|} | {|string |} |{|int32|} | {|nativeint|} | {|loc|}
        | {|list string|}  | {|FanUtil.anti_cxt|} | {| meta_list string|} ->
          res
        | _ -> {|let $pat:pat0 = $expr in $res |}]) cols result in
  gen_str_item ~id:(`Pre "strip_loc_") ~mk_tuple ~mk_record ~mk_variant ~names:[] ();
Typehook.register
    ~filter:(fun s -> not (List.mem s ["loc"; "ant"(* ; "meta_option"; "meta_list" *)]))
    ("Strip",gen_strip);


  
  
(* +-----------------------------------------------------------------+
   | Meta generator                                                  |
   +-----------------------------------------------------------------+ *)
  
let mk_variant_meta_expr cons params = with expr
    let len = List.length params in 
    if String.ends_with cons "Ant" then
      EP.of_vstr_number "Ant" len
    else
      params
      |> List.map (fun [ {expr;_} -> expr ])
      |> List.fold_left mee_app (mee_of_str cons)  ;
        
let mk_record_meta_expr cols = cols |> List.map
  (fun [ {re_label; re_info={expr;_};_} -> (re_label, expr)]) |> mk_record_ee ;

let mk_tuple_meta_expr params =
    params |> List.map (fun [{expr;_} -> expr]) |> mk_tuple_ee ;

let gen_meta_expr = 
  gen_str_item  ~id:(`Pre "meta_")  ~names:["_loc"]
    ~mk_tuple:mk_tuple_meta_expr
    ~mk_record:mk_record_meta_expr ~mk_variant:mk_variant_meta_expr ();

(* add hock FIXME*)  
Typehook.register
    ~position:"__MetaExpr__"
    ~filter:(fun s -> s<>"loc")
    ("MetaExpr",gen_meta_expr);


(* +-----------------------------------------------------------------+
   | Format generator                                                |
   +-----------------------------------------------------------------+ *)
  
let extract info = info
    |> List.map (fun [{name_expr;id_expr;_} -> [name_expr;id_expr] ])
    |> List.concat ;

let mkfmt pre sep post fields = with expr
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
  gen_str_item  ~id:(`Pre "pp_print_")  ~names:["fmt"] 
    ~mk_tuple:mk_tuple_print  ~mk_record:mk_record_print
    ~mk_variant:mk_variant_print ()
    ;    

let gen_print_obj =
  gen_object ~kind:Iter ~mk_tuple:mk_tuple_print
    ~base:"printbase" ~class_name:"print"
    ~names:["fmt"]  ~mk_record:mk_record_print
    ~mk_variant:mk_variant_print ();

[("Print",gen_print);
 ("OPrint",gen_print_obj)] |> List.iter Typehook.register;

(* +-----------------------------------------------------------------+
   | Iter geneartor                                                  |
   +-----------------------------------------------------------------+ *)
let mk_variant_iter _cons params :expr = with expr
  let lst = params
   |> List.map (fun [{name_expr; id_expr;_} -> {| $name_expr $id_expr |}]) in
  {| begin $list:lst end |};

let mk_tuple_iter params : expr =
  mk_variant_iter "" params;

let mk_record_iter cols = with expr
  let lst =
    cols |>
    List.map
    (fun [{re_info={name_expr; id_expr;_};_} -> {| $name_expr $id_expr |}]) in
  {|begin $list:lst end |};

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

let generate (module_types:FSig.module_types) = with str_item 
  let tbl = Hashtbl.create 30 in
  let aux (_,ty) =
    match ty with
    [`TyDcl(_,_,_,`TyVrnEq(_,t),_) ->
      let branches = Ctyp.view_variant t in
      List.iter
        (fun
          [`variant (s,ls) ->
            let arity = List.length ls in
            let try v = Hashtbl.find tbl s in
            if v = arity then
              ()
            else
              failwithf "%s has diffireent arities" s
            with 
              [Not_found -> Hashtbl.add tbl s arity]
          | _ -> ()]) branches
    | _ ->
        FanLoc.errorf
          (loc_of ty) "generate module_types %s"
          (dump_ctyp ty) ] in   
  let _ = List.iter
      (fun
        [`Mutual tys ->
          List.iter aux tys
         |`Single t -> aux t]) module_types in
  let case = Hashtbl.fold
    (fun key arity acc ->
      
      if arity= 1 then 
        {:match_case| $vrn:key _loc  -> _loc | $acc |}
      else if arity > 1 then 
        let pats =
          [ {:patt| _loc|} :: List.init (arity - 1) (fun _ -> {:patt| _ |}) ] in
        {:match_case| $vrn:key $(pat:(tuple_com pats)) -> _loc | $acc |}
      else failwithf "arity=0 key:%s" key
        
    ) tbl {||} in
  {| let loc_of  = fun [ $case ]|};
Typehook.register
    ~filter:(fun s -> not (List.mem s ["loc"; "meta_option";"meta_list"]))
    ("GenLoc",generate);


(* +-----------------------------------------------------------------+
   | Type Generator                                                  |
   +-----------------------------------------------------------------+ *)
(* remove the loc field *)
let generate (module_types:FSig.module_types) : str_item = with str_item
  let aux (name,ty) =
    if not (name ="ant") then 
     with ctyp
  (* use [map_ctyp] instead  *)
     let obj = object
       inherit FanAst.map as super;
       method! ctyp = 
         (fun 
          [ {:ctyp| $vrn of loc |} -> {:ctyp|$vrn |}
          (* | {| ant |} -> {||} *)
          | {| $vrn of (loc * $x )|} ->
              match x with
              [ {| $x*$y|} ->   {| $vrn of ( $x * $y) |}
              | _ -> {| $vrn of $x |}]
          | x -> super#ctyp x ]);
     end in
     obj#ctyp ty
  else ty  in
  (fun x -> let r = FSigUtil.str_item_from_module_types ~f:aux x  in r
  (* {:str_item| module N = struct $r end |} *)) module_types;

Typehook.register ~filter:(fun _ -> true (* not (List.mem s ["loc"; "ant"]) *)) ("LocType",generate);