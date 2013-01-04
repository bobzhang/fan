
open Ast;
open LibUtil;
open Easy;

open FSig;
open Lib.Expr;
open Lib.Basic;

let _loc = FanLoc.ghost;


(* +-----------------------------------------------------------------+
   | Eq generator                                                    |
   +-----------------------------------------------------------------+ *)

let mk_variant_eq _cons : list FSig.ty_info  -> (* Ast. *)expr  = with "expr" fun 
  [ [] -> {|true|}
  | ls -> List.reduce_left_with
        ~compose:(fun x y -> {| $x && $y|}  )
        ~f:(fun [{(* FSig. *)expr;_} -> expr]) ls ];
  
let mk_tuple_eq exprs = mk_variant_eq "" exprs ;
let mk_record_eq : list FSig.record_col -> (* Ast. *)expr  = fun cols -> 
    cols |> List.map (fun [ {(* FSig. *)info;_} -> info])
         |> mk_variant_eq "" ;
    
let gen_eq = with "expr"
  gen_str_item ~id:(`Pre "eq_")  ~names:[]
    ~arity:2
    ~mk_tuple:mk_tuple_eq
    ~mk_record:mk_record_eq
    mk_variant_eq
    ~trail: {|false|} ;
  
[ ("Eq",gen_eq) ; ] |> List.iter Typehook.register;


(* +-----------------------------------------------------------------+
   | Fold generator                                                  |
   +-----------------------------------------------------------------+ *)


let (gen_fold,gen_fold2) = with "expr"
  let mk_variant _cons params = 
    params
    |> List.map (fun [{expr;_} -> expr])
    |> (fun
        [ [] -> {|self|}
        | ls -> List.reduce_right (fun v acc -> {| let self = $v in $acc |}) ls ]) in
  let mk_tuple  = mk_variant ""  in 
  let mk_record cols =
    cols |> List.map (fun [ {(* label; *) info ; _ } -> info ] )
         |> mk_variant "" in 
  (gen_object ~kind:Fold ~mk_tuple ~mk_record
     ~base:"foldbase" ~class_name:"fold" mk_variant ~names:[],
   gen_object ~kind:Fold ~mk_tuple ~mk_record
     ~base:"foldbase2" ~class_name:"fold2"
     mk_variant ~names:[]
     ~arity:2 ~trail: {|invalid_arg "fold2 failure" |} ) ;
begin  
   [("Fold",gen_fold);
    ("Fold2",gen_fold2);] |> List.iter Typehook.register;
end;

(* +-----------------------------------------------------------------+
   | Map generator                                                   |
   +-----------------------------------------------------------------+ *)


let (gen_map,gen_map2) = with "expr"
  let mk_variant cons params =
    params |> List.map (fun [ {expr;_} -> expr]) |> apply (of_str cons) in
  let mk_tuple params =
    params |> List.map (fun [{expr; _ } -> expr]) |> tuple_of_list in 
  let mk_record cols =
    cols |> List.map (fun [ {label; info={expr;_ } ; _ }  ->
          (label,expr) ] )  |> mk_record   in
  (gen_object ~kind:Map ~mk_tuple ~mk_record
     ~base:"mapbase" ~class_name:"map"
     mk_variant ~names:[],
   gen_object ~kind:Map ~mk_tuple ~mk_record
     ~base:"mapbase2" ~class_name:"map2" mk_variant ~names:[]
     ~arity:2 ~trail: {|  invalid_arg "map2 failure" |} );

begin
  [("Map",gen_map);
   ("Map2",gen_map2);]
  |> List.iter Typehook.register;
end;

  
  
(* +-----------------------------------------------------------------+
   | Meta generator                                                  |
   +-----------------------------------------------------------------+ *)
  
let mk_variant_meta_expr cons params = with "expr"
    let len = List.length params in 
    if String.ends_with cons "Ant" then
      match len with
      [ n when n > 1 -> of_ident_number {:ident| (* Ast. *)ExAnt |} len
      | 1 ->  {| (* Ast. *)ExAnt _loc  $(id:xid 0)  |}
      | _ ->  failwithf "%s can not be handled" cons]
    else
      params
      |> List.map (fun [ {expr;_} -> expr ])
      |> List.fold_left mee_app (mee_of_str cons)  ;
        
let mk_record_meta_expr cols = cols |> List.map
  (fun [ {label; info={expr;_};_} -> (label, expr)]) |> mk_record_ee ;

let mk_tuple_meta_expr params =
    params |> List.map (fun [{expr;_} -> expr]) |> mk_tuple_ee ;

let gen_meta_expr = 
  gen_str_item  ~id:(`Pre "meta_")  ~names:["_loc"]
    ~mk_tuple:mk_tuple_meta_expr
    ~mk_record:mk_record_meta_expr mk_variant_meta_expr;
    (* ~module_name:"MetaExpr";     *)

let mk_variant_meta_patt cons params = with "expr"
    let len = List.length params in 
    if String.ends_with cons "Ant" then
      match len with
      [ n when n > 1 -> of_ident_number {:ident| (* Ast. *)PaAnt |} len
      | 1 -> {| (* Ast. *)PaAnt _loc  $(id:xid 0)  |}
      | _ -> failwithf "%s can not be handled" cons ]
    else
      params
      |> List.map (fun [ {expr;_} -> expr ])
      |> List.fold_left mep_app (mep_of_str cons);
        
let mk_record_meta_patt cols = cols |> List.map
      (fun [ {label; info={expr;_};_}
             -> (label, expr)])
         |> mk_record_ep ;

let mk_tuple_meta_patt params = params |> List.map
      (fun [{expr;_} -> expr]) |> mk_tuple_ep;

let gen_meta_patt =
  gen_str_item  ~id:(`Pre "meta_")
    ~names:["_loc"]
    ~mk_tuple:mk_tuple_meta_patt
    ~mk_record:mk_record_meta_patt
    mk_variant_meta_patt
    (* ~module_name:"MetaPatt" *)
;

(* add hock FIXME*)  
Typehook.register
    ~position:"__MetaExpr__"
    ~filter:(fun s -> s<>"loc")
    ("MetaExpr",gen_meta_expr);
Typehook.register
    ~position:"__MetaPatt__"
    ~filter:(fun s -> s<> "loc")
    ("MetaPatt",gen_meta_patt);
(* begin  [ *)
(*    ("MetaExpr",gen_meta_expr) ; *)
(*    ("MetaPatt",gen_meta_patt) ;] |> List.iter Typehook.register; *)
(* end ; *)

(* +-----------------------------------------------------------------+
   | FMap  generator                                                 |
   +-----------------------------------------------------------------+ *)


let (gen_map,gen_map2) = with "expr"
  let mk_variant cons params =
    params
    |> List.map (fun [ {expr;_} -> expr])
    |> apply (of_str ("`"^cons)) in
  let mk_tuple params =
    params |> List.map (fun [{expr; _ } -> expr]) |> tuple_of_list in 
  let mk_record cols =
    cols
    |> List.map (fun [ {label; info={expr;_ } ; _ }  ->
          (label,expr) ] )
    |> mk_record   in
  (gen_object
     ~kind:Map
     ~mk_tuple
     ~mk_record
     ~cons_transform:(fun x -> "`"^x)
     ~base:"mapbase"
     ~class_name:"map"
     mk_variant ~names:[],
   gen_object
     ~kind:Map
     ~mk_tuple
     ~mk_record
     ~cons_transform:(fun x -> "`"^x)
     ~base:"mapbase2" ~class_name:"map2" mk_variant ~names:[]
     ~arity:2 ~trail: {|  invalid_arg "map2 failure" |} );

begin
  [("FMap",gen_map);
   ("FMap2",gen_map2);]
  |> List.iter Typehook.register;
end;


(* +-----------------------------------------------------------------+
   | FFold generator                                                 |
   +-----------------------------------------------------------------+ *)


let (gen_fold,gen_fold2) = with "expr"
  let mk_variant _cons params = 
    params
    |> List.map (fun [{expr;_} -> expr])
    |> (fun
        [ [] -> {|self|}
        | ls -> List.reduce_right (fun v acc -> {| let self = $v in $acc |}) ls ]) in
  let mk_tuple  = mk_variant ""  in 
  let mk_record cols =
    cols |> List.map (fun [ { info ; _ } -> info ] )
         |> mk_variant "" in 
  (gen_object
     ~kind:Fold
     ~mk_tuple
     ~mk_record
     ~cons_transform:(fun x -> "`"^x)
     ~base:"foldbase"
     ~class_name:"fold"
     mk_variant ~names:[],
   gen_object
     ~kind:Fold
     ~mk_tuple
     ~mk_record
     ~cons_transform:(fun x -> "`"^x)
     ~base:"foldbase2"
     ~class_name:"fold2"
     mk_variant
     ~names:[]
     ~arity:2
     ~trail:{|invalid_arg "fold2 failure" |} ) ;
begin  
   [("FFold",gen_fold);
    ("FFold2",gen_fold2);] |> List.iter Typehook.register;
end;
  
(* +-----------------------------------------------------------------+
   | Meta2 generator                                                 |
   +-----------------------------------------------------------------+ *)

let mk_variant_meta_expr cons params = with "expr"
    let len = List.length params in 
    if String.ends_with cons "Ant" then
      match len with
      [ 
       1 ->  {| `ExAnt (_loc,  $(id:xid 0))  |}
      | n when n > 1 -> (* of_ident_number {:ident| ExAnt |} len *) of_vstr_number "ExAnt" len
      | _ ->  failwithf "%s can not be handled" cons]
    else
      params
      |> List.map (fun [ {expr;_} -> expr ])
      |> List.fold_left (* mee_app *)vee_app ((* mee_of_str *)vee_of_str cons)  ;
        
let mk_record_meta_expr cols = cols |> List.map
  (fun [ {label; info={expr;_};_} -> (label, expr)]) |> mk_record_ee ;

let mk_tuple_meta_expr params =
    params |> List.map (fun [{expr;_} -> expr]) |> (* mk_tuple_ee *) mk_tuple_vee;

let gen_meta_expr = 
  gen_str_item  ~id:(`Pre "meta_")  ~names:["_loc"]
    ~cons_transform:(fun x -> "`"^x)
    ~mk_tuple:mk_tuple_meta_expr
    ~mk_record:mk_record_meta_expr
    mk_variant_meta_expr;
    (* ~module_name:"MetaExpr";     *)

let mk_variant_meta_patt cons params = with "expr"
    let len = List.length params in 
    if String.ends_with cons "Ant" then
      match len with
      [ 
        1 -> {| `PaAnt (_loc,  $(id:xid 0))  |}
      | n when n > 1 -> of_vstr_number "PaAnt" len (* of_ident_number {:ident| PaAnt |} len *)
      | _ -> failwithf "%s can not be handled" cons ]
    else
      params
      |> List.map (fun [ {expr;_} -> expr ])
      |> List.fold_left (* mep_app *) vep_app ((* mep_of_str *)vep_of_str cons);
        
let mk_record_meta_patt cols = cols |> List.map
      (fun [ {label; info={expr;_};_}
             -> (label, expr)])
         |> mk_record_ep ;

let mk_tuple_meta_patt params = params |> List.map
      (fun [{expr;_} -> expr]) |> (* mk_tuple_ep *) mk_tuple_vep;

let gen_meta_patt =
  gen_str_item  ~id:(`Pre "meta_")
    ~cons_transform:(fun x -> "`"^x )
    ~names:["_loc"]
    ~mk_tuple:mk_tuple_meta_patt
    ~mk_record:mk_record_meta_patt
    mk_variant_meta_patt
    (* ~module_name:"MetaPatt" *)
;

(* add hock FIXME*)  
Typehook.register
    ~position:"__MetaExpr__"
    ~filter:(fun s -> s<>"loc")
    ("MetaExpr2",gen_meta_expr);
Typehook.register
    ~position:"__MetaPatt__"
    ~filter:(fun s -> s<> "loc")
    ("MetaPatt2",gen_meta_patt);
  

(* +-----------------------------------------------------------------+
   | Format generator                                                |
   +-----------------------------------------------------------------+ *)
  
let extract info = info
    |> List.map (fun [{name_expr;id_expr;_} -> [name_expr;id_expr] ])
    |> List.concat ;

let mkfmt pre sep post fields = with "expr"
    {| Format.fprintf fmt  $(str: pre^ String.concat sep fields ^ post) |} ;
  
let mk_variant_print cons params =
    let len = List.length params in
    let pre =
        if len >= 1 then
          mkfmt ("@[<1>("^cons^"@ ")
            "@ " ")@]" (List.init len (fun _ -> "%a"))
        else
          mkfmt cons "" "" [] in
    params |> extract |> apply pre ;
    
let mk_tuple_print params =
    let len = List.length params in
    let pre = mkfmt "@[<1>(" ",@," ")@]" (List.init len (fun _ -> "%a")) in 
    params |> extract |> apply pre  ;
    
let mk_record_print cols = 
    let pre = cols
       |> List.map (fun [ {label;_} -> label^":%a" ])
       |>  mkfmt "@[<hv 1>{" ";@," "}@]" in 
    cols |> List.map(fun [ {info;_} -> info ])
         |> extract |> apply pre  ;
  
let gen_print =
  gen_str_item  ~id:(`Pre "pp_print_")  ~names:["fmt"] 
    ~mk_tuple:mk_tuple_print  ~mk_record:mk_record_print   mk_variant_print;    

let gen_print_obj =
  gen_object ~kind:Iter ~mk_tuple:mk_tuple_print
    ~base:"printbase" ~class_name:"print"
    ~names:["fmt"]  ~mk_record:mk_record_print mk_variant_print;

[("Print",gen_print);
 ("OPrint",gen_print_obj)] |> List.iter Typehook.register;

(* +-----------------------------------------------------------------+
   | FFormat generator                                               |
   +-----------------------------------------------------------------+ *)

let mk_variant_print cons params =
    let len = List.length params in
    let pre =
        if len >= 1 then
          mkfmt ("@[<1>("^cons^"@ ")
            "@ " ")@]" (List.init len (fun _ -> "%a"))
        else
          mkfmt cons "" "" [] in
    params |> extract |> apply pre ;
    
let mk_tuple_print params =
    let len = List.length params in
    let pre = mkfmt "@[<1>(" ",@," ")@]" (List.init len (fun _ -> "%a")) in 
    params |> extract |> apply pre  ;
    
let mk_record_print cols = 
    let pre = cols
       |> List.map (fun [ {label;_} -> label^":%a" ])
       |>  mkfmt "@[<hv 1>{" ";@," "}@]" in 
    cols |> List.map(fun [ {info;_} -> info ])
         |> extract |> apply pre  ;
  
let gen_print =
  gen_str_item
    ~id:(`Pre "pp_print_")
    ~names:["fmt"] 
    ~mk_tuple:mk_tuple_print
    ~mk_record:mk_record_print
    ~cons_transform:(fun x -> "`"^x)
    mk_variant_print;    

let gen_print_obj =
  gen_object
    ~kind:Iter
    ~mk_tuple:mk_tuple_print
    ~base:"printbase"
    ~class_name:"print"
    ~cons_transform:(fun x -> "`"^x)
    ~names:["fmt"]
    ~mk_record:mk_record_print
    mk_variant_print;

[("FPrint",gen_print);
 ("FOPrint",gen_print_obj)] |> List.iter Typehook.register;

  























(* (with "expr" {| {|`a (3,4)|} |}, *)
(* with "expr" {| {| A(3,4)|} |}) *)

