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
      (* match len with *)
      (* [  *)
      (*  1 ->  {| `Ant (_loc,  $(id:xid 0))  |} *)
      (* | n when n > 1 -> *) (* of_ident_number {:ident| Ant |} len *) of_vstr_number "Ant" len
      (* | _ ->  failwithf "%s can not be handled" cons] *)
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
      (* match len with *)
      (* [  *)
      (*   1 -> {| `Ant (_loc,  $(id:xid 0))  |} *)
      (* | n when n > 1 -> *) of_vstr_number "Ant" len (* of_ident_number {:ident| Ant |} len *)
      (* | _ -> failwithf "%s can not be handled" cons ] *)
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




















(* let anti ~parse_patt ~parse_expr = object *)
(*   inherit Ast.map as super; *)
(*   method! patt = *)
(*     with "patt" *)
(*     fun *)
(*     [ {| $anti:s |} | {| $str:s |} as p -> *)
(*       let mloc _loc = MetaLocQuotation.meta_loc_patt _loc _loc in *)
(*       handle_antiquot_in_string ~s ~default:p ~parse:parse_patt ~loc:_loc *)
(*         ~decorate:(fun n e -> *)
(*           let len = String.length n in  *)
(*           match n with *)
(*           [ "tupexpr" -> {|`ExTup ($(mloc _loc), $e)|} *)
(*           | "seqexpr" -> {|`ExSeq ($(mloc _loc), $e) |} *)
(*           | "uidexpr" -> {| `IdUid ($(mloc _loc), $e) |} (\* use Ant instead *\) *)
(*           | "lidexpr" -> {| `IdLid ($(mloc _loc), $e) |} *)
(*           | "strexpr" -> {| `ExStr ($(mloc _loc), $e) |} *)
(*           | "chrexpr" -> {| `ExChr ($(mloc _loc), $e) |} *)
(*           | "intexpr" -> {| `ExInt ($(mloc _loc), $e) |} *)
(*           | "int32expr" -> {| `ExInt32 ($(mloc _loc), $e) |} *)
(*           | "int64expr" -> {|`ExInt64 ($(mloc _loc), $e)|} *)
(*           | "floexpr" -> {| Ast.ExFlo ($(mloc _loc), $e) |} *)
(*           | "nativeintexpr" -> {|`ExNativeInt ($(mloc _loc), $e) |} *)
(*           | x when (len > 0 && x.[0] = '`') -> failwith (x ^ "is not allowed in pattern") *)
(*           | _ -> e ]) *)
(*       | p -> super#patt p ]; *)
(*     method! expr = with "expr" fun (\* `Ant keeps the right location, `ExStr does not *\) *)
(*       [ {| $anti:s |} | {| $str:s |} as e -> *)
(*           let mloc _loc = MetaLocQuotation.meta_loc_expr _loc _loc in *)
(*           handle_antiquot_in_string ~s ~default:e ~parse:parse_expr ~loc:_loc *)
(*             ~decorate:(fun n e -> (\* e is the parsed Ast node already *\) *)
(*             match n with *)
(*             ["tupexpr" ->   {| `ExTup $(mloc _loc) $e |} *)
(*             | "seqexpr" -> {| `ExSeq $(mloc _loc) $e |} *)
(*             | "uidexpr" -> {| `IdUid $(mloc _loc) $e |} (\* use Ant instead *\) *)
(*             | "lidexpr" -> {| `IdLid $(mloc _loc) $e |} *)
(*             | "strexpr" -> {| `ExStr $(mloc _loc) $e |} *)
(*             | "chrexpr" -> {| `ExChr $(mloc _loc) $e |} *)
(*             | "intexpr" -> {| `ExInt $(mloc _loc) $e |} *)
(*             | "int32expr" -> {| `ExInt32 $(mloc _loc) $e |} *)
(*             | "int64expr" -> {|`ExInt64 $(mloc _loc) $e|} *)
(*             | "floexpr" -> {| Ast.ExFlo $(mloc _loc) $e |} *)
(*             | "nativeintexpr" -> {|`ExNativeInt $(mloc _loc) $e |} *)
(*             | "`nativeintexpr" -> *)
(*                 let e = {| Nativeint.to_string $e |} in *)
(*                 {|`ExNativeInt $(mloc _loc) $e |} *)
(*             | "`intexpr" -> *)
(*                 let e = {|string_of_int $e |} in *)
(*                 {|`ExInt $(mloc _loc) $e |} *)
(*             | "`int32expr" -> *)
(*                 let e = {|Int32.to_string $e |} in *)
(*                 {|`ExInt32 $(mloc _loc) $e |} *)
(*             | "`int64expr" -> *)
(*                 let e = {|Int64.to_string $e |} in *)
(*                 {|`ExInt64 $(mloc _loc) $e |} *)
(*             | "`chrexpr" -> *)
(*                 let e = {|Char.escaped $e|} in *)
(*                 {|`ExChr $(mloc _loc) $e |} *)
(*             | "`strexpr" -> *)
(*                 let e = {|Ast.safe_string_escaped $e |} in *)
(*                 {|`ExStr $(mloc _loc) $e |} *)
(*             | "`floexpr" -> *)
(*                 let e = {| FanUtil.float_repres $e |} in  *)
(*                 {|Ast.ExFlo $(mloc _loc) $e |} *)
(*             | "`boolexpr" -> *)
(*                 let x = {|`IdLid $(mloc _loc) (if $e then "true" else "false" ) |} in *)
(*                 {| {| $(id:$x)  |} |} *)
(*             | "antiexpr" -> {| `Ant $(mloc _loc) $e |} *)
(*             | _ -> e ]) *)
(*       | e -> super#expr e ]; *)
(*   end; *)

                (* {| {| $(id: $({|`IdLid $(mloc _loc) (if $e then "true" else "false" ) |}))  |} |} *)
                  

                  (* {| $(lid:if e then "true" else "false") |} *)
                  (* {| {| $(lid:if $e then "true" else "false") |} |} *)

                  (* {:expr@here|$`bool:x|} *)
                  (*
                    let _ =
  `ExApp
    (_loc,
      (`ExApp
         (_loc,
           (`ExId
              (_loc,
                (`IdAcc
                   (_loc, (`IdUid (_loc, "Ast")),
                     (`IdUid (_loc, "`ExId")))))),
           (`ExId (_loc, (`IdLid (_loc, "_loc")))))),
      (`ExApp
         (_loc,
           (`ExApp
              (_loc,
                (`ExId
                   (_loc,
                     (`IdAcc
                        (_loc, (`IdUid (_loc, "Ast")),
                          (`IdUid (_loc, "`IdLid")))))),
                (`ExId (_loc, (`IdLid (_loc, "_loc")))))),
           (`ExIfe
              (_loc, e, (`ExStr (_loc, "true")),
                (`ExStr (_loc, "false")))))))
                   *)
