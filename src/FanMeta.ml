

(* never used before *)    
(* module MetaLocVar : FanAst.META_LOC= struct *)
(*   let meta_loc _loc _ = {:pat| $(lid:!FanLoc.name) |}; *)
(*   (\* let meta_loc_pat _loc _ = {:pat| $(lid:!FanLoc.name) |}; *\) *)
(*   (\* let meta_loc_exp _loc _ = {:exp| $(lid:!FanLoc.name) |}; *\) *)
(* end; *)
    
(* module MetaLoc *)

(* module MetaGhostLoc *)

(* module MetaLoc : FanAst.META_LOC= struct *)
(*   (\* FIXME *\) *)
(*   (\* let meta_loc_pat _loc _location = *\) *)
(*   (\*   failwith  "MetaLoc.meta_loc_pat not implemented yet"  ; *\) *)
(*     (\* let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple location in *\) *)
(*     (\* {:pat| FanLoc.of_tuple *\) *)
(*     (\*   ($`str:a, $`int:b, $`int:c, $`int:d, *\) *)
(*     (\*    $`int:e, $`int:f, $`int:g, *\) *)
(*     (\*    $(if h then {:pat| true |} else {:pat| false |} )) |}; *\) *)
(*     (\* *)
(*   Ast.App *)
(*     (_loc, *)
(*       (Ast.PaId *)
(*          (_loc, *)
(*            (Ast.IdAcc *)
(*               (_loc, (Ast.Uid (_loc, "FanLoc")), *)
(*                 (Ast.Lid (_loc, "of_tuple")))))), *)
(*       (Ast.PaTup *)
(*          (_loc, *)
(*            (Ast.PaCom *)
(*               (_loc, (Ast.Str (_loc, (Ast.safe_string_escaped a))), *)
(*                 (Ast.PaCom *)
(*                    (_loc, *)
(*                      (Ast.PaCom *)
(*                         (_loc, *)
(*                           (Ast.PaCom *)
(*                              (_loc, *)
(*                                (Ast.PaCom *)
(*                                   (_loc, *)
(*                                     (Ast.PaCom *)
(*                                        (_loc, *)
(*                                          (Ast.PaCom *)
(*                                             (_loc, *)
(*                                               (Ast.Int *)
(*                                                  (_loc, (string_of_int b))), *)
(*                                               (Ast.Int *)
(*                                                  (_loc, (string_of_int c))))), *)
(*                                          (Ast.Int (_loc, (string_of_int d))))), *)
(*                                     (Ast.Int (_loc, (string_of_int e))))), *)
(*                                (Ast.Int (_loc, (string_of_int f))))), *)
(*                           (Ast.Int (_loc, (string_of_int g))))), *)
(*                      (if h *)
(*                       then Ast.PaId (_loc, (Ast.Lid (_loc, "true"))) *)
(*                       else Ast.PaId (_loc, (Ast.Lid (_loc, "false"))))))))))) *)
(*      *\) *)
(* end; *)

let meta_loc _loc location =
  let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple location in
  {:exp'| FanLoc.of_tuple
    ($`str:a, $`int:b, $`int:c, $`int:d,
     $`int:e, $`int:f, $`int:g,
     $(if h then {:exp'| true |} else {:exp'| false |} )) |}
  



open Ast
  
#default_quotation "ep";;
class primitive =  object
  method int _loc (i:int) : ep =  {|$`int:i|};
  method int32 _loc (i:int32) : ep = {|$`int32:i|};
  method int64 _loc  (i:int64) : ep = {|$`int64:i|};
  method nativeint _loc (i:nativeint) : ep = {|$`nativeint:i|};
  method float _loc (i:float) : ep= {|$`flo:i|};
  method string _loc (i:string): ep = {|$`str:i|};
  method char _loc (i:char) : ep = {|$`chr:i|};
  method unit _loc (_:unit) : ep = {|()|};
  (*default use [meta_loc] for expession*)   
  method loc _loc (_l:loc) : ep=
    `Lid (_loc, !FanLoc.name);
  method ant (_loc:loc) (x:ant) : ep = (x:>ep) ;  
  (* FIXME bool antiquot *)
  method bool _loc x  : ep=
    match x with
    |true -> {|true|} | false -> {| false |} ;
  (* method unknown (_loc:loc) : ! 'a . 'a -> ep  = assert false;
     method unknown (_loc : loc) = (assert false : 'a . 'a -> ep )
     a bug to be FIXED
   *)
end;;

{:fans|keep off; derive (MetaObj); |};;

{:ocaml|{:include| "src/Ast.mli"|} |};;

  
