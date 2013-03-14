(**
   reference:
   [printtyp.ml] [oprint.ml]
   the main function [tree_of_typexp]
   Compiler dump [Types.signature] to [out_sig_item] and then
   print [out_sig_item]

 *)

(*
  [Types.signature] -> [typedecl]
 *)
open LibUtil;
open Types;
open AstLoc;

let _loc = FanLoc.ghost ;

exception CtypNotSupport of type_desc;
  
let rec signature_item (x:Types.signature_item)  : option typedecl =
  match  x with
  [ Sig_value _
  | Sig_exception _
  | Sig_module _
  | Sig_class _
  | Sig_modtype _
  | Sig_class_type _ -> None
        (* copied from [printtyp] *)
  | Sig_type (id,_,_) when Btype.is_row_name (Ident.name id) -> None

  | Sig_type (id,td,_rs) ->
      (* per single rs, the first is always [Trec_first]
         [Trec_not] seems to be used in [Recursive modules]
       *)
      try Some (type_declaration id td) with [_ -> None] ]

(* constraint is removed in typedtree *)      
and type_declaration id
    {type_params;
     (* type_arity; = List.length type_params *)
     type_kind;
     type_private;
     type_manifest;
     (* type_variance; ignore *)
     (* type_newtype_level; *)
     _
   } : typedecl  =
  let name = Ident.name id in 
  let params = List.map type_expr type_params in
  let private_flag =
    match type_private with
    [Asttypes.Private -> `Private _loc  | Asttypes.Public -> `PrNil _loc ] in
  let manifest =
    Option.map type_expr type_manifest in 
  match (type_kind,manifest) with
  [(Type_abstract,None) -> (* type float u *)
    `TyAbstr(_loc,`Lid(_loc,name),params,[])
  |(Type_abstract,Some x) ->  (* type u = int *)
     `TyDcl
      (_loc,
       `Lid(_loc,name), params,
       `TyEq(_loc,private_flag,x),[])
  |(Type_record (xs,_float),Some x) ->
      `TyDcl
        (_loc,`Lid(_loc,name),params,
         `TyMan
           (_loc,x, private_flag,
            `Record(_loc,type_record xs )
               ),[])
  | (Type_record (xs,_float),None) ->
      `TyDcl(_loc,`Lid(_loc,name),params,
            `TyRepr(_loc,private_flag,`Record(_loc,type_record xs)),[])

  |(Type_variant xs,Some x)  ->
      `TyDcl
        (_loc,`Lid(_loc,name),params,
         `TyMan
           (_loc,x,private_flag, `Sum(_loc,type_sum xs)),[])
  |(Type_variant xs,None) ->
      `TyDcl
        (_loc,`Lid(_loc,name),params,
         `TyRepr(_loc,private_flag,`Sum(_loc,type_sum xs)),
         [])]

and type_record (xs: list (Ident.t * Asttypes.mutable_flag * type_expr) ) : name_ctyp=
  sem_of_list &
  List.map
    (fun (i,m,e) ->
      let name = Ident.name i in
      match m with
      [Asttypes.Mutable ->  
        `TyColMut(_loc,`Id(_loc,`Lid(_loc,name)),type_expr e)
      |Asttypes.Immutable ->
        `TyCol(_loc,`Id(_loc,`Lid(_loc,name)),type_expr e)]) xs 
and type_sum (xs: list (Ident.t *  list type_expr *  option type_expr) ) : or_ctyp
    =
  or_of_list &
  List.map
    (fun
      [(i,xs,None) ->
        let name = Ident.name i in
        match xs with
        [[] -> `Id(_loc,`Lid(_loc,name))
        |[x] -> `Of(_loc,`Id(_loc,`Lid(_loc,name)),type_expr x)
        | _ ->
            let tys =  sta_of_list & List.map type_expr xs in
            `Of(_loc,`Id(_loc,`Lid(_loc,name)),tys)]  
      |(_i,_xs,Some _x) -> failwithf "type_sum  for gadt not supported yet"]) xs

and id_path (p:Path.t) : ident  =
  match p with
  [Path.Pident x -> `Lid(_loc,Ident.name x)
  |Path.Pdot(a,x,_depth) -> `Dot(_loc,id_path a,`Lid(_loc,x))
  |Path.Papply(a,b) ->
      `App(_loc, id_path a, id_path b) ]  
and type_expr ({desc ;_} : Types.type_expr) : ctyp =
  match desc with
  [ Tvar opt
  | Tunivar opt -> 
    match opt with
    [Some x ->
       `Quote(_loc,`Normal _loc, `Lid(_loc,x))
    |None ->
        `QuoteAny(_loc,`Normal _loc)]
  | Ttuple ls ->
      tup & sta_of_list & (List.map type_expr ls)
  | Tconstr (path,ls,_ref) ->
      match ls with
      [[] ->
        `Id(_loc,id_path path)
      | _ ->
          appl_of_list [ `Id(_loc,id_path path) :: List.map type_expr ls]]  
  | Tvariant _  
  (* [fatal_error] *)
  | Tpoly _      
  | Tpackage _
  | Tlink _
  | Tfield _ 
  | Tnil
  | Tsubst _      
  | Tobject _          
  | Tarrow _ as x -> raise &  CtypNotSupport x 
 ]
      ;
let signature (sg:Types.signature) =
  List.map signature_item sg;

  
  
