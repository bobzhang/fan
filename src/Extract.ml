(**
   reference:
   [printtyp.ml]   
   Compiler dump [Types.signature] to [out_sig_item] and then
   print [out_sig_item]

 *)

(*
  [Types.signature] -> [typedecl]
 *)
open LibUtil;
open Types;

let _loc = FanLoc.ghost ;


let rec signature_item (x:Types.signature_item)  =
  match  x with
  [ Sig_value _
  | Sig_exception _
  | Sig_module _
  | Sig_class _
  | Sig_modtype _
  | Sig_class_type _ -> `Nil _loc

        (* copied from [printtyp] *)
  | Sig_type (id,_,_) when Btype.is_row_name (Ident.name id) ->
      `Nil _loc
  | Sig_type (id,td,_rs) ->
      (* per single rs, the first is always [Trec_first]
         [Trec_not] seems to be used in [Recursive modules]
       *)
      type_declaration id td
  ]

(* constraint is removed in typedtree *)      
and type_declaration id
    {type_params;
     (* type_arity; = List.length type_params *)
     type_kind;
     type_private;
     type_manifest;
     (* type_variance; ignore *)
     type_newtype_level;
     _
   } : Ast.typedecl  =
  let name = Ident.name id in 
  let params = List.map type_expr type_params in
  let private_flag =
    match type_private with
    [Asttypes.Private -> `Private _loc  | Asttypes.Public -> `Public _loc ] in
  let manifest =
    Option.map type_expr type_manifest in 
  match (type_kind,manifest) with
  [(Type_abstract,None) ->
    `TyDcl(_loc,`Lid(_loc,name),params, `Nil _loc, [])
  |(Type_abstract,Some x)
    (* -> *)
    (*   `TyDcl(_loc,`Lid(_loc,name), *)
    (*          params, *)
    (*          `TyMan(_loc, private_flag,manifest) *)
    (*          , []) *)
  |(Type_record _,x) (* -> *)
      (* match x with *)
      (* [Some x -> `TyMan] *)
      (* `TyDcl(_loc,`Lid(_loc,name), *)
      (*        params, *)
             
      (*       ) *)
  |(Type_variant _,x)  ->
      
  failwithf "not"]
and type_expr ({desc ;_} : Types.type_expr) =
  match desc with
  [ Tvar _
  | Tarrow _
  | Ttuple _
  | Tconstr _

  | Tfield _ 

  
  | Tnil

  (* [fatal_error] *)    
  | Tlink _

  | Tobject _    
  | Tsubst _
  | Tvariant _
  | Tunivar _
  | Tpoly _
  | Tpackage _ -> failwith "not"

 ]
      ;
let signature (sg:Types.signature) =
  List.map signature_item sg;

  
  
