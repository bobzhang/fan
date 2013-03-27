open LibUtil
open Types
open AstLoc
let _loc = FanLoc.ghost
exception CtypNotSupport of type_desc
let rec signature_item (x : Types.signature_item) =
  (match x with
   | Sig_value _|Sig_exception _|Sig_module _|Sig_class _|Sig_modtype _
     |Sig_class_type _ -> None
   | Sig_type (id,_,_) when Btype.is_row_name (Ident.name id) -> None
   | Sig_type (id,td,_rs) ->
       (try Some (type_declaration id td) with | _ -> None) : typedecl option )
and type_declaration id
  { type_params; type_kind; type_private; type_manifest;_} =
  (let name = Ident.name id in
   let params = List.map type_exp type_params in
   let private_flag =
     match type_private with
     | Asttypes.Private  -> `Private _loc
     | Asttypes.Public  -> `PrNil _loc in
   let manifest = Option.map type_exp type_manifest in
   match (type_kind, manifest) with
   | (Type_abstract ,None ) ->
       `TyAbstr (_loc, (`Lid (_loc, name)), params, [])
   | (Type_abstract ,Some x) ->
       `TyDcl
         (_loc, (`Lid (_loc, name)), params, (`TyEq (_loc, private_flag, x)),
           [])
   | (Type_record (xs,_float),Some x) ->
       `TyDcl
         (_loc, (`Lid (_loc, name)), params,
           (`TyMan
              (_loc, x, private_flag, (`Record (_loc, (type_record xs))))),
           [])
   | (Type_record (xs,_float),None ) ->
       `TyDcl
         (_loc, (`Lid (_loc, name)), params,
           (`TyRepr (_loc, private_flag, (`Record (_loc, (type_record xs))))),
           [])
   | (Type_variant xs,Some x) ->
       `TyDcl
         (_loc, (`Lid (_loc, name)), params,
           (`TyMan (_loc, x, private_flag, (`Sum (_loc, (type_sum xs))))),
           [])
   | (Type_variant xs,None ) ->
       `TyDcl
         (_loc, (`Lid (_loc, name)), params,
           (`TyRepr (_loc, private_flag, (`Sum (_loc, (type_sum xs))))), []) : 
  typedecl )
and type_record (xs : (Ident.t* Asttypes.mutable_flag* type_exp) list) =
  (sem_of_list &
     (List.map
        (fun (i,m,e)  ->
           let name = Ident.name i in
           match m with
           | Asttypes.Mutable  ->
               `TyColMut
                 (_loc, (`Id (_loc, (`Lid (_loc, name)))), (type_exp e))
           | Asttypes.Immutable  ->
               `TyCol (_loc, (`Id (_loc, (`Lid (_loc, name)))), (type_exp e)))
        xs) : name_ctyp )
and type_sum (xs : (Ident.t* type_exp list* type_exp option) list) =
  (bar_of_list &
     (List.map
        (function
         | (i,xs,None ) ->
             let name = Ident.name i in
             (match xs with
              | [] -> `Id (_loc, (`Lid (_loc, name)))
              | x::[] ->
                  `Of (_loc, (`Id (_loc, (`Lid (_loc, name)))), (type_exp x))
              | _ ->
                  let tys = sta_of_list & (List.map type_exp xs) in
                  `Of (_loc, (`Id (_loc, (`Lid (_loc, name)))), tys))
         | (_i,_xs,Some _x) ->
             failwithf "type_sum  for gadt not supported yet") xs) : 
  or_ctyp )
and id_path (p : Path.t) =
  (match p with
   | Path.Pident x -> `Lid (_loc, (Ident.name x))
   | Path.Pdot (a,x,_depth) -> `Dot (_loc, (id_path a), (`Lid (_loc, x)))
   | Path.Papply (a,b) -> `App (_loc, (id_path a), (id_path b)) : ident )
and type_exp ({ desc;_} : Types.type_exp) =
  (match desc with
   | Tvar opt|Tunivar opt ->
       (match opt with
        | Some x -> `Quote (_loc, (`Normal _loc), (`Lid (_loc, x)))
        | None  -> `QuoteAny (_loc, (`Normal _loc)))
   | Ttuple ls -> tup & (sta_of_list & (List.map type_exp ls))
   | Tconstr (path,ls,_ref) ->
       (match ls with
        | [] -> `Id (_loc, (id_path path))
        | _ ->
            appl_of_list ((`Id (_loc, (id_path path))) ::
              (List.map type_exp ls)))
   | Tvariant _|Tpoly _|Tpackage _|Tlink _|Tfield _|Tnil |Tsubst _|Tobject _
     |Tarrow _ as x -> raise & (CtypNotSupport x) : ctyp )
let signature (sg : Types.signature) = List.map signature_item sg