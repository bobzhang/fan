

open Util
open Astn_util
open Astfn
(* open Ctyp *)
type default =
  | Atom of exp
  | Invalid_argument

(* let transform default = *)
(*   match default with *)
(*   | Atom e -> e *)
(*   | Invalid_argument -> %exp-{%invalid_arg{}} *)

let transform default =
  match default with
  | Atom e -> e
  | Invalid_argument -> %exp-{%invalid_arg{}}


(** types below are used to tell fan how to produce
    function of type [ident -> ident] *)
(* type fn  = *)
(*     [ `Pre of string *)
(*     | `Post of string *)
(*     | `Fun of (string->string) *)
(*     | `Same ] with print *)


(* let trans (x:fn) = *)
(*   match x with  *)
(*   | `Pre pre -> (fun x -> pre ^ x) *)
(*   | `Post post -> (fun x -> x ^ post) *)
(*   | `Same  -> (fun x -> x) *)
(*   | `Fun f -> f  *)
(* let trans_to_id f x = lid (trans f  x ) *)

type param = {
    arity: int;
    names: string list;
    plugin_name:  string;
    mk_record: (Ctyp.record_col list -> exp) option;
    mk_variant: (string option -> Ctyp.ty_info list -> exp) option ;
      (* ?(default= %exp-{ failwith "arity >= 2 in other branches" } ) *)
    default : default option; 
    excludes : string list;
    kind : Ctyp.kind;
    base : string;
    class_name : string;
  }
(* type param = { *)
(*     arity: int; *)
(*     names: string list; *)
(*     plugin_name:  string; *)
(*     id: fn ; *)
(*     default: default option; *)
(*     (\* ?(default= %exp-{ failwith "arity >= 2 in other branches" } )*\) *)
(*     mk_record: (Ctyp.record_col list -> exp) option; *)
(*     mk_variant: (string option -> Ctyp.ty_info list -> exp) option ; *)
(*     annot: (string -> (ctyp*ctyp)) option; *)
(*     builtin_tbl: ( ctyp * exp) list; *)
(*     excludes : string list; *)
(*   } (\* with print *\) *)

      
module type S = sig
  val p : param
end
let check _ = ()
module Make (U:S) = struct
  open U 
    
  let (arity,names,plugin_name,base,class_name)
      = (p.arity,p.names,p.plugin_name,p.base, p.class_name)
  let mk_variant =
    lazy begin
      match p.mk_variant with 
      | None -> failwithf "Generator  %s can not handle variant" plugin_name
      | Some x -> x
    end
  let mk_tuple =
    lazy
      begin
        match mk_variant with 
        | lazy x -> x None 
      end
  let mk_record =
    lazy
      begin
        match p.mk_record with 
        | None -> failwithf "Generator %s can not handle record" plugin_name
        | Some x -> x 
      end

  let mf_prefix = %fresh{_mf}
  let kind = p.kind
  let left_type_variable = `Pre mf_prefix    
  let tyvar_trans  x =  mf_prefix ^ x

  (* The same as {!Derive_stru} *)
  let rec  exp_of_tuple_ctyp (ty:ctyp) : exp =
    match ty with
    | `Par t  -> 
        let ls = Ast_basic.N.list_of_star t [] in
        let len = List.length ls in
        Expn_util.abstract names
          (Expn_util.currying ~arity
             [ %case-{ 
               $pat{Id_epn.mk_tuple ~arity ~number:len}
               ->
                 ${Lazy.force mk_tuple (List.mapi mapi_exp  ls)}
             } ] )
    | _  -> failwith (__BIND__  ^ Astfn_print.dump_ctyp ty)
  (* Different *)        
  and  exp_of_ctyp ty : exp =
    let rec aux (x : ctyp) =
      match x with 
      | `Lid x -> %exp-{self#$lid:x}
      | (#ident' as id)  -> (** Print warning FIXME *)
          let s  = Idn_util.map_to_string (Idn_util.to_vid id) in
          %exp-{self#$lid:s}
      | `Quote(_,`Lid v) ->
          %exp-{ $lid{tyvar_trans v} self } 
      | `App _  as ty ->
          (match Ast_basic.N.list_of_app ty []  with
          | (#ident' as tctor) :: ls  ->
              appl_of_list
                (aux tctor  ::
                 (ls |> List.map
                   (function
                     | `Quote (_,`Lid s) -> %exp-{ $lid{tyvar_trans s} } 
                     | t -> %exp-{ fun self -> ${aux t} } )) )
          | _  ->
              failwith (__BIND__ ^ Astfn_print.dump_ctyp ty))
      | `Arrow(t1,t2) -> aux %ctyp-{ ($t1,$t2) arrow  } 
      | `Par _  as ty ->
          exp_of_tuple_ctyp  ty 
      | ty -> failwith ( __BIND__ ^ Astfn_print.dump_ctyp ty)  in
    aux ty 


  and mapi_exp (i:int) (ty : ctyp) =
    let name_exp = exp_of_ctyp ty in 
    let base = apply_args name_exp  names in
    (* FIXME as a tuple it is useful when arity> 1??? *)
    let id_eps = Listf.init arity @@ fun index  -> Id.xid ~off:index i  in
    let ep0 = List.hd id_eps in
    let id_ep = tuple_com  id_eps  in
    let exp = appl_of_list (base:: (id_eps:>exp list))  in
    {Ctyp.name_exp;
     info_exp=exp;
     id_ep;
     id_eps;
     ep0;
     ty}

  let mk_prefix (vars:opt_decl_params) (acc:exp)   = 
    let  f (var:decl_params) acc =
      match var with
      | `Quote(_,`Lid(s)) -> %exp-{ fun $lid{tyvar_trans s} -> $acc }
      | t  ->
          failwith (__BIND__  ^ Astfn_print.dump_decl_params t) in
    match vars with
    |`None  -> Expn_util.abstract names  acc
    |`Some xs ->
        let vars = Ast_basic.N.list_of_com xs [] in
        List.fold_right f vars (Expn_util.abstract names  acc)

  let exp_of_poly_variant
       ~result ty =
    let f (cons,tyargs) :  case=
      let len = List.length tyargs in
      let p = (Id_epn.gen_tuple_n  ~arity cons len :> pat) in
      let mk (cons,tyargs) =
        let exps = List.mapi mapi_exp tyargs in
        Lazy.force mk_variant (Some cons) exps in
      let e = mk (cons,tyargs) in
      %case-{ $pat:p -> $e } in 
    (* for the case [`a | b ] *)
    let simple (lid:ident) :case=
      let e = (exp_of_ctyp (lid:>ctyp)) +> names  in
      let (f,a) = Ast_basic.N.view_app [] result in
      let annot = appl_of_list (f :: List.map (fun _ -> `Any) a) in
      
      Ctyp.gen_tuple_abbrev ~arity ~annot ~destination:(Obj kind) lid e
    in
    (* FIXME, be more precise  *)
    (* let info = (TyVrnEq, List.length (Ast_basic.N.list_of_bar ty [])) in *)
    let ls = Ctyp.view_variant ty in
    let res =
      let res = List.fold_left
          (fun  acc x ->
            match x with
            | `variant (cons,args) -> f ("`"^cons,args)::acc
            | `abbrev lid ->  simple lid :: acc  )  [] ls in
      let t =
        if List.length res >= 2 && arity >= 2 then
          
          match Option.map transform p.default  with
          | Some x-> %case-{_ -> $x}::res | None -> res 
              (* [default info :: res] *)
        else res in
      List.rev t in
    Expn_util.currying ~arity res


  (* The same as {!Derive_stru} *)
  let exp_of_or_ctyp (ty : or_ctyp)  : exp=
    let mk   (cons,tyargs)   =
      let args_length = List.length tyargs in  (* ` is not needed here *)
      let exps = List.mapi mapi_exp   tyargs in
      %case-{ ${(Id_epn.gen_tuple_n ~arity  cons args_length )} ->
         ${Lazy.force mk_variant (Some cons) exps} } in
    begin
      let reduce_data_ctors (ty:or_ctyp)   = 
        let branches = Ast_basic.N.list_of_bar ty [] in
        let aux acc (x:or_ctyp)   = 
          (match x with
          | `Of (`Uid cons, tys) ->
              mk (cons , Ast_basic.N.list_of_star tys [])
          | `Uid  cons -> 
              mk (cons, [] )
          | t-> failwith  (__BIND__ ^ Astfn_print.dump_or_ctyp t))::acc  in
        match branches with
        | [] -> [] (* should not happen *)
        | [x] -> aux [] x (* 1 x 1 x 1 ==> 1 *)
        | _ -> 
            let res = List.fold_left aux  []  branches in
            if arity >= 2 then
              match Option.map transform p.default with 
              | None -> res
              | Some x -> %case-{ _ -> $x} :: res
            else res in
      reduce_data_ctors ty  
      |>  List.rev
      |> Expn_util.currying ~arity 
    end
      
  



(* +-----------------------------------------------------------------+
   | Combine the utilities together                                  |
   +-----------------------------------------------------------------+ *)
      
(*
  Given type declarations, generate corresponding
  Astf node represent the [function]
  (combine both exp_of_ctyp and simple_exp_of_ctyp) *)  

  let fun_of_tydcl ~result 
        tydcl :exp = 
    match (tydcl:decl) with 
    | `TyDcl ( _, tyvars, ctyp, _constraints) ->
        begin match ctyp with
        |  `TyMan(_,_,repr) | `TyRepr(_,repr) ->
            begin match repr with
            | `Record t ->       
                let cols =  Ctyp.list_of_record t  in
                let pat = (Ctyp.mk_record ~arity  cols  :> pat)in
                let info =
                  List.mapi
                    (fun i x ->
                      match (x:Ctyp.col) with
                        {label;is_mutable;ty} ->
                          {Ctyp.info = mapi_exp   i ty  ;
                           label;
                           is_mutable}) cols in
                (* For single tuple pattern match this can be optimized
                   by the ocaml compiler *)
                mk_prefix   tyvars
                  (Expn_util.currying ~arity [ %case-{ $pat:pat -> ${Lazy.force mk_record info}  } ])

            |  `Sum ctyp -> 
                let funct = exp_of_or_ctyp ctyp in  
                (* for [exp_of_ctyp] appending names was delayed to be handled in mkcon *)
                mk_prefix  tyvars funct
            | t ->
                failwith (__BIND__ ^ Astfn_print.dump_type_repr t)
            end
        | `TyEq(_,ctyp) ->
            begin match ctyp with 
            | (#ident'  | `Par _ | `Quote _ | `Arrow _ | `App _ as x) ->
                mk_prefix  tyvars
                  (Expn_util.eta_expand
                      (Astn_util.apply_args (exp_of_ctyp x) names) arity)
            | `PolyEq t | `PolySup t | `PolyInf t|`PolyInfSup(t,_) -> 
                let case =  exp_of_poly_variant  ~result t  in
                mk_prefix   tyvars case
            | t -> failwith (__BIND__  ^ Astfn_print.dump_ctyp t)
            end
        | t -> failwith  (__BIND__ ^ Astfn_print.dump_type_info t)
        end
    | t -> failwith  (__BIND__ ^ Astfn_print.dump_decl t)



          
  let obj_of_mtyps
    (lst:Sigs_util.mtyps)  = 
  let tbl = Hashtbl.create 50 in 
  let f tydcl result : exp  =
    fun_of_tydcl ~result tydcl in
  let mk_type tydcl =
    let (name,len) =
      match  tydcl with
      | `TyDcl ( `Lid name, tyvars, _, _) ->
          (name, match tyvars with
          | `None  -> 0
          | `Some xs -> List.length @@ Ast_basic.N.list_of_com  xs [])
      | tydcl ->
          failwith (__BIND__ ^ Astfn_print.dump_decl tydcl) in
    let prefix = List.length names in
    let (ty,result_type) =
      Ctyp.mk_method_type ~number:arity ~prefix ~id:%ident-{ $lid:name } len 
        (Obj kind) in
    (ty,result_type) in
  
  let mk_clfield (name,tydcl) : clfield =
    let (ty,result_type) = mk_type tydcl in
    %clfield-{ method $lid:name : $ty = ${f tydcl result_type} }  in 
  let fs (ty:Sigs_util.types) : clfield =
    match ty with
    | Mutual named_types ->
        sem_of_list (List.map mk_clfield named_types)
    | Single ((name,tydcl) as  named_type) ->
        match Ctyp.abstract_list tydcl with
        | Some n  -> 
            let ty_str : string =   Astfn_print.dump_decl tydcl  in
            let () = Hashtbl.add tbl ty_str (Ctyp.Abstract ty_str) in 
            let (ty,_) = mk_type tydcl in
            %clfield-{ method $lid:name : $ty= ${Expn_util.unknown n}}
        | None ->  mk_clfield named_type  in 
  (* Loc.t will be translated to loc_t
     we need to process extra to generate method loc_t *)
  let (extras,lst) = Sigs_util.transform_mtyps lst in 
  let body = List.map fs lst in
  let prefix = List.length names in
  let body : clfield =
    let items = List.map (fun (dest,src,len) ->
      let (ty,_dest) = Ctyp.mk_method_type ~number:arity ~prefix ~id:src len
          (Obj kind) in
      let () = Hashtbl.add tbl dest (Qualified dest) in
      %clfield-{ method $lid:dest : $ty = ${Expn_util.unknown len} } ) extras in
    sem_of_list (body @ items) in 
  let v = Ctyp.mk_obj class_name  base body in
  begin
    Hashtbl.iter
      (fun _ v ->
        Formatf.eprintf "@[%a@]@." Ctyp.pp_print_warning_type  v)
      tbl;
   Some v
  end




(** For var, in most cases we just add a prefix
    mf_, so we just fix it here

    For Objects, tctor_var, always (`Fun (fun x -> x))
    FIXME we may need a more flexible way to compose branches
 *)

end

let register (p:param) =
  let module N = Make (struct let p = p end ) in
  begin
    Typehook.register
      ~filter:(fun x -> not (List.mem x p.excludes))
      (p.plugin_name, N.obj_of_mtyps)
  end
    
(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/derive_obj.cmo" *)
(* end: *)
