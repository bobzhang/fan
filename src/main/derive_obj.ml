

open Util
open Astn_util
open Astfn
open Ctyp
(* type default = *)
(*   | Atom of exp *)
(*   | Invalid_argument  *)

(* let transform default = *)
(*   match default with *)
(*   | Atom e -> e *)
(*   | Invalid_argument -> %exp-{%invalid_arg{}} *)


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


    excludes: string list;
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
    
  let (arity,names,plugin_name) = (p.arity,p.names,p.plugin_name)
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
  let left_type_variable = `Pre mf_prefix    




  let rec obj_simple_exp_of_ctyp ~right_type_id  ~right_type_variable
      ty : exp = with {pat:ctyp}
  let trans = transform right_type_id in
  let var = basic_transform left_type_variable in
  let tyvar = right_transform right_type_variable  in 
  let rec aux : ctyp -> exp = function
    | (#ident' as id)  -> trans (Idn_util.to_vid id)
    | `Quote(_,`Lid s) ->   tyvar s
    | `App _  as ty ->
        (match Ast_basic.N.list_of_app ty []  with
        | (#ident' as tctor) :: ls  ->
            appl_of_list
              (trans (Idn_util.to_vid tctor) ::
               (ls |> List.map
                 (function
                   | `Quote (_,`Lid s) -> %exp-{ $lid{var s} } 
                   | t -> %exp-{ fun self -> ${aux t} } )) )
        | _  ->
            failwithf "list_of_app in obj_simple_exp_of_ctyp: %s"
              (Astfn_print.dump_ctyp ty))
    | `Arrow(t1,t2) -> aux %ctyp-{ ($t1,$t2) arrow  } 
    | `Par _  as ty ->
        tuple_exp_of_ctyp  ~mk_tuple:(Lazy.force mk_tuple)
          ~f:(obj_simple_exp_of_ctyp ~right_type_id 
                ~right_type_variable ) ty 
    | ty -> failwith ( __BIND__ ^ Astfn_print.dump_ctyp ty)  in
  aux ty 


  let mk_prefix (vars:opt_decl_params) (acc:exp)   = 
    (* let open Transform in  *)
    let varf = basic_transform left_type_variable in
    let  f (var:decl_params) acc =
      match var with
      | `Quote(_,`Lid(s)) -> %exp-{ fun $lid{ varf s} -> $acc }
      | t  ->
          failwith (__BIND__  ^ Astfn_print.dump_decl_params t) in
    match vars with
    |`None  -> Expn_util.abstract names  acc
    |`Some xs ->
        let vars = Ast_basic.N.list_of_com xs [] in
        List.fold_right f vars (Expn_util.abstract names  acc)

  let exp_of_variant
      ~default  ~destination
      simple_exp_of_ctyp ~result ty =
    let f (cons,tyargs) :  case=
      let len = List.length tyargs in
      let p = (Id_epn.gen_tuple_n (* ?cons_transform *) ~arity cons len :> pat) in
      let mk (cons,tyargs) =
        let exps = List.mapi (mapi_exp  ~f:simple_exp_of_ctyp) tyargs in
        Lazy.force mk_variant (Some cons) exps in
      let e = mk (cons,tyargs) in
      %case-{ $pat:p -> $e } in 
    (* for the case [`a | b ] *)
    let simple (lid:ident) :case=
      let e = (simple_exp_of_ctyp (lid:>ctyp)) +> names  in
      let (f,a) = Ast_basic.N.view_app [] result in
      let annot = appl_of_list (f :: List.map (fun _ -> `Any) a) in
      gen_tuple_abbrev ~arity ~annot ~destination lid e in
    (* FIXME, be more precise  *)
    let info = (TyVrnEq, List.length (Ast_basic.N.list_of_bar ty [])) in
    let ls = Ctyp.view_variant ty in
    let res =
      let res = List.fold_left
          (fun  acc x ->
            match x with
            | `variant (cons,args) -> f ("`"^cons,args)::acc
            | `abbrev lid ->  simple lid :: acc  )  [] ls in
      let t =
        if List.length res >= 2 && arity >= 2 then
          match default info with | Some x-> x::res | None -> res 
              (* [default info :: res] *)
        else res in
      List.rev t in
    Expn_util.currying ~arity res

      
  let exp_of_ctyp
      (* ?cons_transform *)
      ~default 
      simple_exp_of_ctyp (ty : or_ctyp)  =
    let f  (cons:string) (tyargs:ctyp list )  : case = 
      let args_length = List.length tyargs in  (* ` is not needed here *)
      let p : pat =
        (* calling gen_tuple_n*)
        (Id_epn.gen_tuple_n (* ?cons_transform *) ~arity  cons args_length :> pat) in
      let mk (cons,tyargs) =
        let exps = List.mapi (mapi_exp ~f:simple_exp_of_ctyp) tyargs in
        Lazy.force mk_variant (Some cons) exps in
      let e = mk (cons,tyargs) in
      %case-{ $pat:p -> $e } in  begin 
        let info = (Sum, List.length (Ast_basic.N.list_of_bar ty [])) in 
        let res :  case list =
          Ctyp.reduce_data_ctors ty  [] f ~compose:cons  in
        let res =
          let t = (* only under this case we need defaulting  *)
            if List.length res >= 2 && arity >= 2 then
              match default info with
              | Some x-> x::res
              | None -> res 
            else res in
          List.rev t in 
        Expn_util.currying ~arity res 
      end



(* +-----------------------------------------------------------------+
   | Combine the utilities together                                  |
   +-----------------------------------------------------------------+ *)
      
(*
  Given type declarations, generate corresponding
  Astf node represent the [function]
  (combine both exp_of_ctyp and simple_exp_of_ctyp) *)  

  let fun_of_tydcl ~result
      simple_exp_of_ctyp exp_of_ctyp exp_of_variant  tydcl :exp = 
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
                          {info = (mapi_exp  ~f:simple_exp_of_ctyp) i ty  ;
                           label;
                           is_mutable}) cols in
                (* For single tuple pattern match this can be optimized
                   by the ocaml compiler *)
                mk_prefix   tyvars
                  (Expn_util.currying ~arity [ %case-{ $pat:pat -> ${Lazy.force mk_record info}  } ])

            |  `Sum ctyp -> 
                let funct = exp_of_ctyp ctyp in  
                (* for [exp_of_ctyp] appending names was delayed to be handled in mkcon *)
                mk_prefix  tyvars funct
            | t ->
                failwithf "fun_of_tydcl outer %s" (Astfn_print.dump_type_repr t)
            end
        | `TyEq(_,ctyp) ->
            begin match ctyp with 
            | (#ident'  | `Par _ | `Quote _ | `Arrow _ | `App _ as x) ->
                let exp = simple_exp_of_ctyp x in
                let funct = Expn_util.eta_expand (exp+>names) arity  in
                mk_prefix  tyvars funct
            | `PolyEq t | `PolySup t | `PolyInf t|`PolyInfSup(t,_) -> 
                let case =  exp_of_variant ~result t  in
                mk_prefix   tyvars case
            | t -> failwithf "fun_of_tydcl inner %s" (Astfn_print.dump_ctyp t)
            end
        | t -> failwithf  "fun_of_tydcl middle %s" (Astfn_print.dump_type_info t)
        end
    | t -> failwithf "fun_of_tydcl outer %s" (Astfn_print.dump_decl t)



          
  let obj_of_mtyps
      (* ?cons_transform *)
      (* ?module_name *)
      ~default  
      
      base
      class_name  simple_exp_of_ctyp ~kind:(k:kind) (lst:Sigs_util.mtyps) : stru = with {pat:ctyp}
  let tbl = Hashtbl.create 50 in 
  let f tydcl result : exp  =
    fun_of_tydcl  

      simple_exp_of_ctyp
      (exp_of_ctyp (* ?cons_transform *)
         ~default
         simple_exp_of_ctyp)
      (exp_of_variant (* ?cons_transform *)
         ~destination:(Obj k)
         ~default 
         simple_exp_of_ctyp) ~result tydcl in
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
        (Obj k) in
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
            let () = Hashtbl.add tbl ty_str (Abstract ty_str) in 
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
      let (ty,_dest) = Ctyp.mk_method_type ~number:arity ~prefix ~id:src len (Obj k) in
      let () = Hashtbl.add tbl dest (Qualified dest) in
      %clfield-{ method $lid:dest : $ty = ${Expn_util.unknown len} } ) extras in
    sem_of_list (body @ items) in 
  let v = Ctyp.mk_obj class_name  base body in
  (Hashtbl.iter (fun _ v ->
    Formatf.eprintf "@[%a@]@." pp_print_warning_type  v)
     tbl;
   v
  ) 


    

(*   check S.names; *)








(** For var, in most cases we just add a prefix
    mf_, so we just fix it here

    For Objects, tctor_var, always (`Fun (fun x -> x))
    FIXME we may need a more flexible way to compose branches
 *)











  let mk
      ?(default= %exp-{ failwith "arity >= 2 in other branches" } )
      (* ?cons_transform *)
      ~kind
      ~base
      ~class_name = 
    let make ()= 
      let () =  check names in


      let right_type_variable =
        `Exp (fun v -> let v = basic_transform left_type_variable v
        in  %exp-{ $lid:v self } ) in
      let left_type_id  = `Pre ""in
      let right_type_id  =
        `Obj (basic_transform left_type_id) in
      let default (_,number)=
        if number > 1 then
          let pat = (Id_epn.tuple_of_number `Any arity :> pat)in 
          Some %case-{ $pat:pat -> $default }
        else None in
      obj_of_mtyps
        (* ?cons_transform *)
        ~default

        base
        class_name
        (obj_simple_exp_of_ctyp
           ~right_type_id  ~right_type_variable
        )
        ~kind in
    make

end

(* let register (p:param) =  *)
(*   let module N = Make (struct let p = p end ) in  *)
(*   register p  *)
    
(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/derive_obj.cmo" *)
(* end: *)
