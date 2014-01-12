

open Util
open Astn_util
open Astfn




type default =
  | Atom of exp
  | Invalid_argument 

let transform default =
  match default with
  | Atom e -> e
  | Invalid_argument -> %exp-{%invalid_arg{}}


(** types below are used to tell fan how to produce
   function of type [ident -> ident] *)
type fn  =
    [ `Pre of string
    | `Post of string
    | `Fun of (string->string)
    | `Same ] with print


let trans (x:fn) =
  match x with 
  | `Pre pre -> (fun x -> pre ^ x)
  | `Post post -> (fun x -> x ^ post)
  | `Same  -> (fun x -> x)
  | `Fun f -> f 
let trans_to_id f x = lid (trans f  x )

type param = {
    arity: int;
    names: string list;
    plugin_name:  string;
    id: fn ;
    default: default option;
    (* ?(default= %exp-{ failwith "arity >= 2 in other branches" } )*)
    mk_record: (Ctyp.record_col list -> exp) option;
    mk_variant: (string option -> Ctyp.ty_info list -> exp) option ;
    annot: (string -> (ctyp*ctyp)) option;
    builtin_tbl: ( ctyp * exp) list;
    excludes : string list;
  } (* with print *)

module type S = sig
  val p : param
end
      


module Make (U:S) = struct
  open U
  let arity = p.arity
  let names = p.names
  let builtin_tbl = Hashtblf.of_list p.builtin_tbl
  let plugin_name = p.plugin_name

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
  let mf_prefix = "mf_"

 
      

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

  and  exp_of_ctyp (ty:ctyp) =

    let tyvar = fun x -> %exp-{$lid{mf_prefix^x}}  in
    let rec aux  x : exp =
      if Hashtbl.mem builtin_tbl x then
        Hashtbl.find builtin_tbl x
      else 
        match x with
        | `Lid id -> (trans_to_id p.id id  :> exp )
        | (#ident' as id) ->
            (Idn_util.ident_map_of_ident (trans_to_id p.id) (Idn_util.to_vid id ) :> exp)
        | `App(t1,t2) ->
            %exp-{ ${aux t1} ${aux t2} }
        | `Quote (_,`Lid s) ->   tyvar s
        | `Arrow(t1,t2) ->
            aux %ctyp-{ ($t1,$t2) arrow } (* arrow is a keyword now*)
        | `Par _  as ty ->
            exp_of_tuple_ctyp ty 
        | (ty:ctyp) -> (* TOPBIND required *)
            failwith (__BIND__ ^ Astfn_print.dump_ctyp ty) in
    aux ty
  and  mapi_exp (i:int) (ty : ctyp)  : Ctyp.ty_info =
    let name_exp = exp_of_ctyp ty in 
    let base = apply_args name_exp  names in
    (* FIXME as a tuple it is useful when arity> 1??? *)
    let id_eps = Listf.init arity (fun index  -> Id.xid ~off:index i)  in
    let ep0 = List.hd id_eps in
    let id_ep = tuple_com  id_eps  in
    let exp = appl_of_list (base :: (id_eps:>exp list))  in
    {name_exp;
     info_exp=exp;
     id_ep;
     id_eps;
     ep0;
     ty
   }



  let mk_prefix  (vars:opt_decl_params) (acc:exp)   =
    let varf = fun x -> mf_prefix ^ x  in
    let  f (var:decl_params) acc =
      match var with
      | `Quote(_,`Lid s) ->
          %exp-{ fun $lid{ varf s} -> $acc }
      | t  ->
          failwith  (__BIND__  ^ Astfn_print.dump_decl_params t) in
    let v = Expn_util.abstract p.names  acc in
    let vars =
      match vars with
      |`None  -> []
      |`Some xs -> Ast_basic.N.list_of_com xs [] in
    List.fold_right f vars v 


      
  let exp_of_poly_variant result ty =


    
    (* FIXME, be more precise  *)
    let ls = Ctyp.view_variant ty in
    let aux acc x =
        (match x with
        | `variant (cons,args) ->
            let cons = "`" ^ cons in
            let len = List.length args in
            let exps = List.mapi mapi_exp args in
            %case-{ $pat{Id_epn.gen_tuple_n  ~arity cons len}
                    ->
                    ${Lazy.force mk_variant (Some cons) exps}}
        | `abbrev lid ->  (* for the case [`a | b ] *)
            let e = apply_args (exp_of_ctyp (lid:>ctyp))  names  in
            let (f,a) = Ast_basic.N.view_app [] result in
            let annot = appl_of_list (f :: List.map (fun _ -> `Any) a) in
            let pat  =
              tuple_com
                (Listf.init arity 
                   (fun i -> %pat-{ (#$id:lid as $lid{ Id.x ~off:i 0 }) }))
            in
            let e = appl_of_list
                (e::
                 Listf.init arity (fun i -> %exp-{ $id{Id.xid ~off:i 0} })) in 
            %case-{
            $pat -> ( $e  :> $annot)}) :: acc   in
    (match ls with
    | [] -> []
    | [x] -> aux [] x 
    | _ -> 
        let res = List.fold_left aux [] ls  in
        let res =
          match Option.map transform p.default  with
          | Some x when arity >= 2 -> %case-{_ -> $x}::res
          | _ ->  res in
        List.rev res )
    |> Expn_util.currying ~arity

  let exp_of_or_ctyp (ty : or_ctyp)  : exp=
    let mk   (cons,tyargs)  : case =
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

      
(* *)
(*   Given type declarations, generate corresponding *)
(*   Astf node represent the [function] *)
(*   (combine both exp_of_ctyp and simple_exp_of_ctyp) *)
  let fun_of_tydcl  result   tydcl :exp =
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
                      | {label;is_mutable;ty} ->
                          {Ctyp.info = mapi_exp i ty  ;
                           label;
                           is_mutable}) cols in
                (* For single tuple pattern match this can be optimized *)
(*            by the ocaml compiler *)
                [ %case-{ $pat:pat -> ${Lazy.force mk_record info}  } ]
                |> Expn_util.currying ~arity
            |  `Sum ctyp ->
                (* for [exp_of_ctyp] appending names was delayed to be handled in mkcon *)
                exp_of_or_ctyp ctyp
            | t ->
                failwith (__BIND__  ^ Astfn_print.dump_type_repr t)
            end

        | `TyEq(_,ctyp) ->
            begin match ctyp with
            | (#ident'  | `Par _ | `Quote _ | `Arrow _ | `App _ as x) ->
                Expn_util.eta_expand (apply_args (exp_of_ctyp x) names) arity
            | `PolyEq t | `PolySup t | `PolyInf t|`PolyInfSup(t,_) ->
                exp_of_poly_variant result t
            | t -> failwith (__BIND__ ^ Astfn_print.dump_ctyp t)
            end
        | t -> failwith  (__BIND__ ^  Astfn_print.dump_type_info t)
        end |> mk_prefix tyvars


    | t -> failwith (__BIND__  ^ Astfn_print.dump_decl t)

          
  let bind_of_tydcl tydcl =
    let (name,_len) =
      match  tydcl with
      | `TyDcl ( `Lid name, tyvars, _, _) ->
          (name, match tyvars with
          | `None  -> 0
          | `Some xs -> List.length @@ Ast_basic.N.list_of_com  xs [])
      | tydcl ->
          failwith (__BIND__ ^ Astfn_print.dump_decl tydcl) in
    let fname = trans_to_id p.id name in
    let (annot,result) =
      match p.annot with
      |None -> (None,%ctyp-{_})
      |Some (f:string -> (ctyp * ctyp))  -> let (a,b) = f name in (Some a, b) in

    let fun_exp =
      if not @@ %p{`TyAbstr _} tydcl  then
        fun_of_tydcl result tydcl
      else
        begin
          Format.eprintf "Warning: %s as a abstract type no structure generated\n"
          @@ Astfn_print.dump_decl tydcl;
          %exp-{ failwith "Abstract data type not implemented" }
        end in
    match annot with
    | None ->
        %bind-{ $fname = $fun_exp }
    | Some x ->
        %bind-{ $fname  = ($fun_exp : $x ) }
          

  let stru_of_mtyps (lst:Sigs_util.mtyps) : stru option =
    (* return new types as generated  new context *)
    let fs (ty:Sigs_util.types) : stru option =
      match ty with
      | Mutual named_types ->
          begin match named_types with
          | [] -> None (* FIXME *)
          | xs ->
              let bind =
                Listf.reduce_right_with
                  ~compose:(fun x y -> %bind-{ $x and $y } )
                  ~f:(fun (_name,ty) ->
                    bind_of_tydcl  ty ) xs in
              Some %stru-{ let rec $bind }
          end
      | Single (_,tydcl) ->
          let flag =
            if Ctyp.is_recursive tydcl then `Positive
            else `Negative in
          let  bind = bind_of_tydcl  tydcl in
          Some %stru-{ let $rec:flag  $bind } in
     match lst with
     | [] -> None 
     | _ ->  Some (sem_of_list (Listf.filter_map fs lst ))

                

(************************************)
(** *)
(*   Generate warnings for abstract data type *)
(*   and qualified data type. *)
(*   all the types in one module will derive a class  *)
(************************************)

(** For var, in most cases we just add a prefix *)
(*    mf_, so we just fix it here *)

(*    For Objects, tctor_var, always (`Fun (fun x -> x)) *)
(*    FIXME we may need a more flexible way to compose branches *)
(*  *)
end

let register p =
  let module M = struct
    let p = p 
  end in
  let module N = Make(M) in
  begin 
    Hashtbl.replace Lang_t.dispatch_tbl p.plugin_name N.exp_of_ctyp;
    Typehook.register ~filter:(fun x -> not (List.mem x  p.excludes))
      (p.plugin_name, N.stru_of_mtyps )
   end   

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/derive_stru.cmo" *)
(* end: *)
