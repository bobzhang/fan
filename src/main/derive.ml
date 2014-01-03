

open Format
open Util
open Astn_util
open Astfn
open Ctyp
open Sigs_util
  
let check_valid str =
  let len = String.length str in
  if
    not
      (len > 1 &&
       (not @@ Charf.is_digit str.[1])
         && (not @@ Stringf.starts_with str "all_")) then begin
           eprintf "%s is not a valid name" str;
           eprintf "For valid name its length should be more than 1\n\
             can not be a-[digit], can not start with [all_]";
           exit 2;
         end 


(* preserved keywords for the generator *)
let preserve =  ["self";"unit"; "result"]

let check names =
  List.iter (fun name ->
    if List.mem name preserve  then begin 
      eprintf "%s is not a valid name\n" name;
      eprintf "preserved keywords:\n";
      List.iter (fun s -> eprintf "%s\n" s) preserve;
      exit 2
    end
    else check_valid name) names


(* let mapi_exp *)
(*     ?(arity=1) *)
(*     ?(names=[]) *)
(*     ~f:(f:(ctyp->exp)) *)
(*     (i:int) *)
(*     (ty : ctyp)  : *)
(*     ty_info = *)
(*   let name_exp = f ty in  *)
(*   let base = apply_args name_exp  names in *)
(*   (\* FIXME as a tuple it is useful when arity> 1??? *\) *)
(*   let id_eps = Listf.init arity @@ fun index  -> Id.xid ~off:index i  in *)
(*   let ep0 = List.hd id_eps in *)
(*   let id_ep = tuple_com  id_eps  in *)
(*   let exp = appl_of_list (base:: (id_eps:>exp list))  in *)
(*   {name_exp; *)
(*    info_exp=exp; *)
(*    id_ep; *)
(*    id_eps; *)
(*    ep0; *)
(*    ty *)
(*  } *)


(* let tuple_exp_of_ctyp ?(arity=1) ?(names=[]) ~mk_tuple *)
(*     ~f (ty:ctyp) : exp = *)
(*   match ty with *)
(*   | `Par t  ->  *)
(*     let ls = Ast_basic.N.list_of_star t [] in *)
(*     let len = List.length ls in *)
(*     let pat = (Id_epn.mk_tuple ~arity ~number:len :> pat) in *)
(*     let tys = *)
(*       mk_tuple *)
(*         (List.mapi (mapi_exp ~arity ~names  ~f) ls) in *)
(*     Expn_util.abstract names *)
(*       (Expn_util.currying [ %case-{ $pat:pat -> $tys } ] ~arity) *)
(*   | _  -> failwith (__BIND__  ^ Astfn_print.dump_ctyp ty) *)


  
let normal_simple_exp_of_ctyp
    ?arity ?names ~mk_tuple
    ~right_type_id ~left_type_id
    ~right_type_variable
    cxt (ty:ctyp) = 
  (* let open Transform in *)
  let right_trans = transform right_type_id in
  let left_trans = basic_transform left_type_id in 
  let tyvar = right_transform right_type_variable  in 
  let rec aux =  function
    | `Lid id -> 
        if Hashset.mem cxt id then
          lid (left_trans id)
        else
          right_trans (lid id)
    | (#ident' as id) ->
        right_trans (Idn_util.to_vid id )
    | `App(t1,t2) ->
        %exp-{ ${aux t1} ${aux t2} }
    | `Quote (_,`Lid s) ->   tyvar s
    | `Arrow(t1,t2) ->
        aux %ctyp-{ ($t1,$t2) arrow } (* arrow is a keyword now*)
    | `Par _  as ty ->
        tuple_exp_of_ctyp  ?arity ?names ~mk_tuple
          ~f:aux ty 
    | (ty:ctyp) -> (* TOPBIND required *)
       failwithf "normal_simple_exp_of_ctyp : %s"
          (Astfn_print.dump_ctyp ty) in
  aux ty

(* slightly different from [normal_simple_exp_of_ctyp]
   without context??
 *)   
let rec obj_simple_exp_of_ctyp ~right_type_id ~left_type_variable ~right_type_variable
    ?names ?arity ~mk_tuple ty = with {pat:ctyp}
  (* let open Transform in  *)
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
        tuple_exp_of_ctyp ?arity ?names ~mk_tuple
          ~f:(obj_simple_exp_of_ctyp ~right_type_id ~left_type_variable
                ~right_type_variable ?names ?arity ~mk_tuple) ty 
    | ty -> failwithf "obj_simple_exp_of_ctyp: %s" (Astfn_print.dump_ctyp ty)  in
  aux ty 

let exp_of_ctyp
    ?cons_transform
    ?(arity=1)
    ?(names=[])
    ~default ~mk_variant
    simple_exp_of_ctyp (ty : or_ctyp)  =
  let f  (cons:string) (tyargs:ctyp list )  : case = 
    let args_length = List.length tyargs in  (* ` is not needed here *)
    let p : pat =
      (* calling gen_tuple_n*)
      (Id_epn.gen_tuple_n ?cons_transform ~arity  cons args_length :> pat) in
    let mk (cons,tyargs) =
      let exps = List.mapi (mapi_exp ~arity ~names ~f:simple_exp_of_ctyp) tyargs in
      mk_variant (Some cons) exps in
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

let exp_of_variant ?cons_transform
    ?(arity=1)?(names=[]) ~default ~mk_variant ~destination
    simple_exp_of_ctyp ~result ty =
  let f (cons,tyargs) :  case=
    let len = List.length tyargs in
    let p = (Id_epn.gen_tuple_n ?cons_transform ~arity cons len :> pat) in
    let mk (cons,tyargs) =
      let exps = List.mapi (mapi_exp ~arity ~names ~f:simple_exp_of_ctyp) tyargs in
      mk_variant (Some cons) exps in
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

let mk_prefix (vars:opt_decl_params) (acc:exp) ?(names=[])  ~left_type_variable= 
  (* let open Transform in  *)
  let varf = basic_transform left_type_variable in
  let  f (var:decl_params) acc =
    match var with
    | `Quote(_,`Lid(s)) -> %exp-{ fun $lid{ varf s} -> $acc }
    | t  ->
        failwithf  "mk_prefix: %s" (Astfn_print.dump_decl_params t) in
  match vars with
  |`None  -> Expn_util.abstract names  acc
  |`Some xs ->
      let vars = Ast_basic.N.list_of_com xs [] in
      List.fold_right f vars (Expn_util.abstract names  acc)
  


(* +-----------------------------------------------------------------+
   | Combine the utilities together                                  |
   +-----------------------------------------------------------------+ *)
  
(*
  Given type declarations, generate corresponding
  Astf node represent the [function]
  (combine both exp_of_ctyp and simple_exp_of_ctyp) *)  
let fun_of_tydcl
    ?(names=[]) ?(arity=1) ~left_type_variable ~mk_record  ~result
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
                     {info = (mapi_exp ~arity ~names ~f:simple_exp_of_ctyp) i ty  ;
                      label;
                      is_mutable}) cols in
        (* For single tuple pattern match this can be optimized
           by the ocaml compiler *)
        mk_prefix ~names ~left_type_variable tyvars
            (Expn_util.currying ~arity [ %case-{ $pat:pat -> ${mk_record info}  } ])

       |  `Sum ctyp -> 
          let funct = exp_of_ctyp ctyp in  
          (* for [exp_of_ctyp] appending names was delayed to be handled in mkcon *)
          mk_prefix ~names ~left_type_variable tyvars funct
       | t ->
          failwithf "fun_of_tydcl outer %s" (Astfn_print.dump_type_repr t)
         end
    | `TyEq(_,ctyp) ->
        begin match ctyp with 
        | (#ident'  | `Par _ | `Quote _ | `Arrow _ | `App _ as x) ->
          let exp = simple_exp_of_ctyp x in
          let funct = Expn_util.eta_expand (exp+>names) arity  in
          mk_prefix ~names ~left_type_variable tyvars funct
        | `PolyEq t | `PolySup t | `PolyInf t|`PolyInfSup(t,_) -> 
            let case =  exp_of_variant ~result t  in
            mk_prefix ~names ~left_type_variable tyvars case
        | t -> failwithf "fun_of_tydcl inner %s" (Astfn_print.dump_ctyp t)
        end
    | t -> failwithf  "fun_of_tydcl middle %s" (Astfn_print.dump_type_info t)
       end
   | t -> failwithf "fun_of_tydcl outer %s" (Astfn_print.dump_decl t)



let bind_of_tydcl ?cons_transform simple_exp_of_ctyp
    ?(arity=1) ?(names=[])
    ?(destination=Str_item)
    ?annot
    ~default ~mk_variant
    ~left_type_id:(left_type_id:basic_id_transform)
    ~left_type_variable:(left_type_variable:basic_id_transform)
    ~mk_record
    tydcl
    = 
  (* let open Transform in  *)
  let tctor_var = basic_transform left_type_id in
  let (name,len) =
    match  tydcl with
    | `TyDcl ( `Lid name, tyvars, _, _) ->
        (name, match tyvars with
        | `None  -> 0
        | `Some xs -> List.length @@ Ast_basic.N.list_of_com  xs [])
    | tydcl ->
        failwith (__BIND__ ^ Astfn_print.dump_decl tydcl) in
  let fname = tctor_var name in
  let prefix = List.length names in
  (* FIXME the annot using [_ty]?*)
  let (_ty,result) =
    Ctyp.mk_method_type
      ~number:arity ~prefix
      ~id:(lid name)
      len destination in
  let (annot,result) =
    match annot with
    |None -> (None,result)
    |Some f -> let (a,b) = f name in (Some a, b) in 

  let fun_exp =
    if not @@  Ctyp.is_abstract tydcl then 
      fun_of_tydcl 
        ~names ~arity ~left_type_variable ~mk_record
        ~result
        simple_exp_of_ctyp
        (exp_of_ctyp ?cons_transform ~arity ~names ~default ~mk_variant simple_exp_of_ctyp)
        (exp_of_variant
           ?cons_transform
           ~arity ~names ~default ~mk_variant
           ~destination simple_exp_of_ctyp)
        tydcl
    else
      begin 
        eprintf "Warning: %s as a abstract type no structure generated\n" @@ Astfn_print.dump_decl tydcl;
        %exp-{ failwith "Abstract data type not implemented" }
      end in
  match annot with
  | None -> 
      %bind-{ $lid:fname = $fun_exp }
  | Some x ->
      %bind-{ $lid:fname : $x = $fun_exp }

let stru_of_mtyps (* ?module_name *) ?cons_transform ?annot
    ?arity ?names ~default ~mk_variant ~left_type_id ~left_type_variable
    ~mk_record
    simple_exp_of_ctyp_with_cxt
    (lst:mtyps) : stru =
  let cxt  = Hashset.create 50 in 
  let mk_bind : decl -> bind =
    bind_of_tydcl ?cons_transform ?arity ?annot
      ?names ~default ~mk_variant ~left_type_id ~left_type_variable ~mk_record
      (simple_exp_of_ctyp_with_cxt cxt) in
  (* return new types as generated  new context *)
  let fs (ty:types) : stru=
    match ty with
    | `Mutual named_types ->
        ( match named_types with
        | [] ->  %stru-{ let _ = ()} (* FIXME *)
        | xs ->
            (List.iter (fun (name,_ty)  -> Hashset.add cxt name) xs ;
            let bind =
              Listf.reduce_right_with
                ~compose:(fun x y -> %bind-{ $x and $y } )
                ~f:(fun (_name,ty) ->
                  mk_bind  ty ) xs in
            %stru-{ let rec $bind }))
    | `Single (name,tydcl) ->
        (Hashset.add cxt name;
         let flag =
           if Ctyp.is_recursive tydcl then `Positive 
           else `Negative  
         and bind = mk_bind  tydcl in 
         %stru-{ let $rec:flag  $bind }) in
  (* let item = *)
    match lst with
    | [] -> %stru-{let _ = ()}
    | _ ->  sem_of_list (List.map fs lst )
      (* match module_name with *)
      (* | None -> item *)
      (* | Some m -> %stru-{ module $uid:m = struct $item end }  *)


            

(*************************************************************************)
(**
  Generate warnings for abstract data type
  and qualified data type.
  all the types in one module will derive a class  *)
(*************************************************************************)
let obj_of_mtyps
    ?cons_transform
    (* ?module_name *)
    ?(arity=1) ?(names=[]) ~default  
    ~left_type_variable:(left_type_variable:basic_id_transform)
    ~mk_record
    ~mk_variant
     base
    class_name  simple_exp_of_ctyp ~kind:(k:kind) (lst:mtyps) : stru = with {pat:ctyp}
  let tbl = Hashtbl.create 50 in 
    let f tydcl result =
      fun_of_tydcl ~names 
        ~arity ~left_type_variable
        ~mk_record 
        simple_exp_of_ctyp
        (exp_of_ctyp ?cons_transform
           ~arity ~names
           ~default ~mk_variant
           simple_exp_of_ctyp)
        (exp_of_variant ?cons_transform
           ~destination:(Obj k)
           ~arity ~names
           ~default ~mk_variant
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
    let fs (ty:types) : clfield =
      match ty with
      | `Mutual named_types ->
        sem_of_list (List.map mk_clfield named_types)
      | `Single ((name,tydcl) as  named_type) ->
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
            eprintf "@[%a@]@." pp_print_warning_type  v)
            tbl;
          v
         ) 


  

(*   check S.names; *)








(** For var, in most cases we just add a prefix
   mf_, so we just fix it here

   For Objects, tctor_var, always (`Fun (fun x -> x))
   FIXME we may need a more flexible way to compose branches
 *)
let gen_stru
    ?(arity=1)
    ?(default= %exp-{ failwith "arity >= 2 in other branches" } )
    ?cons_transform
    ?annot
    ~id:(id:basic_id_transform)  ?(names=[])  
    (* you must specify when arity >=2 *)
    ~mk_record ~mk_variant ()= 
  let left_type_variable  = `Pre "mf_" in
  let right_type_variable = `Pre "mf_"in
  let left_type_id = id in
  let right_type_id =
    (id:>full_id_transform)  in
  let default (_,number)=
    if number > 1 then
      let pat = (Id_epn.tuple_of_number `Any  arity :> pat) in 
      Some %case-{ $pat:pat -> $default }
    else None in
  let names = names in
  let mk_record = mk_record in
  let cons_transform = cons_transform in
  let () = check names in
  let mk_tuple = mk_variant None in
  stru_of_mtyps
    ?cons_transform
    ?annot
    ~arity
    ~names
    ~default
    ~mk_variant
    ~left_type_id
    ~left_type_variable
    ~mk_record
    (normal_simple_exp_of_ctyp
       ~arity ~names ~mk_tuple
       ~right_type_id
       ~left_type_id ~right_type_variable)
    


    
let gen_object
    ?(arity=1)
    ?(default= %exp-{ failwith "arity >= 2 in other branches" } )
    ?cons_transform
    ~kind
    ~base
    ~class_name = 
  let make ?(names=[])  ~mk_record  ~mk_variant ()= 
    let () =  check names in
    let mk_tuple = mk_variant None in
    let left_type_variable  = `Pre "mf_" in
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
      ?cons_transform
      ~arity
      ~names
      ~default
      ~left_type_variable
      ~mk_record
      ~mk_variant
      base
      class_name
      (obj_simple_exp_of_ctyp
         ~right_type_id ~left_type_variable ~right_type_variable
         ~names ~arity ~mk_tuple  )
      ~kind in
  make


(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/derive.cmo" *)
(* end: *)
