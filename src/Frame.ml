open Ast;
(* This module builds a generic framework *)
#default_quotation "expr";;

open Format;
open LibUtil;
open Lib;
open Lib.Basic;
open FSig;  
open Lib.Expr;

(* preserved keywords for the generator *)
let preserve =  ["self"; "self_type"; "unit"; "result"];

let check names =
    (* we preserve some keywords to avoid variable capture *)
  List.iter (fun name ->
    if List.mem name preserve  then begin 
      eprintf "%s is not a valid name\n" name;
      eprintf "preserved keywords:\n";
      List.iter (fun s -> eprintf "%s\n" s) preserve;
      exit 2
    end
    else check_valid name) names;

(* collect the [partial evaluated Ast node]
   and meta data
   The input [y] is handled by
   [simple_expr_of_ctyp], generally it will
   be  exlcuding adt or variant type
 *)      
let mapi_expr ?(arity=1) ?(names=[]) (simple_expr_of_ctyp:ctyp->expr) (i:int) (y:ctyp)  : FSig.ty_info =
  with {patt:ctyp;expr}
  let name_expr = simple_expr_of_ctyp y in 
  let base = name_expr  +> names in
  (** FIXME as a tuple it is useful when arity> 1??? *)
  let id_exprs =
    (List.init arity (fun index  -> {| $(id:xid ~off:index i) |} )) in 
  let exp0 = List.hd id_exprs in 
  let id_patts =
    (List.init arity (fun index  -> {:patt| $(id:xid ~off:index i) |})) in 
  let pat0 = List.hd id_patts in
  let id_expr = Expr.tuple_of_list  id_exprs  in
  let id_patt = Patt.tuple_of_list id_patts in 
  let expr = apply base id_exprs  in
  {name_expr; expr; id_expr; id_exprs; id_patt; id_patts;exp0;pat0};       

(* @raise Invalid_argument when type can not be handled *)  
let tuple_expr_of_ctyp ?(arity=1) ?(names=[]) ~mk_tuple
    simple_expr_of_ctyp (ty:ctyp) : expr = with {patt:ctyp}
  match ty with
  [ {|  ($tup:t) |}  -> 
    let ls = FanAst.list_of_ctyp t [] in
    let len = List.length ls in
    let patt = Patt.mk_tuple ?arity ~number:len in
    let tys = List.mapi (mapi_expr ~arity ~names  simple_expr_of_ctyp) ls in
    names <+ (currying
                  [ {:match_case| $pat:patt -> $(mk_tuple tys ) |} ] ?arity)
  | _  -> invalid_arg &
      sprintf  "tuple_expr_of_ctyp {|%s|}\n" "" (*FIXME*)
        (* (Ctyp.to_string  ty) *)];
  
(*
 @supported types type application: list int
     basic type: int
     product type: (int * float * int)
 [m_list
 (fun _loc fmt ((a0, a1, a2), (b0, b1, b2)) ->
   ((m_int _loc fmt (a0, b0)), (m_float _loc fmt (a0, b0)),
    (m_float _loc fmt (a0, b0))))]
 return type is result
Plz supply current type [type list 'a] =>  [list]
 *)    
let rec  normal_simple_expr_of_ctyp
    ?arity ?names ~mk_tuple
    ~right_type_id ~left_type_id
    ~right_type_variable
    cxt ty = 
  let open Transform in
  (* let open ErrorMonad in *)
  let right_trans = transform right_type_id in
  let left_trans = basic_transform left_type_id in 
  let tyvar = right_transform right_type_variable  in 
  let rec aux = with {patt:ctyp;expr} fun 
    [ {| $lid:id |} -> 
      if Hashset.mem cxt id then {| $(lid:left_trans id) |}
      else right_trans {:ident| $lid:id |} 
    | {| $id:id |} ->   right_trans id
      (* recursive call here *)
    | {| $tup:_t |} as ty ->
        tuple_expr_of_ctyp  ?arity ?names ~mk_tuple
          (normal_simple_expr_of_ctyp
             ?arity ?names ~mk_tuple
             ~right_type_id ~left_type_id ~right_type_variable
             cxt) ty 
    | {| $t1 $t2 |} ->  {| $(aux t1) $(aux t2) |} 
    | {|  '$s |} ->   tyvar s
    | {|$t1 -> $t2 |} ->
        aux {:ctyp| arrow $t1 $t2 |} (* arrow is a keyword now*)
    | ty ->
        failwithf "normal_simple_expr_of_ctyp: %s type: \n " (Ctyp.to_string ty)] in
  aux ty;




(*
   list int ==>
      self#list (fun self -> self#int)
   list 'a  ==>
       self#list mf_a 
   'a  ==> (mf_a self)

  list (list 'a) ==>
      self#list (fun self -> (self#list mf_a))

  m_list (tree 'a) ==>
      self#m_list (fun self -> self#tree mf_a)
 *)      
let rec obj_simple_expr_of_ctyp
    ~right_type_id
    ~left_type_variable
    ~right_type_variable
    ?names ?arity ~mk_tuple
      ty = with {patt:ctyp}
  let open Transform in 
  let trans = transform right_type_id in
  let var = basic_transform left_type_variable in
  let tyvar = right_transform right_type_variable  in 
  let rec aux = fun
    [ {| $id:id |} -> trans id
    | {|  '$s |} ->   tyvar s
    | {| $_ $_ |} as ty ->
        match  Ctyp.list_of_app ty  with
        [ [ {| $id:tctor |} :: ls ] ->
          ls |> List.map
            (fun [ {|  '$s |} -> {:expr| $(lid:var s) |} 
                 | t ->   {:expr| fun self -> $(aux t) |} ])
             |> apply (trans tctor)
        | _  -> invalid_arg "list_of_app in obj_simple_expr_of_ctyp"]
    | {|$t1 -> $t2 |} -> 
        aux {:ctyp| $(lid:"arrow") $t1 $t2 |} 
    | {|  $tup:_  |} as ty ->
        tuple_expr_of_ctyp
          ?arity ?names ~mk_tuple
          (obj_simple_expr_of_ctyp
             ~right_type_id
             ~left_type_variable
             ~right_type_variable
             ?names
             ?arity
             ~mk_tuple) ty 
    | ty -> failwithf "obj_simple_expr_of_ctyp %s\n" (Ctyp.to_string ty) ] in
  aux ty ;

(*
  accept [simple_expr_of_ctyp]
  call [reduce_data_ctors]  for variant types
  assume input is  variant type
  accept variant input type to generate  a function expression 
 *)  
let expr_of_ctyp ?cons_transform ?(arity=1) ?(names=[]) ~trail ~mk_variant
    simple_expr_of_ctyp (ty:ctyp)  = with {patt:ctyp}
  let f  cons tyargs acc : list match_case = 
      let args_length = List.length tyargs in  (* ` is not needed here *)
        let p =
          (* calling gen_tuple_n*)
          Patt.gen_tuple_n ?cons_transform ~arity  cons args_length in
          (* Fan_expr.gen_curry_n acc ?arity:S.arity cons args_length in  *)
        let mk (cons,tyargs) =
          let exprs = List.mapi (mapi_expr ~arity ~names
                                   simple_expr_of_ctyp) tyargs in
          mk_variant cons exprs in
      let e = mk (cons,tyargs) in
      [ {:match_case| $pat:p -> $e |} :: acc ] in  begin 
  let info =
    match ty with
    (* FIXME TyVrnInfSup to be added *)
    [ {|  [ $t]  |}  -> (TyVrn, List.length (FanAst.list_of_ctyp t []))
    | {| [= $t ] |} -> (TyVrnEq, List.length (FanAst.list_of_ctyp t []))
    | {| [> $t ] |} -> (TyVrnSup,List.length (FanAst.list_of_ctyp t []))
    | {| [< $t ] |} -> (TyVrnInf,List.length (FanAst.list_of_ctyp t []))
    | _ ->
        invalid_arg
          (sprintf "expr_of_ctyp {|%s|} " "" (*FIXME*)
             (* & Ctyp.to_string ty *)) ] in 
  let res = Ctyp.reduce_data_ctors ty  [] f (* >>= (fun res -> *) in
  let res =
    let t =
      (* only under this case we need trailing  *)
      if List.length res >= 2 && arity >= 2 then
        [ trail info :: res ]
      else res in
    List.rev t in 
  currying ?arity res 
  end;

(* return a [expr] node  *)  
let expr_of_variant ?cons_transform ?(arity=1)?(names=[]) ~trail ~mk_variant  (* ~destination *)
    simple_expr_of_ctyp result ty = with {patt:ctyp;expr:match_case}
  let f (cons,tyargs) :  match_case=
    let len = List.length tyargs in
    let p = Patt.gen_tuple_n ?cons_transform ~arity cons len in
    let mk (cons,tyargs) =
      let exps = List.mapi (mapi_expr ~arity ~names simple_expr_of_ctyp) tyargs in
      mk_variant cons exps in
    let e = mk (cons,tyargs) in
    {| $pat:p -> $e |} in 
  (* for the case [`a | b ] *)
  let simple lid :match_case=
    let e = (simple_expr_of_ctyp {:ctyp|$id:lid|}) +> names  in
    (* let ty = Ctyp.mk_dest_type k *)
    (* let (name,len) = Ctyp.name_length_of_tydcl tydcl in  *)
    (* let ty = Ctyp.mk_dest_type destination ({:ident|$lid:name|},len) in *)
    MatchCase.gen_tuple_abbrev ~arity result lid e in
  (* FIXME, be more precise  *)
  let info = (TyVrnEq, List.length (FanAst.list_of_ctyp ty [])) in
  let ls = Ctyp.view_variant ty in
  let res =
    let res = List.fold_left
      (fun  acc x ->
        match x with
        [ (`variant (cons,args)) -> [f ("`"^cons,args)::acc]
        | `abbrev (lid) ->  [simple lid :: acc ] ])  [] ls in
  let t =
    if List.length res >= 2 && arity >= 2 then
      [trail info :: res]
    else res in
  List.rev t in
  currying ?arity res ;
  
let mk_prefix  vars (acc:expr) ?(names=[])  ~left_type_variable=
  with {patt:ctyp}
  let open Transform in 
  let varf = basic_transform left_type_variable in
  let  f var acc = match var with
  [ {| +' $s |} | {| -' $s |}
  | {| ' $s |} ->
      {| fun $(lid: varf s) -> $acc |}
  | _ -> begin  Ctyp.eprint var ;
   invalid_arg "mk_prefix";end ] in
  List.fold_right f vars ( names <+ acc);

(*
  Given type declarations, generate corresponding
  Ast node represent the [function]
  (combine both expr_of_ctyp and simple_expr_of_ctyp) *)  
let fun_of_tydcl
    ?(names=[]) ?(arity=1) ~left_type_variable ~mk_record ~destination
    simple_expr_of_ctyp expr_of_ctyp expr_of_variant  tydcl :expr =   with {patt:ctyp}  
      
      let  (name,len) = Ctyp.name_length_of_tydcl tydcl in
      let result_type = Ctyp.mk_dest_type ~destination ({:ident|$lid:name|},len) in
        match tydcl with 
        [ `TyDcl (_, _, tyvars, ctyp, _constraints) ->
      let ctyp =
        match ctyp with
        [ ( {| $_ == $ctyp |} (* the latter reifys the structure is used here *)
        |  {| private $ctyp |} ) -> ctyp
        | _ -> ctyp ] in
        match ctyp with (* FIXME the error message is wrong when using lang_at *)
        [ {|  { $t}  |} ->       
          let cols =  Ctyp.list_of_record t  in
          let patt = Patt.mk_record ~arity  cols in
          let info =
          List.mapi
            (fun i x ->  match x with
              [ {label;is_mutable;ctyp} ->
                     {info = (mapi_expr ~arity ~names simple_expr_of_ctyp) i ctyp  ;
                      label = label;
                      is_mutable = is_mutable}
              ] ) cols in
          (* For single tuple pattern match this can be optimized
             by the ocaml compiler *)
        mk_prefix ~names ~left_type_variable tyvars
            (currying ~arity [ {:match_case| $pat:patt -> $(mk_record info)  |} ])
      | {| $id:_|} | {| $tup:_|} | {| $_ $_ |} | {| '$_ |} | {| $_ -> $_ |} ->
          let expr = simple_expr_of_ctyp ctyp in
          let funct = eta_expand (expr+>names) arity  in
          mk_prefix ~names ~left_type_variable tyvars funct

      | {| [= $t]|} | {| [>$t] |} | {| [< $t ]|} | {| [< $t > $_ ]|} ->
          
          let case =  expr_of_variant result_type t (* result_type *) in
          mk_prefix ~names ~left_type_variable tyvars case
         (* FIXME be more precise *)   
      | _ ->
          let funct = expr_of_ctyp ctyp in  
          (* for [expr_of_ctyp]
             appending names was delayed to be
             handled in mkcon *)
          mk_prefix ~names ~left_type_variable tyvars funct ]
  | _tydcl -> 
      failwithf  "fun_of_tydcl <<%s>>\n" (Ctyp.to_string _tydcl) ];


let binding_of_tydcl ?cons_transform simple_expr_of_ctyp
    tydcl ?(arity=1) ?(names=[]) ~trail ~mk_variant
    ~left_type_id ~left_type_variable
    ~mk_record (* ~destination *)
     : binding
    = with {patt:ctyp}

  let open Transform in 
  let tctor_var = basic_transform left_type_id in
  let (name,len) = Ctyp.name_length_of_tydcl tydcl in 
  let ty = Ctyp.mk_method_type_of_name
      ~number:arity ~prefix:names (name,len) Str_item in
  if not ( Ctyp.is_abstract tydcl) then 
    let fun_expr =
      fun_of_tydcl (* ~destination *) ~destination:Str_item
        ~names ~arity ~left_type_variable ~mk_record 
        simple_expr_of_ctyp
        (expr_of_ctyp
           ?cons_transform ~arity ~names ~trail ~mk_variant simple_expr_of_ctyp)
        (expr_of_variant
           ?cons_transform ~arity ~names ~trail ~mk_variant simple_expr_of_ctyp) tydcl  in
    {:binding| $(lid:tctor_var name) : $ty = $fun_expr |}
  else begin
    eprintf "Warning: %s as a abstract type no structure generated\n"
      (Ctyp.to_string tydcl);
    {:binding| $(lid:tctor_var  name) =
    failwithf $(str:"Abstract data type not implemented") |};
  end ;

let str_item_of_module_types ?module_name ?cons_transform
    ?arity ?names ~trail ~mk_variant ~left_type_id ~left_type_variable
    ~mk_record
    simple_expr_of_ctyp_with_cxt
    (lst:module_types)  =
  let cxt  = Hashset.create 50 in 
  let mk_binding (* : string -> ctyp -> binding *) =
    binding_of_tydcl ?cons_transform ?arity
      ?names ~trail ~mk_variant ~left_type_id ~left_type_variable ~mk_record
      (simple_expr_of_ctyp_with_cxt cxt) in
  (* return new types as generated  new context *)
  let fs (ty:types) : str_item= match ty with
    [ `Mutual named_types ->
      let binding = match named_types with
        [ [] -> {:binding| |}
        | xs -> begin 
            List.iter (fun (name,_ty)  -> Hashset.add cxt name) xs ;
            List.reduce_right_with
              ~compose:(fun x y -> {:binding| $x and $y |} )
              ~f:(fun (_name,ty) ->begin
                mk_binding  ty;
              end ) xs
        end ] in 
      {:str_item| let rec $binding |} 
    | `Single (name,tydcl) -> begin 
        Hashset.add cxt name;
        let rec_flag =
          if Ctyp.is_recursive tydcl then `Recursive _loc
          else `ReNil  _loc
        and binding = mk_binding  tydcl in 
        {:str_item| let $rec:rec_flag  $binding |}
    end ] in
  let item =  {:str_item| $(list:List.map fs lst) |}  in
  match module_name with
  [ None -> item
  | Some m -> {:str_item| module $uid:m = struct $item end |} ];


 (*
   Generate warnings for abstract data type
   and qualified data type.
   all the types in one module will derive a class 
  *)
let obj_of_module_types
    ?cons_transform
    ?module_name
    ?(arity=1) ?(names=[]) ~trail  
    ~left_type_variable
    ~mk_record
    ~mk_variant
     base
    class_name  simple_expr_of_ctyp (k:kind) (lst:module_types) = with {patt:ctyp}
  let tbl = Hashtbl.create 50 in 
    let f  =
      fun_of_tydcl ~names ~destination:(Obj k)
        ~arity ~left_type_variable
        ~mk_record 
        simple_expr_of_ctyp
        (expr_of_ctyp ?cons_transform
           ~arity ~names
           ~trail ~mk_variant
           simple_expr_of_ctyp)
        (expr_of_variant ?cons_transform
           ~arity ~names
           ~trail ~mk_variant
           simple_expr_of_ctyp) in
    let mk_type tydcl =
        let (name,len) = Ctyp.name_length_of_tydcl tydcl in
        Ctyp.mk_method_type ~number:arity ~prefix:names ({:ident| $lid:name |} ,len ) (Obj k) in 
    let mk_class_str_item (name,tydcl) : class_str_item = 
      let ty = mk_type tydcl in
      {:class_str_item| method $lid:name : $ty = $(f tydcl) |}  in 
    let fs (ty:types) =
      match ty with
      [ `Mutual named_types ->
        {:class_str_item| $(list: List.map mk_class_str_item named_types ) |}
      | `Single ((name,tydcl) as  named_type) ->
         match Ctyp.abstract_list tydcl with
         [ Some n  -> begin
           let ty_str =  (* (Ctyp.to_string tydcl) FIXME *) "" in
           let () = Hashtbl.add tbl ty_str (Abstract ty_str) in 
           let ty = mk_type tydcl in
           {:class_str_item| method $lid:name : $ty= $(unknown n) |}
         end
         | None ->  mk_class_str_item named_type ]] in 
      (* Loc.t will be translated to loc_t
       we need to process extra to generate method loc_t
     *)
    let (extras,lst) = Ctyp.transform_module_types lst in 
    let body = List.fold_left 
        (fun acc types -> {:class_str_item| $acc; $(fs types) |} )
        ({:class_str_item| |}) lst in
    let body =
      let items = List.map (fun (dest,src,len) ->
        let ty = Ctyp.mk_method_type ~number:arity ~prefix:names (src,len) (Obj k) in
        let () = Hashtbl.add tbl dest (Qualified dest) in 
        {:class_str_item| method
            $lid:dest : $ty = $(unknown len) |} ) extras in
      {:class_str_item| $body ; $list:items |} in  begin 
      let v = Ctyp.mk_obj class_name  base body;
      Hashtbl.iter (fun _ v ->
        eprintf "%a" FSig.pp_print_warning_type  v)
      tbl;
      match module_name with
      [None -> v
      |Some u -> {:str_item| module $uid:u = struct $v  end  |} ]  
      end ;
  
  

(*   check S.names; *)





















