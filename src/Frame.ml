open Ast;
(* This module builds a generic framework *)

#default_quotation "expr";;



open Format;
open LibUtil;
open Lib;
open Lib.Basic;
module Ast = FanAst;
open FSig;  

module Make(S:FSig.Config) = struct   
  open Expr;
  open Ident;

  (* we preserve some keywords to avoid variable capture *)
  List.iter (fun name ->
    if List.mem name preserve  then begin 
      eprintf "%s is not a valid name\n" name;
      eprintf "preserved keywords:\n";
      List.iter (fun s -> eprintf "%s\n" s) preserve;
      exit 2
    end
    else check_valid name) S.names;
  

  (* collect the partial evaluated Ast node  and meta data   *)      
  let mapi_expr simple_expr_of_ctyp i (y:ctyp)  = with {"patt":"ctyp"}
    let name_expr = simple_expr_of_ctyp y in 
    let base = name_expr  +> S.names in
    (** FIXME as a tuple it is useful when arity> 1??? *)
    let id_exprs =
      (List.init S.arity (fun index  -> {| $(id:xid ~off:index i) |} ))
    and id_patts =
      (List.init S.arity (fun index  -> {:patt| $(id:xid ~off:index i) |}))in
    let id_expr = Expr.tuple_of_list  id_exprs  in
    let id_patt = Patt.tuple_of_list id_patts in 
    let expr = apply base id_exprs  in
    {name_expr; expr; id_expr; id_exprs; id_patt; id_patts};       

  (* @raise Invalid_argument when type can not be handled  *)  
  let tuple_expr_of_ctyp simple_expr_of_ctyp ty = with {"patt":"ctyp"}
    let open ErrorMonad in 
    let simple_expr_of_ctyp = unwrap simple_expr_of_ctyp in 
    match ty with
    [ {|  ($tup:t) |}  -> 
      let ls = FanAst.list_of_ctyp t [] in
      let len = List.length ls in
      let patt = Patt.mk_tuple ~arity:S.arity ~number:len in
      let tys = List.mapi (mapi_expr  simple_expr_of_ctyp) ls in
      S.names <+ (currying
                    [ {:match_case| $pat:patt -> $(S.mk_tuple tys ) |} ] ~arity:S.arity)
    | _  -> invalid_arg &
        sprintf  "tuple_expr_of_ctyp {|%s|}\n" "" (*FIXME*)
          (* (!Ctyp.to_string  ty) *)];


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
  let rec  normal_simple_expr_of_ctyp cxt ty = with {"patt":"ctyp"}
    let open Transform in
    let open ErrorMonad in
    let right_trans = transform S.right_type_id in
    let left_trans = basic_transform S.left_type_id in 
    let tyvar = right_transform S.right_type_variable  in 
    let rec aux = fun 
      [ {| $lid:id |} -> 
        if Hashset.mem cxt id then {| $(lid:left_trans id) |}
        else right_trans {:ident| $lid:id |} 
      | {| $id:id |} ->   right_trans id
        (* recursive call here *)
      | {| $tup:_t |} as ty ->
          tuple_expr_of_ctyp  (normal_simple_expr_of_ctyp cxt) ty 
      | {| $t1 $t2 |} ->  {:expr| $(aux t1) $(aux t2) |} 
      | {|  ' $s |} ->   tyvar s
      | {|$t1 -> $t2 |} -> 
          aux {:ctyp| $(lid:"arrow") $t1 $t2 |} 
      | ty ->  raise (Unhandled  ty ) ] in
    try return & aux ty with
      [Unhandled _t ->
        fail & sprintf "normal_simple_expr_of_ctyp inner:{|%s|} outer:{|%s|}\n"
          "" ""
          (* (!Ctyp.to_string t) (!Ctyp.to_string ty) *) ] ;


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
        ty = with {"patt":"ctyp"}
    let open Transform in 
    ErrorMonad.(
    let trans = transform S.right_type_id in
    let var = basic_transform S.left_type_variable in
    let tyvar = right_transform S.right_type_variable  in 
    let rec aux = fun
      [ {| $id:id |} -> trans id
      | {|  '$s |} ->   tyvar s
      | {:ctyp| $_  $_ |} as ty ->
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
          tuple_expr_of_ctyp  (obj_simple_expr_of_ctyp ) ty 
      | ty -> raise (Unhandled ty) ] in
    try return & aux ty with
      [Unhandled _t0 -> fail &
        sprintf
          "obj_simple_expr_of_ctyp inner:{|%s|} outer:{|%s|}\n"
          "" "" (* FIXME *)
          (* (!Ctyp.to_string t0) (!Ctyp.to_string ty) *) ] );
        
  (*
    accept [simple_expr_of_ctyp]
    call [reduce_data_ctors]  for variant types
    assume input is  variant type
    accept variant input type to generate  a function expression 
   *)  
  let expr_of_ctyp  simple_expr_of_ctyp (ty:ctyp)  = with {"patt":"ctyp"}
    let open ErrorMonad in 
    let f  cons tyargs acc = 
        let args_length = List.length tyargs in  (* ` is not needed here *)
          let p =
            (* calling gen_tuple_n*)
            Patt.gen_tuple_n ?cons_transform:S.cons_transform ~arity:S.arity  cons args_length in
            (* Fan_expr.gen_curry_n acc ~arity:S.arity cons args_length in  *)
          let mk (cons,tyargs) =
            let exprs = List.mapi (mapi_expr simple_expr_of_ctyp) tyargs in
            S.mk_variant cons exprs in
        let e = mk (cons,tyargs) in
        [ {:match_case| $pat:p -> $e |} :: acc ] in 
        (* {:match_case| $acc$ | $p$ -> $e$  |} in *)
    let info = match ty with
      (* FIXME TyVrnInfSup to be added *)
      [ {|  [ $t]  |}  -> (TyVrn, List.length (FanAst.list_of_ctyp t []))
      | {| [= $t ] |} -> (TyVrnEq, List.length (FanAst.list_of_ctyp t []))
      | {| [> $t ] |} -> (TyVrnSup,List.length (FanAst.list_of_ctyp t []))
      | {| [< $t ] |} -> (TyVrnInf,List.length (FanAst.list_of_ctyp t []))
      | _ ->
          invalid_arg
            (sprintf "expr_of_ctyp {|%s|} " "" (*FIXME*)
               (* & !Ctyp.to_string ty *)) ] in 
    Ctyp.reduce_data_ctors ty  [] f >>= (fun res ->
      let res = let t =
        (* only under this case we need trailing  *)
        if List.length res >= 2 && S.arity >= 2 then
          [ S.trail info :: res ]
        else res in
      List.rev t in 
      return (currying ~arity:S.arity res ));


  let mk_prefix  vars (acc:expr)  = with {"patt":"ctyp"}
    let open Transform in 
    let varf = basic_transform S.left_type_variable in
    let  f var acc = match var with
    [ {| +' $s |} | {| -' $s |}
    | {| ' $s |} ->
        {| fun $(lid: varf s) -> $acc |}
    | _ -> begin  !Ctyp.eprint var ;
     invalid_arg "mk_prefix";end ] in
    List.fold_right f vars ( S.names <+ acc);

  (*
    Given type declarations, generate corresponding
    Ast node represent the [function]
    (combine both expr_of_ctyp and simple_expr_of_ctyp) *)  
  let fun_of_tydcl simple_expr_of_ctyp expr_of_ctyp  = with {"patt":"ctyp"}
    let open ErrorMonad in fun
    [ `TyDcl (_, _, tyvars, ctyp, _constraints) ->
        let ctyp =
          match ctyp with
          [ ( {| $_ == $ctyp |} (* the latter reifys the structure is used here *)
          |  {| private $ctyp |} ) -> ctyp
          | _ -> ctyp ] in
        match ctyp with
        [ {|  { $t}  |} ->
          (* FIXME the error message is wrong when using lang_at *)
          let cols =  Ctyp.list_of_record t  in
          let patt = Patt.mk_record ~arity:S.arity  cols in
          let info =
            List.mapi
              (fun i x ->  match x with
                [ {label;is_mutable;ctyp} ->
                       {info = (mapi_expr
                           (unwrap simple_expr_of_ctyp)) (* unwrap here *)
                          i ctyp  ;
                        label = label;
                        is_mutable = is_mutable}
                ] ) cols in
            (* For single tuple pattern match this can be optimized
               by the ocaml compiler
             *)
          mk_prefix
            tyvars
            (currying ~arity:S.arity [ {:match_case| $pat:patt -> $(S.mk_record info)  |} ])
        | _ ->
            let process =
              (fun ctyp ->
                  simple_expr_of_ctyp ctyp >>=
                  (fun expr ->
                    return & eta_expand (expr+>S.names) S.arity ))
                (* t -> pp_print_t -> fun (a,b) -> pp_print_t (a,b) *)
                <|>  expr_of_ctyp in 
                  (* for [expr_of_ctyp]
                     appending names was delayed to be
                       handled in mkcon *)
            let funct =
              match process ctyp  with
              [ Left result  ->  result
              | Right str ->
                   invalid_arg
                     (sprintf "fun_of_tydcl{|%s|}\n%s" "" (*FIXME*)
                                  (* (!Ctyp.to_string ctyp) *) str)] in
            mk_prefix tyvars funct ]
    | _tydcl -> 
        invalid_arg
          ( sprintf "fun_of_tydcl <<%s>>\n"
              ""
              (*FIXME*)
              (* (!Ctyp.to_string tydcl) *)) ];

  (* *)    
  let binding_of_tydcl simple_expr_of_ctyp _name tydcl = with {"patt":"ctyp"}
    let open ErrorMonad in
    let open Transform in 
    let tctor_var = basic_transform S.left_type_id in
    let (name,len) = Ctyp.name_length_of_tydcl tydcl in 
    let ty = Ctyp.mk_method_type_of_name
        ~number:S.arity ~prefix:S.names (name,len) Str_item in
    if not & Ctyp.is_abstract tydcl then 
      let fun_expr =
        fun_of_tydcl simple_expr_of_ctyp
          (expr_of_ctyp (unwrap simple_expr_of_ctyp)) tydcl  in
      {:binding| $(lid:tctor_var name) : $ty = $fun_expr |}
    else begin
      eprintf "Warning: %s as a abstract type no structure generated\n" "" (*FIXME*)
        (* (!Ctyp.to_string tydcl) *);
      {:binding| $(lid:tctor_var  name) =
      failwithf $(str:"Abstract data type not implemented") |};
    end ;

  let str_item_of_module_types ?module_name
      simple_expr_of_ctyp_with_cxt
      (lst:module_types)  =
    let cxt  = Hashset.create 50 in 
    let mk_binding =
      binding_of_tydcl (simple_expr_of_ctyp_with_cxt cxt) in
    (* return new types as generated  new context *)
    let fs (ty:types) = match ty with
      [ `Mutual named_types ->
        let binding = match named_types with
          [ [] -> {:binding| |}
          | xs -> begin 
              List.iter (fun (name,_ty)  -> Hashset.add cxt name) xs ;
              List.reduce_right_with
                ~compose:(fun x y -> {:binding| $x and $y |} )
                ~f:(fun (name,ty) ->begin
                  mk_binding name ty;
                end ) xs
          end ] in 
        {:str_item| let rec $binding |} 
      | `Single (name,tydcl) -> begin 
          Hashset.add cxt name;
          let rec_flag =
            if Ctyp.is_recursive tydcl then `ReRecursive
            else `ReNil 
          and binding = mk_binding name tydcl in 
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
  let obj_of_module_types ?module_name base class_name  simple_expr_of_ctyp
      (k:FSig.k) (lst:module_types) = with {"patt":"ctyp"}
    let open ErrorMonad in 
    let tbl = Hashtbl.create 50 in 
      let f  = fun_of_tydcl simple_expr_of_ctyp
          (expr_of_ctyp (unwrap simple_expr_of_ctyp)) in
      let mk_type (_name,tydcl) =
          let (name,len) = Ctyp.name_length_of_tydcl tydcl in
          Ctyp.mk_method_type ~number:S.arity ~prefix:S.names
            ({:ident| $lid:name |} ,len ) (Obj k) in 
      let mk_class_str_item (name,tydcl) = 
        let ty = mk_type (name,tydcl) in
        {:class_str_item| method $lid:name : $ty = $(f tydcl) |}  in 
      let fs (ty:types) =
        match ty with
        [ `Mutual named_types ->
          {:class_str_item| $(list: List.map mk_class_str_item named_types ) |}
        | `Single ((name,tydcl) as  named_type) ->
           match Ctyp.abstract_list tydcl with
           [ Some n  -> begin
             let ty_str =  (* (!Ctyp.to_string tydcl) FIXME *) "" in
             let () = Hashtbl.add tbl ty_str (Abstract ty_str) in 
             let ty = mk_type (name,tydcl) in
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
          let ty = Ctyp.mk_method_type ~number:S.arity
              ~prefix:S.names (src,len) (Obj k) in
          let () = Hashtbl.add tbl dest (Qualified dest) in 
          {:class_str_item| method
              $lid:dest : $ty = $(unknown len) |} ) extras in
        {:class_str_item| $body ; $list:items |} in  begin 
        let v = Ctyp.mk_obj class_name  base body;
        Hashtbl.iter (fun _ v -> eprintf "%s" (string_of_warning_type v))
        tbl;
        match module_name with
        [None -> v
        |Some u -> {:str_item| module $uid:u = struct $v  end  |} ]  
        end ;
    
end;




















