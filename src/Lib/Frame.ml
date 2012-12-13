
(* This module builds a generic framework *)

(* <:fan< *)
(* lang "expr"; *)
(* lang_at "patt" "ctyp"; *)
(* >>; *)

#default_quotation "expr";;
#lang_at "patt" "ctyp";;
open Format;
open LibUtil;
module Ast = Camlp4Ast;
(* <:include_ml< "open_template.ml"; >> ; *)






module Make(S:FSig) = struct   
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
  let mapi_expr simple_expr_of_ctyp i (y:Ast.ctyp)  =
    let ty_name_expr = simple_expr_of_ctyp y in 
    let base = ty_name_expr  +> S.names in
    (** FIXME as a tuple it is useful when arity> 1??? *)
    let ty_id_exprs =
      (init S.arity (fun index  -> {| $(id:xid ~off:index i) |} ))
    and ty_id_patts =
      (init S.arity (fun index  -> <:patt< $(id:xid ~off:index i) >>))in
    let ty_id_expr = Expr.tuple_of_list  ty_id_exprs  in
    let ty_id_patt = Patt.tuple_of_list ty_id_patts in 
    let ty_expr = apply base ty_id_exprs  in
    {ty_name_expr; ty_expr; ty_id_expr; ty_id_exprs; ty_id_patt; ty_id_patts};       

  (* @raise Invalid_argument when type can not be handled  *)  
  let tuple_expr_of_ctyp simple_expr_of_ctyp ty = ErrorMonad.(
    let simple_expr_of_ctyp = unwrap simple_expr_of_ctyp in 
    match ty with
    [ {|  (.$tup:t$.) |}  -> 
      let ls = Ast.list_of_ctyp t [] in
      let len = List.length ls in
      let patt = Patt.mk_tuple ~arity:S.arity ~number:len in
      let tys = mapi (mapi_expr  simple_expr_of_ctyp) ls in
      S.names <+ (currying
                    [ <:match_case< .$patt$. ->
                      .$S.mk_tuple tys $. >> ] ~arity:S.arity)
    | _  -> invalid_arg &
        sprintf  "tuple_expr_of_ctyp {|%s|}\n" (!Ctyp.to_string  ty)]);


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
  let rec  normal_simple_expr_of_ctyp cxt ty =
    let open Transform in
    ErrorMonad.(
    let right_trans = transform S.right_type_id in
    let left_trans = basic_transform S.left_type_id in 
    let tyvar = right_transform S.right_type_variable  in 
    let rec aux = fun 
      [ {| .$lid:id$. |} -> 
        if Hashset.mem cxt id then {| .$lid:left_trans id$. |}
        else right_trans <:ident< .$lid:id$. >> 
      | {| .$id:id$. |} ->   right_trans id
        (* recursive call here *)
      | {| (.$tup:t$.) |} as ty ->
          tuple_expr_of_ctyp  (normal_simple_expr_of_ctyp cxt) ty 
      | {| .$t1$. .$t2$. |} ->  <:expr< .$aux t1$. .$aux t2$. >> 
      | {|  '.$s$. |} ->   tyvar s
      | {| .$t1$. -> .$t2$. |} -> 
          aux <:ctyp< .$lid:"arrow"$. .$t1$. .$t2$. >> 
      | ty ->  raise (Unhandled  ty ) ] in
    try return & aux ty with
      [Unhandled t ->
        fail & sprintf "normal_simple_expr_of_ctyp inner:{|%s|} outer:{|%s|}\n"
          (!Ctyp.to_string t) (!Ctyp.to_string ty) ])
  ;


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
        ty =
    let open Transform in 
    ErrorMonad.(
    let trans = transform S.right_type_id in
    let var = basic_transform S.left_type_variable in
    let tyvar = right_transform S.right_type_variable  in 
    let rec aux = fun
      [ {| .$id:id$. |} -> trans id
      | {|  '.$s$. |} ->   tyvar s
      | {| .$_$. .$_$. |} as ty -> match  Ctyp.list_of_app ty  with
          [ [ {| .$id:tctor$. |} :: ls ] ->
            ls |> List.map (fun [ {|  '.$s$. |} -> <:expr< .$lid:var s$. >> 
                                | t ->   <:expr< fun self -> .$aux t$. >> ])
               |> apply (trans tctor)
          | _  -> invalid_arg "list_of_app in obj_simple_expr_of_ctyp"]
      | {| .$t1$. -> .$t2$. |} -> 
          aux <:ctyp< .$lid:"arrow"$. .$t1$. .$t2$. >> 
      | {|  .$tup:_$.  |} as ty ->
          tuple_expr_of_ctyp  (obj_simple_expr_of_ctyp ) ty 
      | ty -> raise (Unhandled ty) ] in
    try return & aux ty with
      [Unhandled t0 -> fail &
        sprintf "obj_simple_expr_of_ctyp inner:{|%s|} outer:{|%s|}\n"
          (Ctyp.to_string t0) (Ctyp.to_string ty) ] );
        
  (*
    call [reduce_data_ctors]  for variant types
    assume input is  variant type
    accept variant input type to generate  a function expression 
   *)  
  let expr_of_ctyp  simple_expr_of_ctyp (ty:Ast.ctyp)  =
    let open ErrorMonad in 
    let f  cons tyargs acc = 
        let args_length = List.length tyargs in  (* ` is not needed here *)
          let p =
            Patt.gen_tuple_n ~arity:S.arity  cons args_length in
            (* Fan_expr.gen_curry_n acc ~arity:S.arity cons args_length in  *)
          let mk (cons,tyargs) =
            let exprs = mapi (mapi_expr simple_expr_of_ctyp) tyargs in
            S.mk_variant cons exprs in
        let e = mk (cons,tyargs) in
        [ <:match_case< .$p$. -> .$e$. >> :: acc ] in 
        (* <:match_case< $acc$ | $p$ -> $e$  >> in *)
    let info = match ty with
      (* FIXME TyVrnInfSup to be added *)
      [ {|  [.$t$.]  |}  -> (TyVrn, List.length (Ast.list_of_ctyp t []))
      | {| [= .$t$. ] |} -> (TyVrnEq, List.length (Ast.list_of_ctyp t []))
      | {| [> .$t$. ] |} -> (TyVrnSup,List.length (Ast.list_of_ctyp t []))
      | {| [< .$t$. ] |} -> (TyVrnInf,List.length (Ast.list_of_ctyp t []))
      | _ ->  invalid_arg  (sprintf "expr_of_ctyp {|%s|} "
                              & Ctyp.to_string ty) ] in 
    Ctyp.reduce_data_ctors ty  [] f >>= (fun res ->
      let res = let t =
        (* only under this case we need trailing  *)
        if List.length res >= 2 && S.arity >= 2 then
          [ S.trail info :: res ]
        else res in
      List.rev t in 
      return (currying ~arity:S.arity res ));


  let mk_prefix  vars (acc:Ast.expr)  =
    let open Transform in 
    let varf = basic_transform S.left_type_variable in
    let  f var acc = match var with
    [ <@_loc< +'.$s$. >> | <@_loc< -'.$s$. >>
    | <@_loc<  '.$s$. >> ->
        {| fun .$lid: varf s $. -> .$acc$. |}
    | _ -> do { Ctyp.eprint var ;
     invalid_arg "mk_prefix";} ] in
    List.fold_right f vars ( S.names <+ acc)
  ;

  (*
    Given type declarations, generate corresponding
    Ast node represent the function
    (combine both expr_of_ctyp and simple_expr_of_ctyp) *)  
  let fun_of_tydcl simple_expr_of_ctyp expr_of_ctyp  = let open ErrorMonad in fun
    [ Ast.TyDcl _ _ tyvars ctyp constraints ->
        let ctyp =  match ctyp with
        [
         ( {| .$_$. == .$ctyp$. |} (* the latter reifys the structure *)
        | {| private .$ctyp$. |} ) -> ctyp | _ -> ctyp ] in
        match ctyp with
        [ {|  {.$t$.}  |} -> 
          let cols =  Ctyp.list_of_record t  in
          let patt = Patt.mk_record ~arity:S.arity  cols in
          let info =
            mapi (fun i x ->  match x with
                [ {col_label;col_mutable;col_ctyp} ->
                       {record_info = (mapi_expr
                           (unwrap simple_expr_of_ctyp)) (* unwrap here *)
                          i col_ctyp  ;
                        record_label = col_label;
                        record_mutable = col_mutable}
                ] ) cols in
            (* For single tuple pattern match this can be optimized
               by the ocaml compiler
             *)
          mk_prefix tyvars
            (currying ~arity:S.arity
               [ <:match_case< .$patt$. -> .$S.mk_record info$.  >> ])
        | _ ->
            let process =
              (fun ctyp ->
                  simple_expr_of_ctyp ctyp >>= (fun expr ->
                    return & eta_expand (expr+>S.names) S.arity ))
                <|>  expr_of_ctyp in 
                  (* for [expr_of_ctyp]
                     appending names was delayed to be
                       handled in mkcon *)
            let funct =  match process ctyp  with
               [ Left result  ->  result
               | Right str ->
                   invalid_arg (sprintf "fun_of_tydcl{|%s|}\n%s"
                               (Ctyp.mk_obj class_name  base body;
        Hashtbl.iter (fun _ v -> eprintf "%s" (string_of_warning_type v))
        tbl;
        match module_name with
        [None -> v
        |Some u -> <:str_item< module .$uid:u$. = struct .$ v $. end  >> ]  
         } ;
end;




















