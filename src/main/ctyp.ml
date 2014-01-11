(* %%control{default "ctyp-";};; *)

open Astfn
open Astn_util
open Util


type vrn =
  | Sum 
  | TyVrnEq
  | TyVrnSup
  | TyVrnInf
  | TyVrnInfSup
  | TyAbstr
      



type col = {
    label:string;
    is_mutable:bool;
    ty:ctyp
  }

type ty_info = {
    name_exp: exp;
    info_exp: exp; 
    ep0: ep;
    id_ep: ep; 
    id_eps: ep list ;
    ty: ctyp;
  }



let mapi_exp
    ?(arity=1)
    ?(names=[])
    ~f:(f:(ctyp->exp))
    (i:int)
    (ty : ctyp)  :
    ty_info =
  let name_exp = f ty in 
  let base = apply_args name_exp  names in
  (* FIXME as a tuple it is useful when arity> 1??? *)
  let id_eps = Listf.init arity @@ fun index  -> Id.xid ~off:index i  in
  let ep0 = List.hd id_eps in
  let id_ep = tuple_com  id_eps  in
  let exp = appl_of_list (base:: (id_eps:>exp list))  in
  {name_exp;
   info_exp=exp;
   id_ep;
   id_eps;
   ep0;
   ty
 }


let tuple_exp_of_ctyp ?(arity=1) ?(names=[]) ~mk_tuple
    ~f (ty:ctyp) : exp =
  match ty with
  | `Par t  -> 
    let ls = Ast_basic.N.list_of_star t [] in
    let len = List.length ls in
    let pat = (Id_epn.mk_tuple ~arity ~number:len :> pat) in
    let tys =
      mk_tuple
        (List.mapi (mapi_exp ~arity ~names  ~f) ls) in
    Expn_util.abstract names
      (Expn_util.currying [ %case-{ $pat:pat -> $tys } ] ~arity)
  | _  -> failwith (__BIND__  ^ Astfn_print.dump_ctyp ty)
    
(*
  Example:
   {[
  mk_record ~arity:3 (Ctyp.list_of_record %ctyp{ u:int; v:mutable float } )
  |> Ast2pt.print_pat f;
  ({ u = a0; v = a1 },{ u = b0; v = b1 },{ u = c0; v = c1 })

   ]}
 *)
let mk_record ?(arity=1) cols : ep  =
  let mk_list off = 
    Listf.mapi
      (fun i (x:col) ->
        %rec_exp-'{ $lid{x.label} = ${Id.xid ~off i} } ) cols in
  let res =
    let ls = sem_of_list (mk_list  0) in 
    Int.fold_left
      ~start:1 ~until:(arity-1) ~acc:%exp-'{{$ls}} @@ fun acc i ->
        let v = sem_of_list @@ mk_list i in
        com acc %exp-'{{$v}} in
  if arity > 1 then
    %exp-'{$par:res}
  else res     

type vbranch =
   [ `variant of (string* ctyp list )
   | `abbrev of ident ]
type branch =
   [ `branch of (string * ctyp list) ]

type destination =
  |Obj of kind
  (* | Type of ctyp         *)
  |Str_item
and kind =
  | Fold
  | Iter (* Iter style *) 
  | Map (* Map style *)
  | Concrete of ctyp



type warning_type =
  | Abstract of string 
  | Qualified of string  with print (* ("Print") *)



(* Feed to user to compose an expession node *)
type record_col = {
    label: string ;
    is_mutable: bool ;
    info: ty_info;
  }
      
type record_info =  record_col list


(** types below are used to tell fan how to produce
   function of type [ident -> ident] *)
type basic_id_transform =
    [ `Pre of string
    | `Post of string
    | `Fun of (string->string)
    | `Same ]

type rhs_basic_id_transform =
    [ basic_id_transform
    | `Exp of string -> exp ]

type full_id_transform =
    [  basic_id_transform
    | `Idents of  vid list  -> vid 
    (* decompose to a list of ident and compose as an ident *)          
    | `Id of vid -> vid
    (* just pass the ident to user do ident transform *)
    | `Last of string -> vid
    (* pass the string, and << .$old$. .$return$. >>  *)      
    | `Obj of  (string -> string) ]
        

    
(*
  @raise Invalid_argument 

  {[
  list_of_record %ctyp-{ u:int;m:mutable int };
  [{label = "u"; is_mutable = false; ctyp = `Id (, `Lid (, "int"))};
  {label = "m"; is_mutable = true; ctyp = `Id (, `Lid (, "int"))}]
  ]}
  
 *)
let list_of_record (ty:name_ctyp) : col list  =
  let (tys : name_ctyp list )  = Ast_basic.N.list_of_sem ty [] in
  tys|> List.map
    (
     function (x:name_ctyp) ->
       match x with 
       | `RecCol(`Lid label,ty, ((`Positive  | `Negative) as f) ) ->
           {label; ty; is_mutable = %p{`Positive} f}
       | t0 ->
           failwith  ( __BIND__ ^ Astfn_print.dump_name_ctyp t0) )

(** [number] is how many arguments -- in most cases it should be [2]
    [prefix] is additional parameter -- we keep the shape here, only generate type variables [_]
    [id] is the main type, such as [list]
    [len] is the arity of the type, for [list], it's one
 *)    
let mk_method_type ~number ~id:(id:ident) ~prefix len (k:destination) : (ctyp * ctyp) =
  let a_var_lens =
    Listf.init len (fun _ -> %ctyp-{'$lid{%fresh{all_}}}) in
  let b_var_lens =
    Listf.init len (fun _ -> %ctyp-{'$lid{%fresh{all_}}}) in
  let a_names = appl_of_list ((id:>ctyp) :: a_var_lens) in
  let b_names = appl_of_list ((id:>ctyp) :: b_var_lens) in
  let prefix = Listf.init prefix (const %ctyp-{_}) in

  let result_type = %ctyp-{_} in
  let self_type = %ctyp-{'__THIS_OBJ_TYPE__}  in
  let (quant,dst) =
    match k with
    |Obj Map -> (a_var_lens @ b_var_lens, b_names)
    |Obj Iter -> (a_var_lens, result_type)
    |Obj Fold -> (a_var_lens, self_type)
    |Obj (Concrete c ) -> (a_var_lens, c)
    |Str_item -> (a_var_lens, result_type) in
  let base =
    List.fold_right arrow (prefix @ Listf.init number (const  a_names)) dst in
  if len = 0 then
    ( `TyPolEnd  base,dst)
  else
    let quantifiers = appl_of_list quant in
    let params =
      Listf.init len 
      (fun i ->
        let ith_a = List.nth  a_var_lens i in
        let ith_b = List.nth b_var_lens i in
        match k with
        |Obj u  ->
            let dst =
              match  u with
              | Map -> ith_b
              | Iter -> result_type
              | Concrete c -> c
              | Fold-> self_type  in
            List.fold_right arrow (self_type::prefix) dst 
        |Str_item -> List.fold_right arrow (prefix@Listf.init number (const ith_a)) result_type ) in
    (%ctyp-{!$quantifiers . ${List.fold_right arrow params  base}},dst)




let mk_obj class_name  base body =
  %stru-{
   class $lid:class_name = object (self)
     inherit $lid:base ;
     $body;
   end }

    
let is_recursive ty_dcl =
  match ty_dcl with
  | `TyDcl (`Lid name, _, ctyp, _)  ->
      let obj = object(self)
        inherit Astfn_fold.fold as super;
        val mutable is_recursive = false;
        method! ctyp = function
          | %ctyp-{ $lid:i } when i = name -> begin
              is_recursive <- true;
              self
          end
          | x ->  if is_recursive then  self
          else super#ctyp x
        method is_recursive = is_recursive
      end in
      (obj#type_info ctyp)#is_recursive
  | `And _  -> true (* FIXME imprecise *)
  | _ -> failwithf "is_recursive not type declartion: %s" (Astfn_print.dump_decl ty_dcl)

(*
  {:stru|
  type u = int
  and v = bool
  |}
 *)
(*
  detect patterns like [List.t int ] or [List.t]
  Here the order matters
  {[
  ( %sigi{ type 'a tbl  = Ident.tbl 'a } |> fun
  [ <:sigi< type .$FanAst.TyDcl _loc _ _ x _ $. >>
  -> qualified_app_list x ]);
  Some (IdAcc  (Uid  "Ident") (Lid  "tbl"), [TyQuo  "a"])
  ]}
  
 *)  
let qualified_app_list (x:ctyp) : ((ident * ctyp list ) option ) =
  match x with 
  | %ctyp-{ $_ $_ } as x->
      (match Ast_basic.N.list_of_app x [] with
      | %ctyp-{ $lid:_  } :: _  -> None
      | (#ident' as i) ::ys  ->
          Some (i,ys)
      | _ -> None)
  | `Lid _ | `Uid _ -> None
  | #ident'  as i  -> Some (i, [])
  | _ -> None 

let is_abstract (x:decl)=
  match x with
  | `TyAbstr _ -> true
  | _ -> false

let abstract_list (x:decl)=
  match x with 
  | `TyAbstr ( _, lst,  _) ->
      begin match lst with
      | `None  -> Some 0
      |`Some xs ->
          Some (List.length @@ Ast_basic.N.list_of_com xs [])
      end
        (* Some (List.length lst) *)
  | _ -> None
        

    
(* 
   {[
   reduce_data_ctors
   %ctyp-{ A of option int and float | B of float } []
   (fun  s xs acc ->
   (prerr_endline s;  [xs :: acc] ))  ;
   A
   B
   `Id  (`Lid  "float");
   `App  (`Id  (`Lid  "option")) (`Id  (`Lid  "int"));
   `Id  (`Lid  "float")
   ]}
   @return result type to indicate error
   FIXME a good  support for arrow types?
   FIXME moved to astbuild?
   [ A of [`a | `b] and int ]
 *)
let reduce_data_ctors (ty:or_ctyp)  (init:'a) ~compose
    (f:  string -> ctyp list  -> 'e)  =
  let branches = Ast_basic.N.list_of_bar ty [] in
  List.fold_left
    (fun acc x ->
      match (x:or_ctyp) with
      |  `Of (`Uid cons, tys) ->
          compose (f cons (Ast_basic.N.list_of_star tys [])) acc  
      | `Uid  cons -> compose  (f cons [] ) acc
      | t->
          failwithf
            "reduce_data_ctors: %s" (Astfn_print.dump_or_ctyp t)) init  branches
    
let view_sum (t:or_ctyp) =
  let bs = Ast_basic.N.list_of_bar t [] in
  List.map
    (function
      | (* %{$uid:cons} *) `Uid cons ->
          `branch (cons,[])
      | `Of(`Uid cons,t) (* %{$uid:cons of $t} *) ->
          `branch (cons,  Ast_basic.N.list_of_star  t [])
      | _ -> assert false ) bs 

(*
  {[
  reduce_variant {:ctyp| [ `Chr of (loc * string) (* 'c' *)
  | `Int of   (loc * string) (* 42 *)
  | `Int32 of (loc * string)
  | `Int64 of (loc * string)
  | `Flo of (loc * string)
  | `Nativeint of (loc * string)
  (* s *) (* "foo" *)
  | `Str of (loc * string) | u | list int | [ `b | `c ] ] |};

  type v = [ `b];
  type u = [ `a | v ];
  let pp_print_u = function
  [ `a -> pp_print "%a"
  | #v -> pp_print_v 
  ]
  ]}
 *)    

let view_variant (t:row_field) : vbranch list  =
  let lst = Ast_basic.N.list_of_bar t [] in 
  List.map (
  function
    | (* %{ $vrn:cons of $par:t } *)
      (* `Of ( (`TyVrn (_, `C (_,cons))), (`Par (_, t))) *)
      `TyVrnOf( `C cons, `Par t)
      ->
        `variant (cons, Ast_basic.N.list_of_star t [])
    | (* %{ `$cons of $t } *)
      (* `Of (_loc, (`TyVrn (_, `C(_,cons))), t) *)
      `TyVrnOf(`C cons,t) -> `variant (cons, [t])
    | (* %{ `$cons } *)
      `TyVrn (`C cons) ->
        `variant (cons, [])
    | `Ctyp ((#ident' as i)) -> `abbrev i  
    | u -> failwithf "%s %s" __BIND__ (Astfn_print.dump_row_field u)  ) lst 

let conversion_table : (string,string) Hashtbl.t = Hashtbl.create 50
(*************************************************************************)
(* transformation function *)    
let transform : full_id_transform -> vid -> exp  =
  let open Idn_util in  function
    | `Pre pre ->
        fun  x ->  (ident_map (fun x -> pre ^ x) x : exp)
            (* fun [x -> %{ $(id: ident_map (fun x ->  pre ^ x) x ) } ] *)
    | `Post post ->
        fun x -> (ident_map (fun x-> x ^ post) x : exp )
            (* fun [x -> %{ $(id:  ident_map (fun x -> x ^ post) x ) } ] *)
    | `Fun f ->
        fun x -> ident_map f x 
            (* fun [x -> %{ $(id:  ident_map f x ) } ] *)
    | `Last f ->
        fun  x -> (ident_map_of_ident f x : vid :> exp)
            (* %{ $(id: ident_map_of_ident f x  ) } *) 
    | `Same -> fun x -> (x : vid :> exp )
    | `Id f->
        fun x -> (f x : vid :> exp)
            (* fun [x -> %{ $(id: f x ) } ] *)
    | `Idents f ->
        fun x  -> (f (Ast_basic.N.list_of_dot x []) : vid :> exp )
            (* fun [x -> %{ $(id: f (list_of_dot x []) )  }  ] *)
    | `Obj f ->
        function
          | `Lid x  -> %exp-{ self# $lid{ f x} }
          | t -> 
              let dest =  map_to_string t in
              let src = Astfn_print.dump_vid t in (* FIXME *)
              let () =
                if not @@ Hashtbl.mem conversion_table src then begin 
                  Hashtbl.add conversion_table src dest;   
                  Format.eprintf "Warning:  %s ==>  %s ==> unknown\n" src dest;
                end in
              %exp-{self#$lid{f dest}}
                  (*todo  set its default let to self#unknown *)

let basic_transform = function 
  | `Pre pre -> (fun x -> pre ^ x)
  | `Post post -> (fun x -> x ^ post)
  | `Same  -> (fun x -> x)
  | `Fun f -> f 

let left_transform = function
  |  #basic_id_transform as x ->
      (** add as here to overcome the type system *)
      let f = basic_transform x in 
      fun x -> lid (f  x)
   
let right_transform = function
  | #basic_id_transform as x ->
      (** add as here to overcome the type system *)
      let f = basic_transform x in 
      fun x -> %exp-{ $lid{ f x} } 
  | `Exp f -> f 
          




let gen_tuple_abbrev  ~arity ~annot ~destination name e  =
  let args :  pat list =
    Listf.init arity @@ fun i -> %pat-{ (#$id:name as $lid{ Id.x ~off:i 0 }) }in
  let exps =
    Listf.init arity @@ fun i ->
      %exp-{ $id{Id.xid ~off:i 0} }  in
  let e = appl_of_list (e:: exps) in 
  let pat = args |>tuple_com in
  match destination with
  | Obj(Map) ->
     %case-{$pat:pat->($e : ${(name :> ctyp)} :> $annot) }
  |_ -> %case-{$pat:pat->( $e  :> $annot)}



(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/ctyp.cmo" *)
(* end: *)
