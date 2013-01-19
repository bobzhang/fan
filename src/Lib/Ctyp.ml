
#default_quotation "ctyp";;
open FanAst;

open LibUtil;
open Format;
open Basic;
open FSig;

(*  *)
(* let rec fa al =fun *)
(*   [ {| $f $a |}  -> fa [a :: al] f *)
(*   | f -> (f, al) ]; *)

let rec to_var_list =  fun
  [ {| $t1 $t2 |} ->
    to_var_list t1 @ to_var_list t2
  | {| '$lid:s |} | {| + '$lid:s |} | {| - '$lid:s |}  -> [s]
  | _ -> assert false ];

(* let list_of_opt ot acc = match ot with *)
(*   [ {||} -> acc *)
(*   | t -> FanAst.list_of_ctyp t acc ]; *)


let rec name_tags = fun
  [ {| $t1 $t2 |} -> name_tags t1 @ name_tags t2
  | (* {| `$s |} *)
    `TyVrn (_, `C (_,s))
    -> [s]
  | _ -> assert false ];

(* here -> can not be used as a delimiter, if we remove quotations.*)  
let rec to_generalized = fun
    [ {| $t1 -> $t2 |} ->
        let (tl, rt) = to_generalized t2 in
        ([t1 :: tl], rt)
    | t -> ([], t) ];


(*
   {[
   to_string << A of (int*float) >> ;

   "A of (int * float)"
   ]}
 *)
let to_string  =
   (to_string_of_printer FanAst.dump#ctyp);
  (* ref (fun _ -> failwith "Ctyp.to_string foward declaration, not implemented yet"); *)
  (* to_string_of_printer FanBasic.p_ctyp ; *)
let eprint :  (ctyp -> unit) =
  (fun c -> eprintf "@[%a@]" FanAst.dump#ctyp c );


  
let _loc = FanLoc.ghost ; (* FIXME *)

(* DEFINE GETLOC(expr)= FanAst.loc_of expr;   *)
INCLUDE "src/Lib/CommonStructure.ml";  
(*
   compose type abstractions
 *)
let arrow a b =  {| $a -> $b |};
let (|->) = arrow;  
let sta a b = {|$a * $b  |};

let sta_of_list = List.reduce_right sta;  

let arrow_of_list = List.reduce_right arrow;
  
let app_arrow lst acc = List.fold_right arrow lst acc;
  
let tuple_sta_of_list = fun
  [ [] -> invalid_arg "tuple_sta__of_list while list is empty"
  | [x] -> x
  | xs -> {| $(tup:sta_of_list xs) |} ];
  
let (<+) names ty =
  List.fold_right
    (fun name acc -> {| '$lid:name -> $acc |})
    names ty;
  
let (+>) params base = List.fold_right arrow params base;    
(*
   {[
   match <:str_item< type list 'a  = [A of int | B of 'a] >> with
   [ <:str_item<type .$x$. >> -> name_length_of_tydcl x ];
   ("list",1)
   ]}
 *)
let name_length_of_tydcl = fun 
    [ `TyDcl (_, `Lid(_,name), tyvars, _, _) -> (name, List.length tyvars)
    | tydcl -> invalid_arg (
        sprintf "name_length_of_tydcl {|%s|}\n" & to_string tydcl)];      



(*
   generate universal quantifiers for object's type signatures
   {[

  gen_quantifiers ~arity:2 3 |> eprint;
  'all_a0 'all_a1 'all_a2 'all_b0 'all_b1 'all_b2
  ]}
  quantifier variables can not be unified
 *)  
let gen_quantifiers ~arity n  =
  List.init arity
    (fun i -> List.init n (fun j -> {|  '$(lid:allx ~off:i j) |} ))
  |> List.concat |> app_of_list;


(*
  {[
  of_id_len ~off:2 (<:ident< Loc.t >> , 3 ) |> eprint;
  Loc.t 'all_c0 'all_c1 'all_c2
  ]}
 *)  
let of_id_len ~off (id,len) =
  apply {|$id:id |}
    (List.init len
       (fun i -> {|  '$(lid:allx ~off i) |}));
  
(*
   {[
    ( <:str_item< type list 'a  = [A of int | B of 'a] >> |>
    fun [ <:str_item<type .$x$. >> -> name_length_of_tydcl x
        |> of_name_len ~off:1 |> eprint ] );
   list 'all_b0

    ( <:str_item< type list   = [A of int | B] >> |>
    fun [ <:str_item<type .$x$. >> -> name_length_of_tydcl x
        |> of_name_len ~off:1 |> eprint ] );
   ]}

 *)    

let of_name_len ~off (name,len) =
  let id = {:ident| $lid:name |}  in
  of_id_len ~off (id,len);


(*
   @raise Invalid_argument  when the input is not a type declaration

  {[
  
  (fun [ <:str_item<type .$x$. >> -> ty_name_of_tydcl x  |> eprint ])
  <:str_item< type list 'a  = [A of int | B of 'a] >>;

  list 'a
   ]}
 *)  
let ty_name_of_tydcl  = fun 
    [ `TyDcl (_, `Lid(_,name), tyvars, _, _) -> apply {| $lid:name |} tyvars
    | tydcl ->
        invalid_arg & sprintf "ctyp_of_tydcl{|%s|}\n" & to_string tydcl];      

(*
  {[
  (fun [ <:str_item<type .$x$. >> -> gen_ty_of_tydcl ~off:2 x  |> eprint ])
  <:str_item< type list 'a 'b = [A of int | B of 'a] >> ;

  list 'all_c0 'all_c1
  ]}
 *)  
let gen_ty_of_tydcl ~off tydcl =
  tydcl |> name_length_of_tydcl |>of_name_len ~off ;
  
(*
   @raise Invalid_argument 

   {[
     L.Ctyp.list_of_record {:ctyp| u:int;m:mutable int |};
     - : FSig.col list =
     [{label = "u"; is_mutable = false; ctyp = Id (, Lid (, "int"))};
      {label = "m"; is_mutable = true; ctyp = Id (, Lid (, "int"))}]
   ]}
   
 *)
let list_of_record (ty:ctyp) =
    list_of_sem' ty [] |> List.map (
       fun
         [ 
           (* {| $lid:label : mutable $ctyp  |} *)
           `TyCol (_, (`Id (_, (`Lid (_, label)))), (`Mut (_, ctyp))) ->
             {label; ctyp; is_mutable=true}
         | `TyCol (_, (`Id (_, (`Lid (_, label)))), ctyp)
             (* {| $lid:label :  $ctyp  |} *) -> 
               {label; ctyp; is_mutable=false}
         | t0 ->
             FanLoc.errorf (loc_of t0)
               "list_of_record %s" (dump_ctyp t0) ]);

  
(*
   @raise Invalid_argument 
   {[
   gen_tuple_n {| int |} 3  |> eprint;
   (int * int * int)
   gen_tuple_n {| int |} 1  |> eprint;
   int
   ]}
 *)
let gen_tuple_n ty n = List.init n (fun _ -> ty) |> tuple_sta_of_list;

(*
  {[
  repeat_arrow_n <:ctyp< 'a >> 3 |> eprint;
  'a -> 'a -> 'a
  ]}
  *)
let repeat_arrow_n ty n =
  List.init n (fun _ -> ty) |>  arrow_of_list;
  
(*
  [result] is a keyword
  {[
  let (name,len) =
    ( {:str_item| type list 'a  'b = [A of int | B of 'a] |}
      |>
      fun [ {:str_item|type $x |} -> name_length_of_tydcl x]);


  let f = mk_method_type ~number:2 ~prefix:["fmt"]
  ({:ident| $lid:name |},len);

  open Fan_sig;
  
  f (Obj Map)|> eprint;
  ! 'all_a0 'all_a1 'all_b0 'all_b1.
  ('self_type -> 'fmt -> 'all_a0 -> 'all_a0 -> 'all_b0) ->
    ('self_type -> 'fmt -> 'all_a1 -> 'all_a1 -> 'all_b1) ->
      'fmt ->
        list 'all_a0 'all_a1 -> list 'all_a0 'all_a1 -> list 'all_b0 'all_b1

  f (Obj Iter)|> eprint;
  ! 'all_a0 'all_a1.
  ('self_type -> 'fmt -> 'all_a0 -> 'all_a0 -> 'result) ->
    ('self_type -> 'fmt -> 'all_a1 -> 'all_a1 -> 'result) ->
      'fmt -> list 'all_a0 'all_a1 -> list 'all_a0 'all_a1 -> 'result
  
  f (Obj Fold) |> eprint;
  ! 'all_a0 'all_a1.
  ('self_type -> 'fmt -> 'all_a0 -> 'all_a0 -> 'self_type) ->
    ('self_type -> 'fmt -> 'all_a1 -> 'all_a1 -> 'self_type) ->
      'fmt -> list 'all_a0 'all_a1 -> list 'all_a0 'all_a1 -> 'self_type
  
  f Str_item |> eprint;
  ! 'all_a0 'all_a1.
  ('fmt -> 'all_a0 -> 'all_a0 -> 'result) ->
    ('fmt -> 'all_a1 -> 'all_a1 -> 'result) ->
      'fmt -> list 'all_a0 'all_a1 -> list 'all_a0 'all_a1 -> 'result
 *)  
let mk_method_type ~number ~prefix (id,len) (k:destination)  =
  (** FIXME A type variable name need to be valid *)
  let prefix = List.map
      (fun s -> String.drop_while (fun c -> c = '_') s) prefix in 
  let app_src   =
    app_arrow (List.init number (fun _ -> (of_id_len ~off:0 (id,len)))) in
  let result_type = {| 'result |}
  and self_type = {| 'self_type |}  in 
  let (quant,dst) =
    match k with
    [Obj Map -> (2, (of_id_len ~off:1 (id,len)))
    |Obj Iter -> (1, result_type)
    |Obj Fold -> (1, self_type)
    |Str_item -> (1,result_type)] in 
  let params =
    List.init len
      (fun i
        ->
          let app_src = app_arrow
              (List.init number
                 (fun _ -> {|  '$(lid:allx ~off:0 i)  |} )) in
          match k with
          [Obj u  ->
              let dst =
                match  u with
                [ Map -> {|  '$(lid:allx ~off:1 i) |}
                | Iter -> result_type
                | Fold-> self_type ] in
              (self_type |-> (prefix <+ (app_src dst)))
          |Str_item -> prefix <+ (app_src result_type)]) in 
  let base = prefix <+ (app_src dst) in
  if len = 0 then
    (base)
  else let quantifiers = gen_quantifiers ~arity:quant len in
    ({| ! $quantifiers . $(params +> base) |});

(* FIXME : merge with [mk_type_of] *)  
let mk_dest_type  ~destination (id,len) =
  let result_type = {| 'result |}
  and self_type = {| 'self_type |} in 
  let (_quant,dst) =
    match destination with
    [Obj Map ->
      (2, apply {|$id:id |} (List.init len (fun _ -> {|  _ |})))
      (* (2, (of_id_len ~off:1 (id,len))) *)
    |Obj Iter -> (1, result_type)
    |Obj Fold -> (1, self_type)
    |Str_item -> (1,result_type)] in dst;

(* *)  
let mk_method_type_of_name ~number ~prefix (name,len) (k:destination)  =
  let id = {:ident| $lid:name |} in
  mk_method_type ~number ~prefix (id,len) k ;


let mk_obj class_name  base body =
  {:str_item| class $lid:class_name = object (self: 'self_type)
    inherit $lid:base ;
    $body;
  end |};

  
let is_recursive ty_dcl = match ty_dcl with
  [ `TyDcl (_, `Lid(_,name), _, ctyp, _)  ->
    let obj = object(self:'self_type)
      inherit FanAst.fold as super;
      val mutable is_recursive = false;
      method! ctyp = fun
        [ {| $lid:i |} when i = name -> begin 
          is_recursive <- true;
          self;
        end 
        | x ->  if is_recursive then  self
            else super#ctyp x  ];
      method is_recursive = is_recursive;
    end in
    (obj#ctyp ctyp)#is_recursive
  | {| $_ and $_ |} -> true
  | _ -> invalid_arg ("is_recursive not type declartion" ^ to_string ty_dcl)];


(*
  detect patterns like [List.t int ] or [List.t]
  Here the order matters
  {[
  ( <:sig_item< type tbl 'a = Ident.tbl 'a >> |> fun
    [ <:sig_item< type .$FanAst.TyDcl _loc _ _ x _ $. >>
     -> qualified_app_list x ]);
  Some (IdAcc  (Uid  "Ident") (Lid  "tbl"), [TyQuo  "a"])
  ]}
  
 *)  
let qualified_app_list = fun 
  [ {| $_ $_ |} as x->
    match list_of_app x with
    [ [ {| $lid:_  |} :: _ ] -> None
    | [ {| $id:i   |} ::ys]  ->
        Some (i,ys)
    | _ -> None ]
  | ( {| $lid:_  |} | {| $uid:_  |}) -> None
  | {| $id:i  |} -> Some (i, [])
  | _ -> None ];

let is_abstract = fun 
  [ `TyDcl (_, _, _, {| |}, _) -> true | _ -> false];

let abstract_list = fun
  [ `TyDcl (_, _, lst, {| |}, _) -> Some (List.length lst) | _ -> None];
let eq t1 t2 =
  let strip_locs t = (FanAst.map_loc (fun _ -> FanLoc.ghost))#ctyp t in
  strip_locs t1 = strip_locs t2;
  
let eq_list t1 t2 =
  let rec loop = fun
    [ ([],[]) -> true
    | ([x::xs],[y::ys]) -> eq x y && loop (xs,ys)
    | (_,_) -> false] in loop (t1,t2);
  
(*

  {[

  let f = mk_transform_type_eq ();

  let v =
  (f#str_item

  <:str_item<
  type a = Loc.t
  and  b 'a  = [ A of LL.t 'a and LL.t 'a and Loc.t];
  let f x = 3
  >> );

  f#type_transformers |>  opr#str_item fmt;  

  v |> opr#str_item fmt;

  type ll_t 'a0 = LL.t 'a0;
  type loc_t = Loc.t;
  type a = loc_t and b 'a = [ A of ll_t 'a and ll_t 'a and loc_t ];
  let f x = 3;
  
  ]}
  There are two cases:
  The first is [Loc.t => loc_t], and record the relationship to the hashtbl.
  It's reasonalble and sound. But it may bring some unnecessary duplicated code.

  We only consider one duplicated case
  [type u 'a = Loc.t 'a] [type u int = Loc.t int ]
  the type variables are the same as the type definition.
  here we record the relationship [Loc.t => u ]
  ]}
 *)
let mk_transform_type_eq () = object(self:'self_type)
  val transformers = Hashtbl.create 50;
  inherit FanAst.map as super;
  method! str_item = fun
    [ 
     {:str_item| type $(`TyDcl (_, _name, vars, ctyp, _) ) |} as x -> (* FIXME why tuple?*)
       match qualified_app_list ctyp with
       [ Some (i,lst)  -> (* [ type u 'a = Loc.t int U.float]*)
         if  not (eq_list vars lst) then 
           super#str_item x
         else
          (* Manual substitution
             [type u 'a 'b = Loc.t 'a 'b]
             [type u int = Loc.t int]
             This case can not happen [type u FanAst.int = Loc.t FanAst.int ]
           *)
           let src = i and dest = Ident.map_to_string i in begin
             Hashtbl.replace transformers dest (src,List.length lst);
             {:str_item| |} 
           end 
       | None ->  super#str_item x ]
     | x -> super#str_item x ];  
  method! ctyp x =   match qualified_app_list x with
    [ Some (i, lst) ->
      let lst = List.map (fun ctyp -> self#ctyp ctyp) lst in 
      let src = i and dest = Ident.map_to_string i in begin
        Hashtbl.replace transformers dest (src,List.length lst);
        app_of_list [ {| $lid:dest |} :: lst ]
      end 
    | None  -> match x with
        (* <:str_item< type a =b == [A of int] >> ; *)
      [ {| $x == $ctyp |} -> (* ignore x on purpose *)
        {| $x == $(super#ctyp ctyp) |}
      | _ ->   super#ctyp x  ]];

  (* dump the type declarations *)  
  method type_transformers = 
    Hashtbl.fold (fun dest (src,len) acc ->
      [(dest,src,len)  :: acc]) transformers [];

end;



(*
  This is a general tranversal, which could be bootstrapped
  using our pluggin actually
  Preprocess module_types, generate type equalities

 *)
let transform_module_types  lst =
  let obj = mk_transform_type_eq () in 
  let item1 =
    List.map (fun
           [`Mutual ls ->
             `Mutual (List.map
                      (fun (s,ty) ->
                        (s, obj#ctyp ty)) ls)
           |`Single (s,ty) ->
               `Single (s, obj#ctyp ty)]) lst in
  let new_types = obj#type_transformers in
  (new_types,item1);
      
(* 
   {[
    reduce_data_ctors
    {:ctyp| A of option int and float | B of float |} []
      (fun  s xs acc ->
         (prerr_endline s;  [xs :: acc] ))  ;
    A
    B
   Id  (Lid  "float");
    TyApp  (Id  (Lid  "option")) (Id  (Lid  "int"));
    Id  (Lid  "float")
    ]}
    @return result type to indicate error
    FIXME a good  support for arrow types?
    FIXME moved to astbuild?
    [ A of [`a | `b] and int ]
 *)
let reduce_data_ctors (ty:ctyp)  (init:'a) ~compose
    (f:  string -> list ctyp -> 'e)  =
  let branches = list_of_or' ty [] in
  List.fold_left (fun acc x -> match x with
    [  `Of (_loc, (`Id (_, (`Uid (_, cons)))), tys)
      ->
        compose (f cons (list_of_and' tys [])) acc  
    | (* {| $uid:cons |} *)
      `Id (_loc, (`Uid (_, cons)))
      -> compose  (f cons [] ) acc
    | t->
        FanLoc.errorf (loc_of t)
          "reduce_data_ctors: %s" (dump_ctyp t)]) init  branches;
    
let view_sum (t:ctyp) =
  let bs = FanAst.list_of_or' t [] in
  List.map
    (fun
      [ {|$uid:cons|} ->
        `branch (cons,[])
       | {|$uid:cons of $t|} ->
           `branch (cons, FanAst.list_of_and' t [])
       | _ -> assert false ]) bs ;

(*
  {[
  L.Ctyp.reduce_variant {:ctyp| [= `Chr of (loc * string) (* 'c' *)
    | `Int of   (loc * string) (* 42 *)
      | `Int32 of (loc * string)
      | `Int64 of (loc * string)
      | `Flo of (loc * string)
      | `NativeInt of (loc * string)
        (* s *) (* "foo" *)
        | `Str of (loc * string) | u | list int | [= `b | `c ] ] |};

  type v = [= `b];
  type u = [= `a | v ];
  let pp_print_u = function
  [ `a -> pp_print "%a"
  | #v -> pp_print_v 
  ]
  ]}
 *)    
let view_variant (t:ctyp) : list vbranch =

  let lst = list_of_or' t [] in 
  List.map (
  fun [ (* {| `$cons of $tup:t |} *)
        `Of (_loc, (`TyVrn (_, `C (_,cons))), (`Tup (_, t)))
        ->
        `variant (cons, list_of_star' t [])
      | (* {| `$cons of $t |} *)
        `Of (_loc, (`TyVrn (_, `C(_,cons))), t)
        -> `variant (cons, [t])
      | (* {| `$cons |} *)
        `TyVrn (_loc, `C (_,cons))
        ->
          `variant (cons, [])
      |  `Id (_loc,i) -> `abbrev i  
      (* | {|$lid:x|} -> `abbrev x  *)
      | u -> FanLoc.errorf (FanAst.loc_of u)
            "view_variant %s" (FanAst.dump_ctyp u) ] ) lst ;

    
let of_str_item = fun
  [ `Type(_,x) -> x
  | t ->
      FanLoc.errorf (FanAst.loc_of t)
        "Ctyp.of_str_item %s" (dump_str_item t) ];
