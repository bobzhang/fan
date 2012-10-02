(* <:fan<
 * lang "expr" ; >> ; *)

<:include_ml< "open_template.ml"; >>;

(*
   {[
   to_string << A of (int*float) >> ;

   "A of (int * float)"
   ]}
 *)
value to_string  =  to_string_of_printer opr#ctyp ;
value eprint v = eprintf "@[%a@]@." opr#ctyp v ;

<:fan<
lang "ctyp";
>>;
  
<:include_ml<
  "fan_structure.ml";
>> ;

    
(*
   compose type abstractions
 *)
value arrow a b =  << .$a$. -> .$b$. >>;
value (|->) = arrow;  
value sta a b = << .$a$. * .$b$.  >>;
value sta_of_list = reduce_right sta;  
value arrow_of_list = reduce_right arrow;
value app_arrow lst acc = List.fold_right arrow lst acc;
value tuple_sta_of_list = fun
  [ [] -> invalid_arg "tuple_sta__of_list while list is empty"
  | [x] -> x
  | xs -> << .$tup:sta_of_list xs$. >> ];
  
value (<+) names ty =
  List.fold_right (fun name acc -> << '.$name$. -> .$acc$. >>)
    names ty
;
  
value (+>) params base = List.fold_right arrow params base;    
(*
   {[
   match <:str_item< type list 'a  = [A of int | B of 'a] >> with
   [ <:str_item<type .$x$. >> -> name_length_of_tydcl x ];
   ("list",1)
   ]}
 *)
value name_length_of_tydcl = fun 
    [ Ast.TyDcl _ name tyvars _ _ -> (name, List.length tyvars)
    | tydcl -> invalid_arg &
        sprintf "name_length_of_tydcl <<%s>>\n" & to_string tydcl];      



(*
   generate universal quantifiers for object's type signatures
   {[

  gen_quantifiers ~arity:2 3 |> eprint;
  'all_a0 'all_a1 'all_a2 'all_b0 'all_b1 'all_b2
  ]}
  quantifier variables can not be unified
 *)  
value gen_quantifiers ~arity n  =
  init arity (fun i -> init n (fun j -> <<  '.$lid:allx ~off:i j$. >> ))
  |> List.concat |> app_of_list
;


(*
  {[
  of_id_len ~off:2 (<:ident< Loc.t >> , 3 ) |> eprint;
  Loc.t 'all_c0 'all_c1 'all_c2
  ]}
 *)  
value of_id_len ~off (id,len) =
  apply << .$id:id$. >> (init len (fun i -> <<  '.$lid:allx ~off i$. >> ));
  
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

value of_name_len ~off (name,len) =
  let id = <:ident< .$lid:name$. >>  in
  of_id_len ~off (id,len);


(*
   @raise Invalid_argument  when the input is not a type declaration

  {[
  
  (fun [ <:str_item<type .$x$. >> -> ty_name_of_tydcl x  |> eprint ])
  <:str_item< type list 'a  = [A of int | B of 'a] >>;

  list 'a
   ]}
 *)  
value ty_name_of_tydcl  = fun 
    [ Ast.TyDcl _ name tyvars _ _ -> apply << .$lid:name$. >> tyvars
    | tydcl ->
        invalid_arg & sprintf "ctyp_of_tydcl<<%s>>\n" & to_string tydcl]
;      

(*
  {[
  (fun [ <:str_item<type .$x$. >> -> gen_ty_of_tydcl ~off:2 x  |> eprint ])
  <:str_item< type list 'a 'b = [A of int | B of 'a] >> ;

  list 'all_c0 'all_c1
  ]}
 *)  
value gen_ty_of_tydcl ~off tydcl =
  tydcl |> name_length_of_tydcl |>of_name_len ~off ;
  
(*
   @raise Invalid_argument 

   {[
   list_of_record <:ctyp< u:int; m:mutable int >>;
   [{Sig.label="u"; Sig.is_mutable=False; Sig.ctyp=TyId  (IdLid  "int")};
    {Sig.label="m"; Sig.is_mutable=True; Sig.ctyp=TyId  (IdLid  "int")}]
   ]}
   
 *)
value list_of_record ty =
  try 
    ty |> list_of_sem |> List.map (
       fun
         [ << .$lid:col_label$. : mutable .$col_ctyp$.  >> ->
           {col_label; col_ctyp; col_mutable=True}
         | << .$lid:col_label$. :  .$col_ctyp$.  >> ->
           {col_label; col_ctyp; col_mutable=False}
         | t0 -> raise & Unhandled t0 ])
  with
    [Unhandled t0 ->
      invalid_arg &
      (sprintf "list_of_record inner: <<%s>> outer: <<%s>>"
                     (to_string t0) (to_string ty)) ]
;

  
(*
   @raise Invalid_argument 
   {[
   gen_tuple_n << int >> 3  |> eprint;
   (int * int * int)
   gen_tuple_n << int >> 1  |> eprint;
   int
   ]}
 *)
value gen_tuple_n ty n = init n (fun _ -> ty) |> tuple_sta_of_list;

(*
  {[
  repeat_arrow_n <:ctyp< 'a >> 3 |> eprint;
  'a -> 'a -> 'a
  ]}
  *)
value repeat_arrow_n ty n =
  init n (fun _ -> ty) |>  arrow_of_list;
  
(*
  [result] is a keyword
  {[
  value (name,len) =
    ( <:str_item< type list 'a  'b = [A of int | B of 'a] >> |>
     fun [ <:str_item<type .$x$. >> -> name_length_of_tydcl x]);


  value f = mk_method_type ~number:2 ~prefix:["fmt"]
  (<:ident< .$lid:name$. >>,len);

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
value mk_method_type ~number ~prefix 
    (id,len) (k:obj_dest)  =
  (** FIXME A type variable name need to be valid *)
  let prefix = List.map
      (fun s -> String.drop_while (fun c -> c = '_') s) prefix in 
  let app_src   =
    app_arrow (init number (fun _ -> (of_id_len ~off:0 (id,len)))) in
  let result_type = << 'result >> and self_type = << 'self_type >> in 
  let (quant,dst) = match k with
    [Obj Map -> (2, (of_id_len ~off:1 (id,len)))
    |Obj Iter -> (1, result_type)
    |Obj Fold -> (1, self_type)
    |Str_item -> (1,result_type)] in 
  let params = init len (fun i
    -> let app_src = app_arrow
        (init number (fun _ -> <<  '.$lid:allx ~off:0 i $. >> )) in
      match k with
        [Obj u  ->
          let dst = match  u with
          [ Map -> <<  '.$lid:allx ~off:1 i$. >>
          | Iter -> result_type
          | Fold-> self_type ] in
          (self_type |-> (prefix <+ (app_src dst)))
        |Str_item -> prefix <+ (app_src result_type)]) in 
  let base = prefix <+ (app_src dst) in
  if len = 0 then base
  else let quantifiers = gen_quantifiers ~arity:quant len in
    << ! .$quantifiers$. . .$params +> base$. >>
;

value mk_method_type_of_name ~number ~prefix (name,len) (k:obj_dest)  =
  let id = <:ident< .$lid:name$. >> in
  mk_method_type ~number ~prefix (id,len) k ;


value mk_obj class_name  base body =
  <:str_item< class .$lid:class_name$. = object (self:'self_type)
    inherit .$lid:base$.;
    .$body$.;
  end >>
;

  
value is_recursive ty_dcl = match ty_dcl with
  [ Ast.TyDcl _ name _ ctyp _  ->
    let obj = object(self:'self_type)
      inherit Ast.fold as super;
      value mutable is_recursive = False;
      method! ctyp = fun
        [ << .$lid:i$. >> when i = name -> begin 
          is_recursive := True;
          self;
        end 
        | x ->  if is_recursive then  self
            else super#ctyp x  ];
      method is_recursive = is_recursive;
    end in
    (obj#ctyp ctyp)#is_recursive
  | << .$_$. and .$_$. >> -> True
  | _ -> invalid_arg ("is_recursive not type declartion" ^ to_string ty_dcl)];


(*
  detect patterns like [List.t int ] or [List.t]
  Here the order matters
  {[
  ( <:sig_item< type tbl 'a = Ident.tbl 'a >> |> fun
    [ <:sig_item< type .$Ast.TyDcl _loc _ _ x _ $. >>
     -> qualified_app_list x ]);
  Some (IdAcc  (IdUid  "Ident") (IdLid  "tbl"), [TyQuo  "a"])
  ]}
  
 *)  
value qualified_app_list = fun 
  [ << .$_$. .$_$. >> as x->
    match list_of_app x with
    [ [ << .$lid:_$.  >> :: _ ] -> None
    | [ << .$id:i$.   >> ::ys]  ->
        Some (i,ys)
    | _ -> None ]
  | ( << .$lid:_$.  >> | << .$uid:_$.  >>) -> None
  | << .$id:i$.  >> -> Some (i, [])
  | _ -> None ];

value is_abstract = fun 
  [ Ast.TyDcl _ _ _ << >> _ -> True | _ -> False];

value abstract_list = fun
  [ Ast.TyDcl _ _ lst << >> _ -> Some (List.length lst) | _ -> None];
value eq t1 t2 =
  let strip_locs t = (Ast.map_loc (fun _ -> Ast.Loc.ghost))#ctyp t in
  strip_locs t1 = strip_locs t2;
  
value eq_list t1 t2 =
  let rec loop = fun
    [ ([],[]) -> True
    | ([x::xs],[y::ys]) -> eq x y && loop (xs,ys)
    | (_,_) -> False] in loop (t1,t2);
  
(*

  {[

  value f = mk_transform_type_eq ();

  value v =
  (f#str_item

  <:str_item<
  type a = Loc.t
  and  b 'a  = [ A of LL.t 'a and LL.t 'a and Loc.t];
  value f x = 3
  >> );

  f#type_transformers |>  opr#str_item fmt;  

  v |> opr#str_item fmt;

  type ll_t 'a0 = LL.t 'a0;
  type loc_t = Loc.t;
  type a = loc_t and b 'a = [ A of ll_t 'a and ll_t 'a and loc_t ];
  value f x = 3;
  
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
value mk_transform_type_eq () = object(self:'self_type)
  value transformers = Hashtbl.create 50;
  inherit Ast.map as super;
  method! str_item = fun
    [ 
     <:str_item< type .$Ast.TyDcl _ name vars ctyp _ $. >> as x ->
       match qualified_app_list ctyp with
       [ Some (i,lst)  -> (* [ type u 'a = Loc.t int U.float]*)
         if  not (eq_list vars lst) then 
           super#str_item x
         else
          (* Manual substitution
             [type u 'a 'b = Loc.t 'a 'b]
             [type u int = Loc.t int]
             This case can not happen [type u Ast.int = Loc.t Ast.int ]
           *)
           let src = i and dest = Fan_ident.map_to_string i in begin
             Hashtbl.replace transformers dest (src,List.length lst);
             <:str_item< >> 
           end 
       | None ->  super#str_item x ]
     | x -> super#str_item x ];  
  method! ctyp x =   match qualified_app_list x with
    [ Some (i, lst) ->
      let lst = List.map (fun ctyp -> self#ctyp ctyp) lst in 
      let src = i and dest = Fan_ident.map_to_string i in begin
        Hashtbl.replace transformers dest (src,List.length lst);
        app_of_list [ << .$lid:dest$. >> :: lst ]
      end 
    | None  -> match x with
        (* <:str_item< type a =b == [A of int] >> ; *)
      [ << .$x$. == .$ctyp$. >> -> (* ignore x on purpose *)
        << .$x$. == .$super#ctyp ctyp$. >>
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
value transform_module_types  lst =
  let obj = mk_transform_type_eq () in 
  let item1 =
    List.map (fun
           [Mutual ls ->
             Mutual (List.map
                      (fun (s,ty) ->
                        (s, obj#ctyp ty)) ls)
           |Single (s,ty) ->
               Single (s, obj#ctyp ty)]) lst in
  let new_types = obj#type_transformers in
  (new_types,item1);
      
(* 
   {[
    reduce_data_ctors
    << A of option int and float | B of float >> []
      (fun  s xs acc ->
        do{ prerr_endline s; List.append xs acc })  ;
    A
    B
   TyId  (IdLid  "float");
    TyApp  (TyId  (IdLid  "option")) (TyId  (IdLid  "int"));
    TyId  (IdLid  "float")
    ]}
    @return result type to indicate error
    FIXME a good  support for arrow types?
    FIXME moved to astbuild?
    [ A of [`a | `b] and int ]
 *)
value reduce_data_ctors (ty:Ast.ctyp)  (init:'a)
    (f:  string -> list Ast.ctyp -> 'e)  = ErrorMonad.(
	(* antiquotation list can not be recognized in pattern
	    language the same applies to `int, int they behave the
	    same *)
	let rec loop acc t = match t with
	[ << .$uid:cons$. of .$tys$. >> ->
	  f cons (Ast.list_of_ctyp tys []) acc

        | << `.$uid:cons$. of .$tys$. >> ->
            f ("`" ^ cons)
              (Ast.list_of_ctyp tys []) acc
	| << .$uid:cons$. >> ->
	    f cons [] acc
              
        | <<  `.$uid:cons$. >> ->
            f ("`"^cons) [] acc
              
	| << .$t1$. | .$t2$. >> ->
	    loop (loop acc t1) t2
              
        | ( << [ .$ty$.  ] >>  | << [= .$ty$. ] >> 
        | << [< .$ty$. ] >>  | << [> .$ty$. ] >> )  ->
            loop  acc ty     
        | << >> -> acc
            (* we don't handle the type constructs  below *)
        | t ->  raise (Unhandled t)
        ] in
        try
           return & loop init ty
        with
         [Unhandled t0  ->
           fail
           (sprintf "reduce_data_ctors inner <<%s>> outer <<%s>>" 
              (to_string t0 )
              (to_string ty))
        ]
)
;

(*
  {[
  value f k = unknown ~number:2 ~prefix:["fmt"] k
  |> opr#class_str_item fmt ;

  f Fold;
  method unknown : ! 'all_a. 'fmt -> ('all_a * 'all_a) -> 'self_type =
  fun _ -> failwith "unknown fold2";

  f Map;
  method unknown : ! 'all_a. 'fmt -> ('all_a * 'all_a) -> 'all_a =
  fun _ -> failwith "unknown map2";

  f Iter;
  method unknown : ! 'all_a. 'fmt -> ('all_a * 'all_a) -> unit =
  fun _ -> failwith "unknown iter2";
  ]}
  *)
(* value unknown ~number ~prefix k =
 *   let all_a = << 'all_a >> in 
 *   let src = gen_tuple_n all_a number in
 *   let unit_type = << 'result>> and self_type = << 'self_type >> in 
 *   let (error,dst) = match k with
 *     [Map ->
 *       (<:expr< failwith .$str:"unknown map"  ^string_of_int number$. >>,
 *         all_a )
 *     |Iter ->
 *         (<:expr< failwith .$str:"unknown iter" ^string_of_int number$. >>,
 *           unit_type)
 *     |Fold ->
 *         (<:expr< failwith .$str:"unknown fold" ^string_of_int number$. >>,
 *           self_type) ] in 
 *   let base = prefix <+ (src|-> dst) in
 *   <:class_str_item< method unknown : $ << ! 'all_a . .$base$. >> $
 *     = fun [_ -> $ error  $ ] >>
 * ; *)
