
(* Utility for Ast.expr*)
<:fan<
lang "expr" ;
>> ;
<:include_ml< "open_template.ml";  >>;

value to_string =  to_string_of_printer opr#expr ;
value eprint v = eprintf "@[%a@]@." opr#expr v ;

<:include_ml<
 "fan_structure.ml";
 "fan_expr_patt.ml";
>> ;
  
value mk_unary_min f arg =
  match arg with
  [ << .$int:n$. >> -> << .$int:String.neg n$. >> 
  | << .$int32:n$. >> -> << .$int32:String.neg n$. >>
  | << .$int64:n$. >> -> << .$int64:String.neg n$. >>
  | << .$nativeint:n$. >> -> << .$nativeint:String.neg n$. >>
  | << .$flo:n$. >> -> << .$flo:String.neg n$. >>
  | _ -> << .$lid:"~" ^ f$. .$arg$. >> ]
;
(*
   since ocaml respect [(~-)] as a prefix [(-)]
   and [(~-.)] as a prefix [(-.)]
   Example:
   {[
   mk_unary_min "-." <:expr< 3 >>;
    Camlp4.PreCast.Ast.expr = ExInt  "-3"

   mk_unary_min "-." <:expr< a >>;
    Camlp4.PreCast.Ast.expr =
   ExApp  (ExId  (IdLid  "~-.")) (ExId  (IdLid  "a"))

   ]}
 *)  

    

value mk_assert  =
  fun
  [ << False >> ->
    << assert False >> 
  | e -> << assert .$e$. >> ]
  ;
(*
   Camlp4 treats [assert False] as a special construct [ExAsf]

   {[
   <:expr< assert False>>;

   Camlp4.PreCast.Ast.expr = ExAsf 

   <:expr< assert .$ <:expr< False >> $. >>;
   
   Camlp4.PreCast.Ast.expr = ExAsr  (ExId  (IdUid  "False"))

   e2s <<assert False>>;
   [
   {Camlp4_import.Parsetree.pstr_desc=
   Camlp4_import.Parsetree.Pstr_eval
   {Camlp4_import.Parsetree.pexp_desc=
   Camlp4_import.Parsetree.Pexp_assertfalse;
   Camlp4_import.Parsetree.pexp_loc=};
   Camlp4_import.Parsetree.pstr_loc=}]

   e2s    <:expr< assert .$ <:expr< False >> $. >>;

   {Camlp4_import.Parsetree.pstr_desc=
   Camlp4_import.Parsetree.Pstr_eval
    {Camlp4_import.Parsetree.pexp_desc=
      Camlp4_import.Parsetree.Pexp_assert
       {Camlp4_import.Parsetree.pexp_desc=
         Camlp4_import.Parsetree.Pexp_construct
          (Camlp4_import.Longident.Lident "false") None True;
        Camlp4_import.Parsetree.pexp_loc=};
     Camlp4_import.Parsetree.pexp_loc=};
  Camlp4_import.Parsetree.pstr_loc=}
   ]}
 *)      


value mk_record label_exprs =
  let rec_bindings = List.map (fun (label, expr) ->
    <:rec_binding< .$lid:label$. = .$expr$. >> ) label_exprs in
  << { .$list:rec_bindings$. } >>
;
(*
   FIXME: label is lid
 *)



value failure =
  << raise (Failure "metafilter: Cannot handle that kind of types ") >>
;       



value (<+) names acc  =
  List.fold_right (fun name acc ->  << fun [ .$lid:name$. -> .$acc$. ]>>)
    names acc 
;
value (<+<) patts acc =
  List.fold_right (fun p acc -> << fun [.$p$. -> .$acc$.] >> ) patts acc
;
(*
  {[
   gen_app_first 3 2  |> eprint;
   c0 c1 c2
   Warning: strange pattern application of a non constructor
  ]}
 *)
(* value gen_app_first ~number ~off =
 *   init number (fun i -> << .$id:xid ~off i$. >>) |> app_of_list
 * ; *)
    
(*
   Add Prefixes to expression
   {[

    ["x0";"x1";"x2"] <+ << blabla>> |> eprint;
    fun x0 x1 x2 -> blabla

   ]}
 *)  

  
value mk_seq es = let _loc = Loc.ghost in 
  << .$seq:Ast.exSem_of_list es$. >>
;
(*
   {[
   mkseq [ <:expr<f >> ;
           <:expr< a >> ; <:expr< b >>
        ] |> eprint
   ;

   (f; a; b)
   ]}
 *)


value mep_comma x y = << Ast.PaCom _loc .$x$. .$y$. >>    
;
(*
   Notice for meta, we should annote quotation expander
   expr explicitly
 *)    

  
value mep_app x y =  << Ast.PaApp _loc .$x$. .$y$. >>      
;       
(*
   {[
   mep_app <:patt< a >> <:patt<g >> |> eprint 
   Ast.PaApp _loc a g
   ]}
 *)

  

value mk_tuple_ep = fun 
   [ [] -> assert False
   | [x] -> x
   | xs  ->
       << Ast.PaTup _loc .$ reduce_right mep_comma xs $. >>
   ]
;
(*

   We want to generate code 
   {[
   <:expr< <:patt< (A,B,C) >> >>
   ]}

   {[
   mk_tuple_ep
   [ <:patt< f >> ;
   <:patt< a >> ;
   <:patt< b>> ] |> eprint ;
   
   Ast.PaTup _loc (Ast.PaCom _loc f (Ast.PaCom _loc a b))
   ]}
 *)


(*
   {[
   meta_of_str "A" = <:expr< <:patt< A >> >> ;
   True 
   ]}
 *)  

value mep_of_str  s =
  let u = << Ast.IdUid _loc .$str:s$. >> in
  << Ast.PaId _loc .$u$. >>
;

value mee_of_str s =
  let u = << Ast.IdUid _loc .$str:s$. >> in 
  << Ast.ExId _loc .$u$. >>
;
(*
   {[
    meta_of_str "A" = << << A >> >> ;
    True
    ]}
 *)

(*
   @raise Invalid_argument
   
   There are 2 stages here 
   We want to generate code  like this
   {[
   <<
       << ($meta_int _loc x0$, $meta_int _loc x1$ ) >>
   >>
   ]}

   {[
   ( <:expr< <:expr< (A,B,C) >> >>   |> eprint );

   Ast.ExTup _loc
   (Ast.ExCom _loc (Ast.ExId _loc (Ast.IdUid _loc "A"))
   (Ast.ExCom _loc (Ast.ExId _loc (Ast.IdUid _loc "B"))
        (Ast.ExId _loc (Ast.IdUid _loc "C"))))

   ]}
   
   Normal practice:
     first print the result, then find a mechanical way to
     construct
   Here we should avoid singleton tuple error
   << .$tup:a$. >> when a is a single, will cause error

   when dumped
   {[

   mk_tuple_ee
   [ <:expr< f >> ; <:expr< a >> ; <:expr< b>> ] |> eprint;

   Ast.ExTup _loc (Ast.ExCom _loc f (Ast.ExCom _loc a b))

   ]}
 *)      

(*
value mee_semi_col x y =
  << Ast.RbSem _loc $x$ $y$ >> ;
  
(* value meta_semi_col x y = << Ast.RbEq   >>*)
value mk_record_ee label_exprs =
  let rec_bindings = List.map
      (fun (label,expr) ->
        <:rec_binding< $lid:label$ = $expr$ >> ) label_exprs in
  << Ast.ExRec _loc .$ reduce ~dir:`Right mee_semi_col rec_bindings$. >>
;  
*)

  
(*
  {[
  (<< << { u = .$meta $. } >> >> );
  ExApp 
  (ExApp 
   (ExApp  (ExId  (IdAcc  (IdUid  "Ast") (IdUid  "ExRec")))
     (ExId  (IdLid  "_loc")))
   (ExApp 
     (ExApp 
       (ExApp  (ExId  (IdAcc  (IdUid  "Ast") (IdUid  "RbEq")))
         (ExId  (IdLid  "_loc")))
       (ExApp 
         (ExApp  (ExId  (IdAcc  (IdUid  "Ast") (IdUid  "IdLid")))
           (ExId  (IdLid  "_loc")))
         (ExStr  "u")))
     (ExId  (IdLid  "meta"))))
  (ExApp  (ExId  (IdAcc  (IdUid  "Ast") (IdUid  "ExNil")))
  (ExId  (IdLid  "_loc")))

  ]}
  First we need to construct this part
  {[
  (ExApp 
       (ExApp  (ExId  (IdAcc  (IdUid  "Ast") (IdUid  "RbEq")))
         (ExId  (IdLid  "_loc")))
       (ExApp 
         (ExApp  (ExId  (IdAcc  (IdUid  "Ast") (IdUid  "IdLid")))
           (ExId  (IdLid  "_loc")))
         (ExStr  "u")))
  ]}
  given string input u
  we finally want to make 
  {[
  << << {u = $meta_u$ ; v = $meta_v$ } >> >> 
  ]}
  given string input "u" and [ << meta_u >> ]
 *)
value mee_record_left str =
  let u = << Ast.IdLid _loc .$str:str$. >> in 
  << Ast.RbEq _loc .$u$. >> 
;

(*
   {[
   << <:patt< { u = $meta_u$ ; v = $meta_v$ } >> >> ;
   ]}
 *)  
value mep_record_left str =
  let u = << Ast.IdLid _loc .$str:str$. >> in
  << Ast.PaEq _loc .$u$. >>
;
  
  
value mee_comma x y = << Ast.ExCom _loc .$x$. .$y$. >>
;

  
value mee_app x y =   << Ast.ExApp _loc .$x$. .$y$. >>
;
(*
   {[
    mee_app <:expr<f a >> <:expr<g >> |> eprint 
    Ast.ExApp _loc (f a) g
    ]}
*)
  
value mk_tuple_ee = fun 
  [ [] -> invalid_arg "mktupee arity is zero "
  | [x] -> x
  | xs  ->
      << Ast.ExTup _loc .$ reduce_right mee_comma xs$. >>
  ]
;

value mee_record_col label expr =
  << .$mee_record_left label$. .$expr$. >>
;
  
value mep_record_col label expr =
  << .$mep_record_left label$. .$expr$. >> 
;
value mee_record_semi a b =
  << Ast.RbSem _loc .$a$. .$b$. >>
;

value mep_record_semi a b =
  << Ast.PaSem _loc .$a$. .$b$. >>
;  
(*
   {[
   ( << << {u = $meta_u$ ; v = $meta_v$ } >> >>
     |> e2s ) =
   ((<< << {u = $meta_u$ ; v = $meta_v$ } >> >> |> e2s));

   True
   ]}
   They are syntaxlly different, the first has a trailing Nil.
 *)  
value mk_record_ee label_exprs = let open List in 
  label_exprs
  |> map (fun (label,expr) -> mee_record_col label expr)
  |> (fun es ->
      <<  Ast.ExRec _loc
         .$reduce_right mee_record_semi es $. << >> >> )

;

(*
  Syntactially not equivalent, but dumped result should be the same 
  {[
  (e2s << <:patt< { u = $meta_u$ ; v = $meta_v$ } >> >>
  = 
  e2s (mk_record_ep [ ("u", << meta_u>> ) ; ("v", <<meta_v>>)]))
  ;
  ]}
 *)    
value mk_record_ep label_exprs = let open List in
  label_exprs
  |> map (fun (label,expr) -> mep_record_col label expr)
  |> (fun es ->
     << Ast.PaRec _loc
        .$reduce_right mep_record_semi es $. >> )
;



(* overcome the monomophism restriction
   {[
   eta_expand << f>> 3 |> eprint;
    fun a0 a1 a2 -> f a0 a1 a2
   ]}
 *)
value eta_expand expr number =
  let names = init number (fun i -> x ~off:0 i ) in
  names <+ (expr +> names);


(*
  {[
  gen_curry_n <<3>> ~arity:2 "`X" 2 |> eprint;
  fun [ `X a0 a1 -> fun [ `X b0 b1 -> 3 ] ]
  ]}
 *)
value gen_curry_n acc ~arity cons n =
  let args = init arity
      (fun i -> init n (fun j -> <:patt< .$id:xid ~off:i j$. >>)) in
  let pat = Fan_patt.of_str cons in
  List.fold_right
    (fun p acc -> << fun [ .$p$. -> .$acc$. ] >> )
    (List.map (fun lst -> Fan_patt.apply pat lst) args) acc
;

(*
  
  {[
  currying
  <:match_case<
  (A0 a0 a1,A0 b0 b1) -> 1
  | (A1 a0 a1, A1 b0 b1) -> 2
  | (A2 a0 a1, A2 b0 b1) -> 3
  >> ~arity:2 |> eprint ;
  fun a0 b0 ->
  match (a0, b0) with
  [ (A0 a0 a1, A0 b0 b1) -> 1
  | (A1 a0 a1, A1 b0 b1) -> 2
  | (A2 a0 a1, A2 b0 b1) -> 3 ]
  ]}
  make sure the names generated are shadowed by
  gen_tuple_n
 *)  
value currying match_cases ~arity =
  (* let branches = List.length match_cases in *)
  if  arity >= 2 then 
    let names = init arity (fun i -> x ~off:i 0) in
    let exprs = List.map (fun s-> << .$lid:s$. >> ) names in
    names <+ << match .$tuple_of_list exprs$. with [ .$list:match_cases$. ]>>
  else << fun [ .$list:match_cases$. ] >>;

(*
  {[
  unknown 3 |> eprint;
  fun _ _ _ -> self#unknown
  ]}
    *)
value unknown len =
  if len = 0 then
    << self#unknown>>
  else << failwith .$str:"not implemented!"$. >>;

