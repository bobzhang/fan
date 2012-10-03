
(** Utility for Ast.ident *)


<:fan<
lang "ident";
>>;

<:include_ml<
"open_template.ml";
>>;

value to_string =
  to_string_of_printer opr#ident
;
  
value eprint v = eprintf "@[%a@]@." opr#ident v
;
(**
   @return a string 
   {[
   lid_of_ident << A.B.c >>;

   string = "c"
   ]}
   
 *)      
value rec lid_of_ident =
    fun
    [ << .$_$. . .$i$. >> -> lid_of_ident i
    | << .$lid:lid$. >> -> lid
    | x  -> invalid_arg ("lid_of_ident" ^ to_string x )  ]
;

(**
   {[
   uid_of_ident <<A.(B.G.h)>>;

   Some (IdAcc  (IdUid  "A") (IdAcc  (IdUid  "B") (IdUid  "G")))
   ]}
  *)
value uid_of_ident =
  let rec aux = 
    fun
    [ << .$a$. . .$lid:lid$. >> -> (** left assoc, pattern right assoc *)
      Some a
    | <@_loc< .$a$. . .$b$. >> ->
        match aux b with
        [None -> assert False
        |Some v ->
            Some << .$a$. . .$v$. >> ]
    | << .$lid:lid$. >> ->
        None
    | _ -> assert False
    ] in
  aux;

(**
   {[
    list_of_acc_ident  << A.B.c >> [];
    [IdUid  "A"; IdUid  "B"; IdLid  "c"]
   ]}
 *)    
value rec list_of_acc_ident x acc =
    match x with
    [ << .$x$. . .$y$. >>  ->
      list_of_acc_ident x (list_of_acc_ident y acc)
    | x -> [x :: acc] ];



(*
  {[

  map_to_string <:ident< A.B.f.g>>;
  a_b_f_g

  map_to_string <:ident< u>> |> print_string;
  u

  map_to_string <:ident<
  Camlp4.Sig.MakeCamlp4Ast(Camlp4.PreCast.Loc).meta_bool >> 

  ]}
  see ident_map 
 *)  
value map_to_string ident =
  let rec aux i acc = match i with 
  [ << .$a$. . .$b$.  >> -> aux a ("_" ^ aux b acc)
  | << .$a$. .$b$. >> -> ("app_" ^(aux a ( "_to_" ^ aux b acc)) ^ "_end")
  | << .$lid:x$. >> -> x ^ acc
  | << .$uid:x$. >> -> String.lowercase x ^ acc
  | t -> invalid_arg & ("map_to_string: " ^ to_string ident) ] in 
    aux ident ""
;


(*
   For qualified identifier, we only use the last qulifier.
   {[
   ident_map (fun x -> "meta_" ^ x ) <:ident< A.B.g >> ;
   IdAcc  (IdUid  "B") (IdLid  "meta_g")

   ident_map (fun x -> "meta_" ^ x ) <:ident< B.g >> ;
   IdAcc  (IdUid  "B") (IdLid  "meta_g")

   ident_map (fun x -> "meta_" ^ x ) <:ident< g >> ;
   Camlp4.PreCast.Ast.ident = IdLid  "meta_g"

   ]}
 *)
value ident_map f x =
  let lst = list_of_acc_ident x [] in
  match lst with
  [ [] ->  invalid_arg "ident_map identifier [] "
  | [ << .$lid:y$. >> ]  -> << .$lid: f y$. >>
  | ls ->
      let l = List.length ls in
      match drop (l-2) ls with
      [ [ q; << .$lid:y$. >> ] ->
        << .$q$. . .$lid: f y$. >>
      | _ -> invalid_arg ("ident_map identifier" ^ to_string x )
      ]];          

(* the same as [ident_map] except f is of type
   [string -> Ast.ident ]
 *)
value ident_map_of_ident f x =
  let lst = list_of_acc_ident x [] in
  match lst with
  [ [] ->  invalid_arg "ident_map identifier [] "
  | [ << .$lid:y$. >> ]  -> f y 
  | ls ->
      let l = List.length ls in
      match drop (l-2) ls with
      [ [ q; << .$lid:y$. >> ] ->
        << .$q$. . .$f y$. >>
      | _ -> invalid_arg ("ident_map identifier" ^ to_string x )
      ]];          
    
(**
   {[
   eprintf "@[%a@]@." opr#ident (ident_map_full (fun x -> "meta_" ^ f  )
   << A.B.s>>);
   ]}
   This kind of map makes shadowing extremely difficult 
   [A.B.meta_s]
 *)  
value ident_map_full f x =
  let _loc = Ast.loc_of_ident x in 
  match (uid_of_ident x ,lid_of_ident x ) with
  [(Some pre, s) ->
    << .$pre$. . .$lid:f s$. >> 
  |(None,s) ->
    << .$lid:f s $. >>   
  ]
 ;

value eq t1 t2 =
  let strip_locs t = (Ast.map_loc (fun _ -> Ast.Loc.ghost))#ident t in
  strip_locs t1 = strip_locs t2;





