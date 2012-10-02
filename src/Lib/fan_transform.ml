<:fan<
lang "expr";
>>;

<:include_ml<
"open_template.ml";
>>;




value transform = let open Fan_ident in fun 
  [ `Pre pre ->
     fun [x -> << .$id: ident_map (fun x ->  pre ^ x) x $. >> ]
  | `Post post ->
     fun [x -> << .$id:  ident_map (fun x -> x ^ post) x $. >> ]
  | `Fun f ->
     fun [x -> << .$id:  ident_map f x $. >> ]
  | `Last f ->
      fun [ x -> << .$id: ident_map_of_ident f x  $. >> ]
         
  | `Ident f->
     fun [x -> << .$id: f x $. >> ]
  | `Idents f ->
      fun [x -> << .$id: f (list_of_acc_ident x []) $.  >>  ]
  | `Obj f ->
      fun [ <:ident< .$lid:x$. >>  -> << self# .$f x$. >>
          | t -> do{
            let dest =  map_to_string t;
            let src = Fan_ident.to_string t;
            if not (Hashtbl.mem conversion_table src) then begin 
              Hashtbl.add conversion_table src dest;   
              eprintf "Warning:  %s ==>  %s ==> unknown\n" src dest;
            end
            else ();
            << self# .$f dest$. >> (*todo  set its default value to self#unknown *)
           }]
  ]
;

value basic_transform = fun 
  [ `Pre pre -> (fun x -> pre ^ x)
  | `Post post -> (fun x -> x ^ post)
  | `Fun f -> f 
  ]
;
  
value right_transform = fun 
  [ #basic_id_transform as x ->
   (** add as here to overcome the type system *)
    let f = basic_transform x in 
    fun [x -> << .$lid: f x$. >> ]
  | `Exp f -> f 
  ]
;
  




















