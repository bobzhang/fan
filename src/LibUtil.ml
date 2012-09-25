include Format;
module SMap = Map.Make(String);
module SSet = Set.Make(String);
value (|>) x f = f x;
value (&) f x = f x;

type log = string;

type result 'a =
  [ Left of 'a
  | Right of log];

(* value left x  =  Left x ; *)
  
value ret x = Left x ;  
value right x = Right x ;

(** maybe you need more log here *)  
value (>>=) ma f = match ma with
  [ Left v -> f v
  | Right x -> Right x (** rewrite to overcome the type system*)
  ];

value (>>|) ma (str,f) = match ma with
  [ Left v -> f v
  | Right x -> Right (x ^ str) ]
;
  
value (|-) f g  x =
  g (f x );

(**
   append error message later
 *)  
value (>>?) ma str = match ma with
  [ Left _ -> ma
  | Right x -> Right (x^str)]
;
  
value (<|>) fa fb = fun a->
  match fa a with
  [ Left _ as x -> x
  | Right str ->
      fb a >>? str
  ]
;

value unwrap f a = match f a with
  [ Left res -> res
  | Right msg -> invalid_arg msg]
;
  
value failwithf fmt = ksprintf failwith fmt  ;

value opt_bind o f = match o with
  [Some x -> f x
  | None -> None]
;

value opt_map o f = match o with
  [ Some x -> Some f x
  | None -> None]
;
  
value prerr_endlinef fmt = ksprintf prerr_endline fmt;

value verbose = ref 1;  
value dprintf ?(log_level=1)   =
  if !verbose >= log_level then
    (* prerr_endlinef fmt str *)
    eprintf
  else ifprintf err_formatter;


value ends_with s e = 
   let ne = String.length e
   and ns = String.length s in
   ns >= ne && String.sub s (ns-ne) ne = e 
;

value starts_with s e =
  let ne = String.length e and ns = String.length s in
  ns >= ne && String.sub s 0 ne = e
;    
value is_antiquot_data_ctor s =
       ends_with s "Ant"
;

(**
   [neg_string "ab" ] = ["-ab"]
   [neg_string ""] = ["-"]
 *)
value neg_string n =
  let len = String.length n in
  if len > 0 && n.[0] = '-' then String.sub n 1 (len - 1)
  else "-" ^ n
;
  
(** f will be incremented for each item
    indexed from 0 *)
value fold_lefti
     (f : 'a -> int -> 'b -> 'b )
     (ty: list 'a)  (init:'b) : 'b =
    let (_,res) = List.fold_left begin fun (i,acc) ty ->
      (i+1, f ty i acc)
    end (0, init) ty
    in res
;


value  mapi_m f xs =
    let rec aux acc xs = 
      match xs with
    [ [] ->  ret []
    | [x::xs] ->
        f x acc >>= (fun x ->
        aux (acc+1) xs >>= (fun xs ->
        (ret [x::xs] )))]
    in aux 0 xs
;
    
value mapi f xs =
  let rec aux acc xs = match xs with
    [ [] -> []
    | [x::xs] ->
        [f x acc :: aux (acc+1) xs ]
    ] in
  aux 0 xs
;
  
(**
   [start,until]
 *)
value fold_nat_left ?(start=0) ~until ~acc f = do{
  let v = ref acc ;
  for x = start to until do
    v.val := f v.val x 
  done;
  v.val  
}
;  


value iteri f lst =
  let i = ref 0 in 
  List.iter (fun x -> 
    let () = f i.val x in
    incr i) lst
;    

type dir = [= `Left | `Right];

value reduce_left f lst =
  match lst with
    [ [] -> invalid_arg "reduce_left length zero"
    | [x::xs] ->
        let rec loop x xs = match xs with
          [ [] -> x
          | [y::ys] -> loop (f x y) ys] in loop x xs];
    
value reduce_right_with ~compose ~f  lst =
  match lst with
    [ [] -> invalid_arg "reduce_right length zero"
    | xs ->
        let rec loop xs = match xs with
          [ [] -> assert False
          | [y] -> f y
          | [y::ys] -> compose (f y) (loop ys) ] in loop xs];
value reduce_right compose = reduce_right_with ~compose ~f:(fun x -> x);


(*
  {[
  string_drop_while (fun x -> x = '_') "__a";
  string = "a"
  string_drop_while (fun x -> x = '_') "a";
  string = "a"
  string_drop_while (fun x -> x = '_') "";
  string = ""
  ]}
 *)  
value string_drop_while f s =
  let len = String.length s in
  let found = ref False in
  let i = ref 0 in begin 
  while i.val < len && not found.val do
    if not (f s.[i.val]) then 
      found.val:=True
    else incr i
  done ;
  String.sub s i.val (len - i.val)
  end 
;      

  
exception First;

(** do sequence does not support try with
    use do only for simple construct
 *)
value find_first f lst =
  let res = ref None in
  try
    List.iter (fun x ->
    match f x with
    [ Some v -> do{ res.val := Some v; raise First}
    | None -> ()]) lst;
    None;
  with [First ->  res.val]
;
  

value mkmke preds (cons,tyargs)=
  match find_first (fun pred -> pred(cons,tyargs)) preds with
  [ None -> invalid_arg "mkmke"
  | Some r -> r ]
;

value adapt f a =
  Some (f a )
;

(* #default_quotation "expr"; *)
(* Camlp4.PreCast.Syntax.Quotation.default.val := "expr"; *)


value rec intersperse y xs = match xs with
  [ [] -> []
  | [x] -> xs
  | [x::xs] ->
      [x ; y :: intersperse y xs]
  ]
;  

value init n f =
  Array.( init n f |> to_list)
;    



value to_string_of_printer printer v =
  let buf = Buffer.create 30 in 
  let () = Format.bprintf buf "@[%a@]" printer v in 
  Buffer.contents buf 
;

(**
   {[
   mk_anti ~c:"binding" "list" "code" ;
   
   string = "\\$listbinding:code"
   ]}
 *)    
value mk_anti ?(c = "") n s = "\\$"^n^c^":"^s;

(** \\$expr;:code *)
value is_antiquot s =
  let len = String.length s in
  len > 2 && s.[0] = '\\' && s.[1] = '$';

  
value handle_antiquot_in_string ~s ~term ~parse ~loc ~decorate =
  if is_antiquot s then
    let pos = String.index s ':' in
    let name = String.sub s 2 (pos - 2)
    and code = String.sub s (pos + 1) (String.length s - pos - 1) in
    decorate name (parse loc code)
  else term;

value is_capital c =
  let c = Char.code c  in
  (c >= Char.code 'A' &&
   c <= Char.code 'Z')
;
  
value is_digit c =
  let c = Char.code c in
  (c >= Char.code '0' && c <= Char.code '9')
;    
(**
   {[
   destruct_poly "`a";
   Some "a"
   ]}
 *)
value destruct_poly s =
  let n = String.length s in
  if n = 0 then
    invalid_arg "destruct_poly length=0"
  else
    if s.[0] = '`' then
      Some (String.sub s 1 (n-1))
    else None
;

value uncurry f (x,y)  =f x y;
(**
   {[
   drop 3 [1;2;3;4];

   list int = [4]
   ]}
 *)  
value rec drop n = fun
  [ [_ :: l] when n > 0 -> drop (n-1) l
  | l -> l]
;

(*
  {[
  [1;2;3;4;5]
  ([4;3;2;1], 5 )
  ]}
 *)
value lastbut1 ls = match ls with
  [ [ ] -> failwith "lastbut1 empty"
  |  _ -> let l = List.rev ls in
     (List.tl l, List.hd l ) ];
  
