
open Format;
open LibUtil;
open Lib;
module Ast = Camlp4Ast;
open FSig;
let transform =
  let _loc = FanLoc.ghost in
  let open Lib.Ident in with "expr" fun 
  [ `Pre pre ->
     fun [x -> {| $(id: ident_map (fun x ->  pre ^ x) x ) |} ]
  | `Post post ->
     fun [x -> {| $(id:  ident_map (fun x -> x ^ post) x ) |} ]
  | `Fun f ->
     fun [x -> {| $(id:  ident_map f x ) |} ]
  | `Last f ->
      fun [ x -> {| $(id: ident_map_of_ident f x  ) |} ]
         
  | `Ident f->
     fun [x -> {| $(id: f x ) |} ]
  | `Idents f ->
      fun [x -> {| $(id: f (list_of_acc_ident x []) )  |}  ]
  | `Obj f ->
      fun [ {:ident| $lid:x |}  -> {| self# $(f x) |}
          | t -> begin
            let dest =  map_to_string t;
            let src = !Lib.Ident.to_string t;
            if not (Hashtbl.mem Basic.conversion_table src) then begin 
              Hashtbl.add Basic.conversion_table src dest;   
              eprintf "Warning:  %s ==>  %s ==> unknown\n" src dest;
            end
            else ();
            {| self# $(f dest) |} (*todo  set its default let to self#unknown *)
          end]  ];

let basic_transform = fun 
  [ `Pre pre -> (fun x -> pre ^ x)
  | `Post post -> (fun x -> x ^ post)
  | `Fun f -> f ];
  
let right_transform =
  let _loc = FanLoc.ghost in with "expr" fun 
  [ #basic_id_transform as x ->
   (** add as here to overcome the type system *)
    let f = basic_transform x in 
    fun [x -> {| $(lid: f x) |} ]
  | `Exp f -> f ];
  




















