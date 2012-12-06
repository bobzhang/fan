open Camlp4Ast;


(*
  {[
  
  ]}
 *)
let rec normalize_acc =fun
  [ {:ident@_loc| $i1.$i2 |} ->
    {:expr| $(normalize_acc i1).$(normalize_acc i2) |}
  | {:ident@_loc| ($i1 $i2) |} ->
      {:expr| $(normalize_acc i1) $(normalize_acc i2) |}
  | {:ident@_loc| $anti:_ |} | {:ident@_loc| $uid:_ |} |
    {:ident@_loc| $lid:_ |} as i -> {:expr| $id:i |} ];


let rec to_lid = fun
  [ {:ident| $_ . $i |} -> to_lid i
  | {:ident| $lid:lid |} -> lid
  | _                     -> assert false ];


(*
  {[
  mapping an ident to  a type variable 
  ]}
 *)  
let rec tvar_of_ident = fun
  [ {:ident| $lid:x |} | {:ident| $uid:x |} -> x
  | {:ident| $uid:x.$xs |} -> x ^ "__" ^ tvar_of_ident xs
  | _ -> failwith "internal error in the Grammar extension" ];
