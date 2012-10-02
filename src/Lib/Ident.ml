open Camlp4Ast;


(*
  {[
  
  ]}
 *)
let rec normalize_acc =fun
  [ <:ident@_loc< $i1.$i2 >> ->
    <:expr< $(normalize_acc i1).$(normalize_acc i2) >>
  | <:ident@_loc< $i1 $i2 >> ->
      <:expr< $(normalize_acc i1) $(normalize_acc i2) >>
  | <:ident@_loc< $anti:_ >> | <:ident@_loc< $uid:_ >> |
    <:ident@_loc< $lid:_ >> as i -> <:expr< $id:i >> ];

