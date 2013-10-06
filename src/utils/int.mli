(** Int module *)
  
(**
  closed interval 
  {[
  fold_left ~until:3 ~acc:0 (fun acc i -> acc + i);
  int = 6
  ]}
 *)

val fold_left : ?start:int -> until:int -> acc:'a -> ('a -> int -> 'a) -> 'a
