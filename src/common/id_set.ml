(** A common utility module
    when doing code generation *)
include Set.Make(
  struct
    type t = Locf.t * string
    let compare (_,x) (_,y) = String.compare x y
  end
 )
