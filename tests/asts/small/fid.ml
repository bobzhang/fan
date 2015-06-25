let x ?(off= 0)  =
  function
  | (i : int) ->
      if off > 25
      then invalid_arg "unsupported offset in x "
      else
        (let base = let open Char in ((code 'a') + off) |> chr in
         "_" ^ ((Stringf.of_char base) ^ (string_of_int i)))
let xid ?(off= 0)  = function | (i : int) -> `Lid (x ~off i)
let allx ?(off= 0)  = function | i -> "all" ^ (x ~off i)
let allxid ?(off= 0)  = function | i -> `Lid (allx ~off i)
