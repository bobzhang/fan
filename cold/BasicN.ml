open LibUtil

let x ?(off= 0)  (i : int) =
  if off > 25
  then invalid_arg "unsupported offset in x "
  else
    (let base = let open Char in ((code 'a') + off) |> chr in
     "_" ^ ((String.of_char base) ^ (string_of_int i)))

let xid ?(off= 0)  (i : int) = `Lid (x ~off i)

let allx ?(off= 0)  i = "all" ^ (x ~off i)

let allxid ?(off= 0)  i = `Lid (allx ~off i)

let conversion_table: (string,string) Hashtbl.t = Hashtbl.create 50