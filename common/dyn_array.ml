
type 'a t = {mutable next : int ; mutable data : 'a array}

exception Error
    
let default_size = 32

let create x = {next = 0 ; data = Array.create default_size x}

let reset t = t.next <- 0

(** resize internal only*)    
let incr_table table new_size =
  let t = Array.create new_size table.data.(0) in
  (Array.blit table.data 0 t 0 (Array.length table.data) ;
  table.data <- t)

let add table i =
 let size = Array.length table.data in
 (if table.next >= size then
    incr_table table (2*size);
 (table.data.(table.next) <- i ;
 table.next <- table.next + 1))

let get t i =
  if 0 <= i && i < t.next then
    t.data.(i)
  else raise Error

let to_array' t =
  let r = Array.sub t.data 0 t.next in
  reset t ;
  r
