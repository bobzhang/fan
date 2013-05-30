type 'a t =  {
  mutable next: int;
  mutable data: 'a array} 

let default_size = 32

let create x = { next = 0; data = (Array.create default_size x) }
and reset t = t.next <- 0

let incr_table table new_size =
  let t = Array.create new_size ((table.data).(0)) in
  begin
    Array.blit table.data 0 t 0 (Array.length table.data); table.data <- t
  end

let emit table i =
  let size = Array.length table.data in
  begin
    if table.next >= size then incr_table table (2 * size);
    (table.data).(table.next) <- i; table.next <- table.next + 1
  end

exception Error

let get t i = if (0 <= i) && (i < t.next) then (t.data).(i) else raise Error

let trim t = let r = Array.sub t.data 0 t.next in begin reset t; r end

let iter t f =
  let size = t.next and data = t.data in
  for i = 0 to size - 1 do f (data.(i)) done

let size t = t.next