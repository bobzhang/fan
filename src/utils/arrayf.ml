
include Array
let fold_left2 f acc  a1 a2 =
  let l1 = Array.length a1
  and l2 = Array.length a2 in
  if l1 <> l2 then invalid_arg "Array.fold_left2 length is not equal"
  else
    let acc = ref acc in
    let rec loop i =
      if i < l1 then begin 
        acc := f !acc a1.(i) a2.(i);
        loop (i+1);
      end 
      else
        !acc in
    loop 0 
      (* let of_stream s = *)
      
let stream a =
  Fstream.of_array a
    
    (* let filter_map f arr = *)
let filter_opt t = begin 
  let n = length t in
  let res_size = ref 0 in
  let first_some = ref None in
  (for i = 0 to n - 1 do
    match t.(i) with
    | None -> ()
    | Some _ as s -> begin 
        if !res_size = 0 then first_some := s else () ;
        incr res_size
    end
  done;
   match !first_some with
   | None -> [||]
   | Some el ->
       let result = create (!res_size) el in
       let pos = ref 0 in
       let _ =
         for i = 0 to n - 1 do
           match t.(i) with
           | None -> ()
           | Some x -> begin 
               result.(!pos) <- x;
               incr pos
           end
         done in 
       result)
end
let filter_map f a = filter_opt  (map f a)
let filter_mapi f a = filter_opt (mapi f a)
let for_all2 p xs ys =
  let n = length xs in
  let _ = if length ys <> n then raise (Invalid_argument "Array.for_all2") in
  let rec loop i =
    if i = n then true
    else if p xs.(i) ys.(i) then loop (succ i)
    else false  in
  loop 0
    


(* local variables: *)
(* compile-command: "pmake arrayf.cmo" *)
(* end: *)
