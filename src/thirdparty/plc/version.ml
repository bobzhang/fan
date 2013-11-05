(** Implementation of open/closed versions using integers and bit operations **)

type t = (int * int)

let make f acc k =
  let n = 1 lsl k in
  let rec aux acc i =
    if i = n then acc else aux (f acc (i,k)) (i+1) in
  aux acc 0

let opened (i,_k) j =
  (i lsr j) land 1 = 1

let reconstruct bs =
  List.fold_left (fun (o,k) b ->
    ((let o = o lsl 1 in if b then o + 1 else o), k+1)) (0,0) bs


let fold f acc (i,k) =
  let rec aux acc j =
    if j = k then acc else aux (f acc (opened (i,k) j)) (j+1)in
  aux acc 0

let partition v l =
  let (l,cs,os) =
    fold
      (fun (l,cs,os) o ->
        match l with
        |(e::l) -> if o then (l,cs,(e::os)) else (l,(e::cs),os )
        | [] -> failwith "partition") (l,[],[]) v in
  if l <> [] then failwith "partition"
  else (List.rev cs, List.rev os)

let to_string (i,k) =
  Stringf.init k (fun j -> if opened (i,k) j then 'o' else 'c');;

let neg (i,k) =
  (1 lsl k - 1 - i, k)
