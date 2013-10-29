
include List
let rev_len l =
  let rec aux l ((n,acc) as r) =
    match l with
    |[] -> r 
    |x::xs -> aux xs (n+1,x::acc) in
  aux l (0,[])
    
let hd = function
  | [] -> failwith "hd"
  | a::_ -> a

let tl = function
  | [] -> failwith "List.tl"
  | _::l -> l
        
let safe_tl = function
  | [] -> []
  | _::l -> l
        
let null xs = xs = []
    
    (*
      {[
      drop 3 [1;2;3;4];

      list int = [4]
      ]}
     *)
let rec drop n = function
  | _ :: l when n > 0 -> drop (n-1) l
  | l -> l

        (*
          {[
          [1;2;3;4;5]
          ([4;3;2;1], 5 )
          ]}
         *)  
let lastbut1 ls =
  match ls with
  | [ ] -> failwith "lastbut1 empty"
  |  _ -> let l = List.rev ls in
    (List.tl l, List.hd l )
      
let last ls =
  match ls with
  | [] -> failwith "last empty"
  | _ -> List.hd (List.rev ls)
        
(* split_at 3 [1;2;3;4;5;6] = ([1;2;3],[4;5;6])*)    
let  split_at n xs =
  let rec aux  n acc xs = 
    match xs with 
    | [] ->
        if n = 0 then (acc,[])
        else invalid_arg "Index past end of list"
    | (h::t  as l) ->
        if n = 0 then (acc, l)
        else aux (n-1) (h::acc) t  in
  if n < 0 then invalid_arg "split_at n< 0"
  else
    let (a,b) =  aux n [] xs  in
    (rev a ,b)
      
let rec find_map f v =
  match v with
  | [] -> None 
  | x :: xs ->
      match f x with
      | Some _ as y  -> y
      | None -> find_map f xs

(* include BatList;
   return a pair [(int,acc)]
 *)
let fold_lefti f acc ls =
  fold_left (fun (i,acc) x -> (i+1,f i acc x) ) (0,acc) ls
    
(* let fold_righti f ls acc = *)
    (*   fold_right (fun x (acc ) *)
let rec remove x v =
  match v with 
  | (y, _) :: l when y = x -> l
  | d :: l -> (d :: remove x l)
  | [] -> []

let iteri f lst =
  let i = ref 0 in 
  List.iter
    (fun x -> 
      let () = f !i x in  incr i) lst



let reduce_left f lst =
  match lst with
  | [] -> invalid_arg "reduce_left length zero"
  | x::xs ->
      let rec loop x xs =
        match xs with
        | [] -> x
        | y::ys -> loop (f x y) ys in loop x xs
        
let reduce_left_with ~compose ~project lst =     
  match lst with
  | [] -> invalid_arg "reduce_left length zero"
  | x :: xs ->
      let rec loop x xs =
        match xs with
        | [] -> x
        | y :: ys -> loop (compose x  (project y))  ys in
      loop (project x) xs
        
let reduce_right_with ~compose ~f  lst =
  match lst with
  | [] -> invalid_arg "reduce_right length zero"
  | xs ->
      let rec loop xs =
        match xs with
        | [] -> assert false
        | [y] -> f y
        | y::ys -> compose (f y) (loop ys)  in
      loop xs
        
let reduce_right compose = reduce_right_with ~compose ~f:(fun x -> x)
    
let init n f =
  Array.init n f |> Array.to_list


let concat_map f lst =
  fold_right (fun x acc -> f x @ acc) lst []

let rec filter_map f ls =
  match ls with
  | [] -> []
  | x::xs ->
      match f x with
      |Some y -> (y:: filter_map  f xs)
      |None -> filter_map f xs

let filter_mapi f ls =
  let rec aux ls acc = 
    match ls with
    | [] -> []
    | x :: xs ->
      match f acc x with
      | Some y -> (y:: aux xs (acc+1) )
      | None -> aux xs (acc+1) in
  aux ls 0
            
            
let take_rev  n lst =
  let rec aux n l acc =
    match l with
    | [] ->  acc
    | x::xs ->
        if n = 1 then (x::acc)
        else aux (n-1) xs (x::acc) in
  if n <0 then invalid_arg "List.take_rev n<0"
  else if n = 0 then []
  else aux n lst []
let find_opt p l =
  try Some(find p l)
  with Not_found -> None

let rec cross (xs: 'a list list) : 'a list list  =
  match xs with
  | [] -> [[]]
  | xs::xss -> 
      concat_map (fun y -> map (fun x -> x :: y) xs ) (cross xss )
      

(* local variables: *)
(* compile-command: "pmake listf.cmo" *)
(* end: *)
