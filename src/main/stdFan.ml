
(** The dependency should only rely on the last version of
  [Fan], keep its dependency only on [LibUtil]  *)
#{:control|import Fan.Inject; default "stru";|}
  
open Format




{:stru| eq_base1  |};;
{:stru| print_base1  |};;

let eq_option mf_a x y =
  match (x,y) with
  | (None,None) -> true
  | (Some x,Some y) -> mf_a x y
  | (_,_) -> false 
    
let eq_ref mf_a x y = mf_a  !x !y
  
let pp_print_option mf_a fmt v =
  match v with
  | None -> fprintf fmt "None"
  | Some v -> fprintf fmt "Some @[%a@]" mf_a v 
    
let pp_print_ref mf_a fmt v =
  fprintf fmt "@[{contents=%a}@]" mf_a  !v
  

let pp_print_list mf_a  fmt  lst =
  let open List in 
  fprintf fmt "@[<1>[%a]@]"
   (fun fmt  -> iter (fun x ->
     fprintf fmt "%a@ " mf_a x )) lst
    
let pp_print_exn fmt (e:exn) =
  fprintf fmt "%s" (Printexc.to_string e)
    
let eq_list mf_a  xs ys =
  let rec loop  = function
    | ([],[]) -> true
    | (x::xs,y::ys) -> mf_a x y && loop (xs,ys)
    | (_,_) -> false in
  loop (xs,ys)

let eq_array mf_a  xs ys =
  let lx = Array.length xs and ly = Array.length ys in
  if lx <> ly then false
  else
    let rec loop i = 
      if i >= lx then true
      else if mf_a xs.(i) ys.(i) then loop (i+1) else false  in
    loop 0 
      
let pp_print_array mf_a  fmt  lst =
  let open Array in 
  fprintf fmt "@[<1>[|%a|]@]"
  (fun fmt  -> iter (fun x ->
    fprintf fmt "%a@ " mf_a x )) lst

let eq_arrow _mf_a _mf_b  _a _b = false

let pp_print_arrow _mf_a _f_b fmt _v =
  fprintf fmt "<<<function>>>"



class printbase = object(self:'self_type)
  {:clfield| print_clfield_base|};  
  method list: ! 'a.  ('self_type -> 'fmt -> 'a -> unit) -> 'fmt -> 'a list -> unit =
    fun mf_a fmt lst -> pp_print_list (fun a -> mf_a self a) fmt lst 
  method array: ! 'a. ('self_type -> 'fmt -> 'a -> unit) -> 'fmt -> 'a array -> unit =
    fun mf_a fmt array -> pp_print_array (fun a -> mf_a self a) fmt array
  method option: ! 'a. ('self_type -> 'fmt -> 'a -> unit) -> 'fmt -> 'a option -> unit =
    fun mf_a fmt o -> pp_print_option (fun a -> mf_a self a) fmt o 
  method arrow: ! 'a 'b. ('self_type -> 'fmt -> 'a -> unit) ->
    ('self_type -> 'fmt -> 'b -> unit) ->
    'fmt -> ('a->'b) -> unit = fun _ _ fmt _v -> fprintf fmt "<<<function>>>"
  method ref: !'a. ('self_type ->'fmt-> 'a -> unit)
    -> 'fmt -> 'a ref -> unit =
    fun mf_a fmt  v -> pp_print_ref (mf_a self) fmt v 
  method unknown: ! 'a. Format.formatter -> 'a -> unit = fun _fmt _x -> () 
end

class mapbase = object (self:'self_type)
  {:clfield|map_clfield_base_1|};  
  method list: ! 'a0 'b0. ('self_type -> 'a0 -> 'b0) -> ('a0 list -> 'b0 list) =
    fun mf_a -> function
      | [] -> []
      | y::ys -> (mf_a self y) :: self#list mf_a ys
  method array: ! 'a0 'b0. ('self_type -> 'a0 -> 'b0) -> ('a0 array -> 'b0 array) =
    fun mf_a arr->
      Array.map (fun x -> mf_a self x) arr;
  method option: ! 'a 'b. ('self_type -> 'a -> 'b) -> ('a option -> 'b option) =
    fun mf_a -> function 
      |None -> None
      |Some x -> Some (mf_a self x)
  method arrow: ! 'a0 'a1 'b0 'b1 .
      ('self_type -> 'a0 -> 'b0) -> ('self_type -> 'a1 -> 'b1) ->
        ('a0 -> 'a1) -> ('b0 -> 'b1) = fun _mf_a _mf_b _f ->
          failwith "not implemented in map arrow"
  method ref: !'a 'b. ('self_type ->'a -> 'b) -> ('a ref -> 'b ref) =
    fun mf_a -> fun  (* {contents} *) x  -> ref (mf_a self !x)
  method unknown: !'a. 'a -> 'a = fun x ->x
end 

  
class iterbase = object(self:'self)
  {:clfield| iter_clfield_base_1 |};
  method list: ! 'a0. ('self_type -> 'a0 -> 'unit) -> ('a0 list -> unit) =
    fun mf_a ls -> List.iter (mf_a self) ls 
  method array: ! 'a0 . ('self_type -> 'a0 -> unit) -> ('a0 array -> unit) =
    fun mf_a arr->
      Array.iter (fun x -> mf_a self x) arr
  method option:
      ! 'a . ('self_type -> 'a -> unit) -> ('a option -> unit ) =
    fun mf_a -> function 
      |None -> ()
      |Some x -> mf_a self x 
  method arrow: ! 'a0 'a1 'b0 'b1 .
      ('self_type -> 'a0 -> unit) -> ('self_type -> 'a1 -> unit) ->
        ('a0 -> 'a1) -> ('b0 -> 'b1) = fun _mf_a _mf_b _f ->
          failwith "not implemented in iter arrow"
  method ref: !'a . ('self_type ->'a -> unit) -> ('a ref -> unit) =
    fun mf_a x  ->  mf_a self !x
  method unknown: !'a. 'a -> unit = fun _-> ()
end


class eqbase = object(self:'self)
  {:clfield| eq_clfield_base_2 |};
  method list: ! 'a0. ('self_type -> 'a0 -> 'a0 -> bool) -> ('a0 list -> 'a0 list -> bool) =
    fun mf_a xs ys -> List.for_all2  (mf_a self) xs ys 
  method array: ! 'a0 . ('self_type -> 'a0 ->'a0 -> bool) -> ('a0 array -> 'a0 array-> bool) =
    fun mf_a xs ys ->
      let for_all2 p xs ys =
        let open Array in
        let n = length xs in
        let _ = if length ys <> n then raise (Invalid_argument "Array.for_all2") in
        let rec loop i =
          if i = n then true
          else if p xs.(i) ys.(i) then loop (succ i)
          else false  in
        loop 0 in 
      for_all2  (mf_a self) xs ys 
  method option:
      ! 'a . ('self_type -> 'a -> 'a-> bool) -> ('a option -> 'a option -> bool ) =
        fun mf_a x y->
          match (x, y) with
          |(None,None) -> true
          |(Some x,Some y) -> (mf_a self x y)
          | (_,_) -> false 

  method arrow: ! 'a0 'a1 'b0 'b1 .
      ('self_type -> 'a0 -> bool) -> ('self_type -> 'a1 -> bool) ->
        ('a0 -> 'a1) -> ('b0 -> 'b1) = fun _mf_a _mf_b _f ->
          failwith "not implemented in iter arrow"
  method ref: !'a . ('self_type ->'a -> 'a-> bool) -> ('a ref -> 'a ref -> bool) =
    fun mf_a x y -> mf_a self !x !y
  method unknown: !'a. 'a -> 'a -> bool = fun _ _ -> true
end


class mapbase2 = object (self:'self_type)
  {:clfield|map_clfield_base_2|};  
  method list:! 'a0 'b0.
            ('self_type -> 'a0 -> 'a0 -> 'b0) ->
              'a0 list  -> 'a0 list  -> 'b0 list =
          fun mf_a x y->
            match (x,y) with 
            | ([],[]) -> []
            | (a0:: a1, b0 :: b1 ) ->
                (mf_a self a0 b0) ::  (self#list mf_a a1 b1)
            | (_, _) -> invalid_arg "map2 failure" 
  method array:! 'a0 'b0.
            ('self_type -> 'a0 -> 'a0 -> 'b0) ->
              'a0 array -> 'a0 array -> 'b0 array =
          fun mf_a arr1 arr2 ->
              let lx = Array.length arr1 and ly = Array.length arr2 in
              if lx <> ly then invalid_arg "map2 array length is not equal" 
              else   
                let f = mf_a self in
                let i = f arr1.(0) arr2.(0) in
                let c = Array.create lx i  in begin 
                  for i = 1 to (lx - 1) do 
                    c.(i) <- f arr1.(i) arr2.(i)
                  done;
                  c
                end;

  method option:! 'a0 'b0 . ('self_type -> 'a0 -> 'a0 -> 'b0) ->
    'a0 option -> 'a0 option -> 'b0 option = fun mf_a x y ->
      match (x,y) with
      |(Some x,Some y) -> Some (mf_a self x y)
      | (_,_) -> None
  method ref: !'a0 'b0. ('self_type -> 'a0 -> 'a0 -> 'b0) ->
    'a0 ref -> 'a0 ref -> 'b0 ref = fun mf_a x y ->
      match (x,y) with (x,y)->  ref (mf_a self !x !y)
      
  method arrow: ! 'a0 'b0 'a1 'b1. ('self_type -> 'a0 -> 'a0 ->'b0) ->
    ('self_type -> 'a1 -> 'a1->'b1) ->
      ('a0->'a1)  -> ('a0->'a1)  -> ('b0->'b1) =
      fun _ _ _ -> invalid_arg "map2 arrow is not implemented"
  method unknown: !'a. 'a -> 'a-> 'a = fun x _ -> x 
end 

class monadbase = mapbase
class monadbase2 = mapbase2
  
class foldbase = object (self:'self_type)
  {:clfield|fold_clfield_base_1|}
  method list : ! 'a0. ('self_type -> 'a0 -> 'self_type) ->
    ('a0 list -> 'self_type) = fun mf_a ->
      List.fold_left (fun self v -> (mf_a self v)) self 
  method array: ! 'a0. ('self_type -> 'a0 -> 'self_type) ->
    ('a0 array -> 'self_type) =
    fun mf_a -> 
      Array.fold_left (fun self v -> (mf_a self v)) self 
  method option: ! 'a0. ('self_type -> 'a0 -> 'self_type) ->
    ('a0 option -> 'self_type) = fun mf_a ->
      function
        | None -> self
        | Some x -> mf_a self x 
  method ref: !'a0.('self_type -> 'a0 -> 'self_type) ->
    ('a0 ref -> 'self_type) = fun mf_a -> fun
      x  -> (mf_a self !x) 
  method arrow: ! 'a0 'a1 . ('self_type -> 'a0 -> 'self_type) ->
    ('self_type -> 'a1 -> 'self_type) -> ('a0 -> 'a1) -> 'self_type =
      fun  _ _ _ -> invalid_arg "fold arrow is not implemented"
  method unknown: !'a. 'a -> 'self_type = fun _ -> self
end 
    
class foldbase2 = object (self:'self_type)
  {:clfield|fold_clfield_base_2|}
  method list: ! 'a0.
      ('self_type -> 'a0 ->  'a0 -> 'self_type) ->
        'a0 list -> 'a0 list -> 'self_type =  fun mf_a lx ly->
          List.fold_left2 mf_a self lx ly
  method array: ! 'a0.
      ('self_type -> 'a0 -> 'a0 -> 'self_type) ->
        'a0 array -> 'a0 array -> 'self_type =  fun mf_a lx ly ->
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
                loop 0 in
          fold_left2 mf_a self lx ly
  method option: ! 'a0. ('self_type -> 'a0 -> 'a0 -> 'self_type) ->
    'a0 option -> 'a0 option -> 'self_type = fun mf_a lx ly ->
      match (lx,ly) with
      | (Some x,Some y) -> mf_a self x y
      | (_,_) -> self 
  method ref: !'a0.('self_type -> 'a0 -> 'a0 -> 'self_type) ->
    'a0 ref -> 'a0 ref -> 'self_type =
      fun mf_a x y ->
        match (x,y) with (a, b) -> (mf_a self !a !b) 
  method arrow: !'a0 'a1.
      ('self_type -> 'a0 -> 'a0 -> 'self_type) ->
        ('self_type -> 'a1 -> 'a1 -> 'self_type) ->
          ('a0->'a1) -> ('a0 -> 'a1) ->  'self_type =
            fun _ _ _ -> invalid_arg "fold2 arrow not implemented"
  method unknown: !'a. 'a -> 'a -> 'self_type = fun _ _ -> self
end 

(**
   The difference between [float_repres] and
   [string_of_float] is quite subtle, and meta-explode
   [float] is generally a bad idea, here we adopt
   [string_of_float] for simplicity

let float_repres f =
  let valid_float_lexeme s =
    let l = String.length s in
    let rec loop i =
      if i >= l then s ^ "."
      else
        match s.[i] with
        |'0' .. '9'
        | '-' -> loop (i + 1)
        | _ -> s in
    loop 0 in
  match classify_float f with
  | FP_nan -> "nan"
  | FP_infinite ->
      if f < 0.0 then "neg_infinity" else "infinity"
  | _ ->
      let float_val =
        let s1 = Printf.sprintf "%.12g" f in
        if f = (float_of_string s1) then s1
        else begin
          let s2 = Printf.sprintf "%.15g" f in
          if f = (float_of_string s2) then s2
          else Printf.sprintf "%.18g" f
        end in
      valid_float_lexeme float_val

*)

(* open FAst  *)
(* class primitive =  object *)
(*   method int _loc (i:int)  =  {:ep|$`int:i|} *)
(*   method int32 _loc (i:int32)  = {:ep|$`int32:i|} *)
(*   method int64 _loc  (i:int64)  = {:ep|$`int64:i|} *)
(*   method nativeint _loc (i:nativeint)  = {:ep|$`nativeint:i|} *)
(*   method float _loc (i:float) = {:ep|$`flo:i|} *)
(*   method string _loc (i:string)  = {:ep|$`str:i|} *)
(*   method char _loc (i:char)  = {:ep|$`chr:i|} *)
(*   method unit _loc (_:unit) = {:ep|()|} *)
(*   (\*default use [meta_loc] for expession*\) *)
(*   method loc _loc (_l: loc)  = *)
(*     let n  = !FLoc.name in {:ep|$lid:n|} *)
(*   method ant (_loc:loc) (x:ant)  = (x:>ep) *)
(*   (\* FIXME bool antiquot *\) *)
(*   method bool _loc x = *)
(*     match x with *)
(*     |true -> {:ep|true|} *)
(*     | false -> {:ep| false |} *)
(*   (\* method unknown (_loc:loc) : ! 'a . 'a -> ep  = assert false; *)
(*      method unknown (_loc : loc) = (assert false : 'a . 'a -> ep ) *)
(*      a bug to be FIXED *)
(*    *\) *)
(* end;; *)


    

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/stdFan.cmo " *)
(* end: *)
