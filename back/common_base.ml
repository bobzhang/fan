open Format;
open Camlp4.PreCast;

<:fan<
lang_at "str_item" "fan_str_item";
lang_at "class_str_item" "fan_class_str_item";
>> ;  
(** A module support for basic types.
    For basic types, it was generated
    by plugins *)
value uncurry f (x,y)  =f x y;

 
  
module Array = struct
  include Array;
  value fold_left2 f acc  a1 a2 =
    let l1 = Array.length a1
    and l2 = Array.length a2 in
    if l1 <> l2 then invalid_arg "Array.fold_left2 length is not equal"
    else
      let acc = ref acc in
      let rec loop i =
        if i < l1 then begin 
          acc.val := f acc.val a1.(i) a2.(i);
          loop (i+1);
        end 
        else
          acc.val in
      loop 0 ;
end;

<< "code_template.cmo" "eq_base1" >> ;
<< "code_template.cmo" "print_base1">>;
  
value eq_option mf_a x y = match (x,y) with
  [ (None,None) -> True
  | (Some x,Some y) -> mf_a x y
  | (_,_) -> False ];
value eq_ref mf_a x y = mf_a  x.val y.val;
value pp_print_option mf_a fmt v = match v with
  [ None -> fprintf fmt "None"
  | Some v -> fprintf fmt "Some @[%a@]" mf_a v ];
value pp_print_ref mf_a fmt v =
  fprintf fmt "@[{contents=%a}@]" mf_a  v.val;  
value eq_list mf_a  xs ys =
  let rec loop  = fun
    [ ([],[]) -> True
    | ([x::xs],[y::ys]) -> mf_a x y && loop (xs,ys)
    | (_,_) -> False] in
  loop (xs,ys);

value pp_print_list mf_a  fmt  lst = let open List in 
  fprintf fmt "@[<1>[%a]@]"
   (fun fmt  -> iter (fun x ->
     fprintf fmt "%a@ " mf_a x )) lst ;
  
value eq_array mf_a  xs ys =
  let lx = Array.length xs and ly = Array.length ys in
  if lx <> ly then False
  else
    let rec loop = fun
      [ i -> if i >= lx then True
       else if mf_a xs.(i) ys.(i) then loop (i+1) else False ] in
    loop 0 ;
    
value pp_print_array mf_a  fmt  lst = let open Array in 
  fprintf fmt "@[<1>[|%a|]@]"
  (fun fmt  -> iter (fun x ->
    fprintf fmt "%a@ " mf_a x )) lst;

value eq_arrow mf_a mf_b  a b = False;
value pp_print_arrow mf_a f_b fmt v =
  fprintf fmt "<<<function>>>";

class printbase = object(self:'self_type)
  << "code_template.cmo"  "print_class_str_item_base" >> ;
  method list: ! 'a.  ('self_type -> 'fmt -> 'a -> unit) -> 'fmt -> list 'a -> unit =
    fun mf_a fmt lst -> pp_print_list (fun a -> mf_a self a) fmt lst ;
  method array: ! 'a. ('self_type -> 'fmt -> 'a -> unit) -> 'fmt -> array 'a -> unit =
    fun mf_a fmt array -> pp_print_array (fun a -> mf_a self a) fmt array;
  method option: ! 'a. ('self_type -> 'fmt -> 'a -> unit) -> 'fmt -> option 'a -> unit =
    fun mf_a fmt o -> pp_print_option (fun a -> mf_a self a) fmt o ;
  method arrow: ! 'a 'b. ('self_type -> 'fmt -> 'a -> unit) -> ('self_type -> 'fmt -> 'b -> unit) ->
    'fmt -> ('a->'b) -> unit = fun _ _ fmt v -> fprintf fmt "<<<function>>>";
  method ref: !'a. ('self_type ->'fmt-> 'a -> unit)
    -> 'fmt -> ref 'a -> unit =
    fun mf_a fmt  v -> pp_print_ref (mf_a self) fmt v ;
  method unknown: ! 'a. Format.formatter -> 'a -> unit = fun fmt x -> () ;
end;

class mapbase = object (self:'self_type)
  <<  "code_template.cmo" "map_class_str_item_base_1" >>;  
  method list: ! 'a0 'b0. ('self_type -> 'a0 -> 'b0) -> (list 'a0 -> list 'b0) =
    fun mf_a -> fun [ [] -> []
    | [y::ys] -> [ (mf_a self y) :: self#list mf_a ys]];
  method array: ! 'a0 'b0. ('self_type -> 'a0 -> 'b0) -> (array 'a0 -> array 'b0) =
    fun mf_a arr->
      Array.map (fun x -> mf_a self x) arr;
  method option: ! 'a 'b. ('self_type -> 'a -> 'b) -> (option 'a -> option 'b) =
    fun mf_a oa -> match oa with
      [None -> None
      |Some x -> Some (mf_a self x)];
  method arrow: ! 'a0 'a1 'b0 'b1 .
      ('self_type -> 'a0 -> 'b0) -> ('self_type -> 'a1 -> 'b1) ->
        ('a0 -> 'a1) -> ('b0 -> 'b1) = fun mf_a mf_b f ->
          failwith "not implemented in map arrow";
  method ref: !'a 'b. ('self_type ->'a -> 'b) -> (ref 'a -> ref 'b) =
    fun mf_a -> fun [{val} -> {val= mf_a self val}];
  method unknown: !'a. 'a -> 'a = fun x ->x;         
end ;


    

class mapbase2 = object (self:'self_type)
  << "code_template.cmo"  "map_class_str_item_base_2" >> ;  
  method list:! 'a0 'b0.
            ('self_type -> 'a0 -> 'a0 -> 'b0) ->
              list 'a0  -> list 'a0  -> list 'b0 =
          fun mf_a x y-> match (x,y) with 
            [ ([],[]) -> []
            | ([a0:: a1], [b0 :: b1] ) ->
                [(mf_a self a0 b0) ::  (self#list mf_a a1 b1)]
            | (_, _) -> invalid_arg "map2 failure" ];
  method array:! 'a0 'b0.
            ('self_type -> 'a0 -> 'a0 -> 'b0) ->
              array 'a0 -> array 'a0 -> array 'b0 =
          fun mf_a arr1 arr2 ->
              let lx = Array.length arr1 and ly = Array.length arr2 in
              if lx <> ly then invalid_arg "map2 array length is not equal" 
              else   
                let f = mf_a self in
                let i = f arr1.(0) arr2.(0) in
                let c = Array.create lx i  in begin 
                  for i = 1 to (lx - 1) do 
                    c.(i) := f arr1.(i) arr2.(i)
                  done;
                  c
                end;

  method option:! 'a0 'b0 . ('self_type -> 'a0 -> 'a0 -> 'b0) ->
    option 'a0 -> option 'a0 -> option 'b0 = fun mf_a x y ->
      match (x,y) with
      [(Some x,Some y) -> Some (mf_a self x y)
      | (_,_) -> None];
  method ref: !'a0 'b0. ('self_type -> 'a0 -> 'a0 -> 'b0) ->
    ref 'a0 -> ref 'a0 -> ref 'b0 = fun mf_a x y -> match (x,y) with
     [({val=a},{val=b})-> {val=mf_a self a b }];
      
  method arrow: ! 'a0 'b0 'a1 'b1. ('self_type -> 'a0 -> 'a0 ->'b0) ->
    ('self_type -> 'a1 -> 'a1->'b1) ->
      ('a0->'a1)  -> ('a0->'a1)  -> ('b0->'b1) =
      fun _ _ _ -> invalid_arg "map2 arrow is not implemented";
  method unknown: !'a. 'a -> 'a-> 'a = fun x _ -> x ;
end ;

class monadbase = mapbase;
class monadbase2 = mapbase2;
  
class foldbase = object (self:'self_type)
  << "code_template.cmo" "fold_class_str_item_base_1" >> ; 
  method list : ! 'a0. ('self_type -> 'a0 -> 'self_type) ->
    (list 'a0 -> 'self_type) = fun mf_a ->
      List.fold_left (fun self v -> (mf_a self v)) self ;
  method array: ! 'a0. ('self_type -> 'a0 -> 'self_type) ->
    (array 'a0 -> 'self_type) =
    fun mf_a -> 
      Array.fold_left (fun self v -> (mf_a self v)) self ;
  method option: ! 'a0. ('self_type -> 'a0 -> 'self_type) ->
    (option 'a0 -> 'self_type) = fun mf_a ->
      fun
        [None -> self
        |Some x -> mf_a self x ];
  method ref: !'a0.('self_type -> 'a0 -> 'self_type) ->
    (ref 'a0 -> 'self_type) = fun mf_a -> fun
      [ {val} -> mf_a self val];
  method arrow: ! 'a0 'a1 . ('self_type -> 'a0 -> 'self_type) ->
    ('self_type -> 'a1 -> 'self_type) -> ('a0 -> 'a1) -> 'self_type =
      fun  _ _ _ -> invalid_arg "fold arrow is not implemented";
  method unknown: !'a. 'a -> 'self_type = fun _ -> self;
end ;
  
class foldbase2 = object (self:'self_type)
  << "code_template.cmo" "fold_class_str_item_base_2" >>;
  method list: ! 'a0.
      ('self_type -> 'a0 ->  'a0 -> 'self_type) ->
        list 'a0 -> list 'a0 -> 'self_type =  fun mf_a lx ly->
          List.fold_left2 mf_a self lx ly;
  method array: ! 'a0.
      ('self_type -> 'a0 -> 'a0 -> 'self_type) ->
        array 'a0 -> array 'a0 -> 'self_type =  fun mf_a lx ly -> 
          Array.fold_left2 mf_a self lx ly;
  method option: ! 'a0. ('self_type -> 'a0 -> 'a0 -> 'self_type) ->
    option 'a0 -> option 'a0 -> 'self_type = fun mf_a lx ly ->
      match (lx,ly) with
      [ (Some x,Some y) -> mf_a self x y
      | (_,_) -> self ];
  method ref: !'a0.('self_type -> 'a0 -> 'a0 -> 'self_type) ->
    ref 'a0 -> ref 'a0 -> 'self_type = fun mf_a x y -> match (x,y) with
      [ ({val=a},{val=b}) ->
        mf_a self a b ];
  method arrow: !'a0 'a1.
      ('self_type -> 'a0 -> 'a0 -> 'self_type) ->
        ('self_type -> 'a1 -> 'a1 -> 'self_type) ->
          ('a0->'a1) -> ('a0 -> 'a1) ->  'self_type =
            fun _ _ _ -> invalid_arg "fold2 arrow not implemented";
  method unknown: !'a. 'a -> 'a -> 'self_type = fun _ _ -> self;
end ;

      
  (* method map: ! 'a. (Format.formatter -> 'a -> unit) ->
   *   (Format.formatter -> Map.t 'a -> unit) = fun fmt map -> 
   *     Format.fprintf fmt "@[{%a}@]"
   *     (fun fmt m -> Types.Meths.iter
   *         (fun k v
   *           -> Format.fprintf fmt "%a=>%a"
   *               pp_print_string k mf_v v) m) map *)
