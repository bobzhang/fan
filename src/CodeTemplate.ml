
open AstLoc;
open LibUtil;

let _loc =FanLoc.ghost ;

type ty_meta = {
    str:string;
    print: [= `Exist | `Custom of str_item | `Fmt of string];
    eq: [= `Def | `Custom of str_item]
  };

let base1_types = with str_item
  [ ("int", `Exist , `Def) ;
    ("int32", `Fmt "%ld", `Def);
    ("int64", `Fmt "%Ld", `Def );
    ("nativeint", `Fmt "%nd", `Def);
    ("float", `Exist, `Def);
    ("string", `Fmt "%S", `Def );
    ("bool", `Exist, `Def);    
    ("char", `Exist, `Def);
    ("unit", `Custom (
     {| let pp_print_unit : Format.formatter -> unit -> unit = fun fmt _ ->
          Format.fprintf fmt "()" |} ),
     `Custom ( {|  let eq_unit : unit -> unit -> bool = fun _ _ -> true |}  ))];

let ty_metas =
  base1_types |> List.map (fun
    [(str,print,eq) ->
      {str;print;eq}]);
  
let print_base1 = with str_item
  let items =
    ty_metas |> List.map (fun [
      {str;print;_} ->
    let ty = {:ctyp| Format.formatter -> $lid:str -> unit |} in
    let name = "pp_print_"^str in
    match print with
    [`Exist -> {| let $lid:name = $lid:name |}
    |`Custom s -> s
    |`Fmt c ->
      {|let $lid:name : $ty =
        fun fmt a -> Format.fprintf fmt $str:c a  |} ]]) in
      sem_of_list1 items;

  
  
let (map_class_str_item_base_1,
     map_class_str_item_base_2,
     fold_class_str_item_base_1,
     fold_class_str_item_base_2,
     print_class_str_item_base,
     iter_class_str_item_base_1,
     eq_class_str_item_base_2 
    ) =  with class_str_item
  let ty_names = ty_metas |> List.map (fun [{str;_} -> str]) in
  let v1 = ty_names |> List.map (fun x ->
              let ty = {:ctyp| $lid:x -> $lid:x |} in
              let exp = {:expr|fun x -> x |} in
              {| method $lid:x : $ty = $exp |} ) in 
  let v2 = ty_names |> List.map (fun x ->
              let ty = {:ctyp| $lid:x -> $lid:x -> $lid:x |} in
              let exp = {:expr| fun x _ ->  x|} in
              {| method $lid:x : $ty = $exp |} ) in
  let v3 = ty_names |> List.map (fun x ->
              let ty = {:ctyp| $lid:x -> 'self_type |} in
              let exp = {:expr|fun _ -> self |} in 
              {| method $lid:x : $ty = $exp |} ) in 
  let v4 = ty_names |> List.map (fun x ->
              let ty = {:ctyp| $lid:x -> $lid:x -> 'self_type |} in
              let exp = {:expr|fun _ _ -> self |} in
              {| method $lid:x : $ty = $exp |} ) in
  let v5 = ty_names |> List.map (fun x ->
    let exp = {:expr|$(lid:"pp_print_"^x)|} in
    {| method $lid:x  = $exp  |} ) in
  let v6 = ty_names |> List.map
    (fun x ->
      let ty = {:ctyp| $lid:x -> unit |} in
      let exp = {:expr| fun _ -> () |} in
      {| method $lid:x : $ty = $exp  |}) in
  let v7 = ty_names |> List.map
    (fun x ->
      let exp = {:expr|fun x y -> x = y|} in
      let ty = {:ctyp| $lid:x -> $lid:x -> bool |} in
      {| method $lid:x : $ty = $exp  |}) in
  (sem_of_list1 v1, sem_of_list1 v2, sem_of_list1 v3,sem_of_list1 v4,sem_of_list1 v5,
   sem_of_list1 v6, sem_of_list1 v7)

  (* ({|$list:v1|},{|$list:v2|},{|$list:v3|}, *)
  (*  {|$list:v4|},{|$list:v5|},{|$list:v6|},{|$list:v7|}) *);

let eq_base1 = with str_item
  let items = ty_metas |> List.map (fun [
  {str;eq;_} ->
    let ty =  {:ctyp| $lid:str -> $lid:str -> bool |}  in
    let name = "eq_" ^ str in
    match eq with
    [`Def -> {| let $lid:name : $ty = (=) |}
    |`Custom s -> s ]]) in
    sem_of_list1 items
    (* {| $list:items |} *) ;

let open AstInjection in begin 
  register_inject_class_str_item
    ("map_class_str_item_base_1",map_class_str_item_base_1);
  register_inject_class_str_item
    ("map_class_str_item_base_2",map_class_str_item_base_2);
  register_inject_class_str_item
    ("fold_class_str_item_base_1",fold_class_str_item_base_1);
  register_inject_class_str_item
    ("fold_class_str_item_base_2",fold_class_str_item_base_2);
  (* val print_class_str_item_base : class_str_item *)
  register_inject_class_str_item
    ("print_class_str_item_base",print_class_str_item_base);
  register_inject_class_str_item
    ("iter_class_str_item_base_1", iter_class_str_item_base_1);
  register_inject_class_str_item
    ("eq_class_str_item_base_2", eq_class_str_item_base_2);
  register_inject_str_item ("eq_base1",eq_base1);
  register_inject_str_item ("print_base1",print_base1);

end;






