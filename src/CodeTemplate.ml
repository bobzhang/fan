
open AstLoc;
open LibUtil;

let _loc =FanLoc.ghost ;

type ty_meta = {
    str:string;
    print: [= `Exist | `Custom of stru | `Fmt of string];
    eq: [= `Def | `Custom of stru]
  };

let base1_types = with stru
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
  
let print_base1 = with stru
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
      sem_of_list items;

  
  
let (map_cstru_base_1,
     map_cstru_base_2,
     fold_cstru_base_1,
     fold_cstru_base_2,
     print_cstru_base,
     iter_cstru_base_1,
     eq_cstru_base_2 
    ) =  with cstru
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
  (sem_of_list v1, sem_of_list v2, sem_of_list v3,sem_of_list v4,sem_of_list v5,
   sem_of_list v6, sem_of_list v7)

  (* ({|$list:v1|},{|$list:v2|},{|$list:v3|}, *)
  (*  {|$list:v4|},{|$list:v5|},{|$list:v6|},{|$list:v7|}) *);

let eq_base1 = with stru
  let items = ty_metas |> List.map (fun [
  {str;eq;_} ->
    let ty =  {:ctyp| $lid:str -> $lid:str -> bool |}  in
    let name = "eq_" ^ str in
    match eq with
    [`Def -> {| let $lid:name : $ty = (=) |}
    |`Custom s -> s ]]) in
    sem_of_list items
    (* {| $list:items |} *) ;

let open AstInjection in begin 
  register_inject_cstru
    ("map_cstru_base_1",map_cstru_base_1);
  register_inject_cstru
    ("map_cstru_base_2",map_cstru_base_2);
  register_inject_cstru
    ("fold_cstru_base_1",fold_cstru_base_1);
  register_inject_cstru
    ("fold_cstru_base_2",fold_cstru_base_2);
  (* val print_cstru_base : cstru *)
  register_inject_cstru
    ("print_cstru_base",print_cstru_base);
  register_inject_cstru
    ("iter_cstru_base_1", iter_cstru_base_1);
  register_inject_cstru
    ("eq_cstru_base_2", eq_cstru_base_2);
  register_inject_stru ("eq_base1",eq_base1);
  register_inject_stru ("print_base1",print_base1);

end;






