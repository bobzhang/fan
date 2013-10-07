

open FAst
open Ast_gen


let _loc =FLoc.ghost 

type ty_meta = {
    str:string;
    print: [ `Exist | `Custom of stru | `Fmt of string];
    eq: [ `Def | `Custom of stru]
  }

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
     `Custom ( {|  let eq_unit : unit -> unit -> bool = fun _ _ -> true |}  ))]

let ty_metas =
  base1_types |> List.map
    (fun (str,print,eq) -> {str;print;eq})
  
let print_base1 = with stru
let items =
  ty_metas |> List.map (fun 
    {str;print;_} ->
      let ty = {:ctyp| Format.formatter -> $lid:str -> unit |} in
      let name = "pp_print_"^str in
      match print with
      |`Exist -> {| let $lid:name = $lid:name |}
      |`Custom s -> s
      |`Fmt c ->
          {|let $lid:name : $ty =
            fun fmt a -> Format.fprintf fmt $str:c a  |} ) in
          sem_of_list items

            
            
let (map_clfield_base_1,
     map_clfield_base_2,
     fold_clfield_base_1,
     fold_clfield_base_2,
     print_clfield_base,
     iter_clfield_base_1,
     eq_clfield_base_2 
    ) =  with clfield
  let ty_names = ty_metas |> List.map (fun {str;_} -> str) in
  let v1 = ty_names |> List.map (fun x ->
              let ty = {:ctyp| $lid:x -> $lid:x |} in
              let exp = {:exp|fun x -> x |} in
              {| method $lid:x : $ty = $exp |} ) in 
  let v2 = ty_names |> List.map (fun x ->
              let ty = {:ctyp| $lid:x -> $lid:x -> $lid:x |} in
              let exp = {:exp| fun x _ ->  x|} in
              {| method $lid:x : $ty = $exp |} ) in
  let v3 = ty_names |> List.map (fun x ->
              let ty = {:ctyp| $lid:x -> 'self_type |} in
              let exp = {:exp|fun _ -> self |} in 
              {| method $lid:x : $ty = $exp |} ) in 
  let v4 = ty_names |> List.map (fun x ->
              let ty = {:ctyp| $lid:x -> $lid:x -> 'self_type |} in
              let exp = {:exp|fun _ _ -> self |} in
              {| method $lid:x : $ty = $exp |} ) in
  let v5 = ty_names |> List.map (fun x ->
    let exp = {:exp|$(lid:"pp_print_"^x)|} in
    {| method $lid:x  = $exp  |} ) in
  let v6 = ty_names |> List.map
    (fun x ->
      let ty = {:ctyp| $lid:x -> unit |} in
      let exp = {:exp| fun _ -> () |} in
      {| method $lid:x : $ty = $exp  |}) in
  let v7 = ty_names |> List.map
    (fun x ->
      let exp = {:exp|fun x y -> x = y|} in
      let ty = {:ctyp| $lid:x -> $lid:x -> bool |} in
      {| method $lid:x : $ty = $exp  |}) in
  (sem_of_list v1, sem_of_list v2, sem_of_list v3,sem_of_list v4,sem_of_list v5,
   sem_of_list v6, sem_of_list v7)


let eq_base1 =
  with stru
  let items =
    ty_metas |> List.map
      (fun 
        {str;eq;_} ->
          let ty =  {:ctyp| $lid:str -> $lid:str -> bool |}  in
          let name = "eq_" ^ str in
          match eq with
          |`Def -> {| let $lid:name : $ty = (=) |}
          |`Custom s -> s ) in
            sem_of_list items


let open AstInjection in begin 
  register_inject_clfield
    ("map_clfield_base_1",map_clfield_base_1);
  register_inject_clfield
    ("map_clfield_base_2",map_clfield_base_2);
  register_inject_clfield
    ("fold_clfield_base_1",fold_clfield_base_1);
  register_inject_clfield
    ("fold_clfield_base_2",fold_clfield_base_2);
  (* val print_clfield_base : clfield *)
  register_inject_clfield
    ("print_clfield_base",print_clfield_base);
  register_inject_clfield
    ("iter_clfield_base_1", iter_clfield_base_1);
  register_inject_clfield
    ("eq_clfield_base_2", eq_clfield_base_2);
  register_inject_stru ("eq_base1",eq_base1);
  register_inject_stru ("print_base1",print_base1)

end



(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/codeTemplate.cmo" *)
(* end: *)
