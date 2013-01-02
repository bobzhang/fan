
(* A pure and re-entrant module *)
open LibUtil;

open Ulex;

(*  the generated code depends on [next] [backtrack] [start] [Error] *)

module Ast = Camlp4Ast;
let _loc = FanLoc.ghost;

  

let get_tables ~tables () = begin 
  let t = Hashtbl.fold (fun key x accu -> [(x,key)::accu]) tables [] ;
  Hashtbl.clear tables;
  t
end;

let table_prefix = "__table_"  ;

let mk_table_name i =
  Printf.sprintf "%s%i" table_prefix i;
let table_name ~tables ~counter t =
  try
     mk_table_name (Hashtbl.find tables t)
  with Not_found -> begin 
    incr counter;
    (* let n = Printf.sprintf "%s%i" table_prefix !counter ; *)
    Hashtbl.add tables t !counter;
    mk_table_name !counter
  end;

let output_byte buf b =
  let open Buffer in begin
    ignore( buf +> '\\' +>  Char.chr (48 + b/100)  +>
            Char.chr (48 + (b/10) mod 10) +> Char.chr (48 + b mod 10))
  end;
  
let output_byte_array v =  begin 
  let b = Buffer.create (Array.length v * 5) ;
  for i = 0 to Array.length v - 1 do
    output_byte b (v.(i) land 0xFF);
    if i land 15 = 15 then Buffer.add_string b "\\\n    " else ()
  done;
  let s = Buffer.contents b ;
  {:expr| $str:s |}
end;

let table (n,t) = {:str_item| let $lid:n = $(output_byte_array t) |};
let binding_table (n,t) = {:binding|  $lid:n = $(output_byte_array t) |};

let partition_prefix = "__partition_";
let partition_name i =
  Printf.sprintf "%s%i" partition_prefix i ;


let partition ~counter ~tables (i,p) =
  let rec gen_tree = function 
    [ Lte (i,yes,no) ->
	{:expr| if (c <= $`int:i) 
	then $(gen_tree yes) else $(gen_tree no) |}
    | Return i ->
	{:expr| $`int:i |}
    | Table (offset, t) ->
	let c = if offset = 0 then {:expr| c |} 
	else {:expr| (c - $`int:offset) |} in
	{:expr| Char.code ($(lid: table_name ~tables ~counter t).[$c]) - 1|} ] in
  let body = gen_tree (simplify LexSet.min_code LexSet.max_code (decision_table p)) in
  let f = partition_name i in
  {:str_item| let $lid:f = fun c -> $body |};

let binding_partition ~counter ~tables (i,p) = 
  let rec gen_tree = function 
    [ Lte (i,yes,no) ->
	{:expr| if (c <= $`int:i) 
	then $(gen_tree yes) else $(gen_tree no) |}
    | Return i ->
	{:expr| $`int:i |}
    | Table (offset, t) ->
	let c = if offset = 0 then {:expr| c |} 
	else {:expr| (c - $`int:offset) |} in
	{:expr| Char.code ($(lid: table_name ~tables ~counter t).[$c]) - 1|} ] in
  let body = gen_tree
      (simplify LexSet.min_code LexSet.max_code (decision_table p)) in
  let f = partition_name i in
  {:binding|  $lid:f = fun c -> $body |};

(* Code generation for the automata *)

let best_final final =
  let fin = ref None in begin 
  Array.iteri 
    (fun i b -> if b && (!fin = None) then fin := Some i else ()) final;
  !fin
  end;

let state_prefix = "__state_";
let mk_state_name i =
  Printf.sprintf "__state_%i" i;
  
let gen_definition _loc l =
  let call_state auto state = with "expr"
    let (_,trans,final) = auto.(state) in
    if Array.length trans = 0 then
      match best_final final with
      [ Some i -> {| $`int:i |}
      | None -> assert false]
    else
      let f = mk_state_name state in
      {| $lid:f lexbuf |} in
  let gen_state auto _loc i (part,trans,final) =
    let f = mk_state_name i in 
    let p = partition_name part in
    let cases =
      Array.mapi 
        (fun i j -> {:match_case| $`int:i -> $(call_state auto j) |})
        trans in
    let cases = Array.to_list cases in
    let body = 
      {:expr|
      match ($lid:p (Ulexing.next lexbuf)) with
      [ $list:cases
      | _ -> Ulexing.backtrack lexbuf ] |} in
    let ret body =
      {:binding| $lid:f = fun lexbuf -> $body |} in
    match best_final final with
    [ None -> ret body
    | Some i -> 
	if Array.length trans = 0 then {:binding||} else
	ret
	  {:expr| begin  Ulexing.mark lexbuf $`int:i;  $body end |} ] in

  let part_tbl = Hashtbl.create 30 in
  let brs = Array.of_list l in
  let rs = Array.map fst brs in
  let auto = Ulex.compile ~part_tbl  rs in
  
  let cases = Array.mapi (fun i (_,e) -> {:match_case| $`int:i -> $e |}) brs in
  let table_counter = ref 0 in 
  let tables = Hashtbl.create 31 in
  let states = Array.mapi (gen_state  auto _loc) auto in
  let partitions =
    List.sort
      (fun (i0,_) (i1,_) -> compare i0 i1)
      (partitions ~part_tbl ()) in 
  let parts = 
    List.map
      (binding_partition ~counter:table_counter ~tables) partitions in
  let tables =
    List.map
      (fun (i,arr) ->
        binding_table (mk_table_name i,arr))
      (List.sort (fun (i0,_) (i1,_) -> compare i0 i1) (get_tables ~tables ()))
  in
  {:expr|
  fun lexbuf ->
    let $list:tables in
    let $list:parts in 
    let rec $(list:Array.to_list states) in
    begin
      Ulexing.start lexbuf;
      match $(lid:mk_state_name 0) lexbuf with
        [ $(list:Array.to_list cases) | _ -> raise Ulexing.Error ]
    end
  |};


(* Lexer specification parser *)
















