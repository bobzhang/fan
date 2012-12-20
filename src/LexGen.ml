
(*
  the generated code depends on [next] [backtrack] [start] [Error]
  *)
module Ast = Camlp4Ast;
let _loc = FanLoc.ghost;
open Ulex;
  
let tables = Hashtbl.create 31;
let tables_counter = ref 0;
let get_tables () = begin 
  let t = Hashtbl.fold (fun key x accu -> [(x,key)::accu]) tables [] ;
  Hashtbl.clear tables;
  t
end;
  
let table_name t =
  try Hashtbl.find tables t
  with Not_found -> begin 
    incr tables_counter;
    let n = Printf.sprintf "__ulex_table_%i" !tables_counter ;
    Hashtbl.add tables t n;
    n
  end;

let output_byte buf b = begin 
  Buffer.add_char buf '\\';
  Buffer.add_char buf (Char.chr(48 + b / 100));
  Buffer.add_char buf (Char.chr(48 + (b / 10) mod 10));
  Buffer.add_char buf (Char.chr(48 + b mod 10))
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

let partition_name i = Printf.sprintf "__ulex_partition_%i" i;

let partition (i,p) =
  let rec gen_tree = function 
    [ Lte (i,yes,no) ->
	{:expr| if (c <= $`int:i) 
	then $(gen_tree yes) else $(gen_tree no) |}
    | Return i ->
	{:expr| $`int:i |}
    | Table (offset, t) ->
	let c = if offset = 0 then {:expr| c |} 
	else {:expr| (c - $`int:offset) |} in
	{:expr| Char.code ($(lid: table_name t).[$c]) - 1|} ] in
  let body = gen_tree (simplify (-1) (Cset.max_code) (decision_table p)) in
  let f = partition_name i in
  {:str_item| let $lid:f = fun c -> $body |};


(* Code generation for the automata *)

let best_final final =
  let fin = ref None in begin 
  Array.iteri 
    (fun i b -> if b && (!fin = None) then fin := Some i else ()) final;
  !fin
  end;

let call_state auto state =
  match auto.(state) with (_,trans,final) ->
    if Array.length trans = 0 
    then match best_final final with
      [ Some i -> {:expr| $`int:i |}
      | None -> assert false]
    else
      let f = Printf.sprintf "__ulex_state_%i" state in
      {:expr| $lid:f lexbuf |};
	

let gen_state auto _loc i (part,trans,final) = 
  let f = Printf.sprintf "__ulex_state_%i" i in
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
	  {:expr| begin  Ulexing.mark lexbuf $`int:i;  $body end |} ];


let gen_definition _loc l =
  let brs = Array.of_list l in
  let rs = Array.map fst brs in
  let auto = Ulex.compile rs in

  let cases = Array.mapi (fun i (_,e) -> {:match_case| $`int:i -> $e |}) brs in
  let states = Array.mapi (gen_state auto _loc) auto in
  {:expr| fun lexbuf ->
    let rec $(list:Array.to_list states) in
    begin
      Ulexing.start lexbuf;
      match __ulex_state_0 lexbuf with
        [ $(list:Array.to_list cases) | _ -> raise Ulexing.Error ]
    end |};


(* Lexer specification parser *)

(* let char_int s = *)
(*   let i = int_of_string s in *)
(*   if (i >=0) && (i <= Cset.max_code) then i *)
(*   else failwith ("Invalid Unicode code point: " ^ s); *)

let regexp_for_string s =
  let rec aux n =
    if n = String.length s then Ulex.eps
    else 
      Ulex.seq (Ulex.chars (Cset.singleton (Char.code s.[n]))) (aux (succ n))
  in aux 0;















