(* NFA *)

open AstLoc;
open LibUtil;
type node = { 
    id : int; 
    mutable eps :  list node; (* FIXME inconsistent with original syntax *)
    mutable trans : list (LexSet.t * node);
};

(* Compilation regexp -> NFA *)

type regexp = node -> node;

let cur_id = ref 0;
let new_node () = begin 
  incr cur_id;
  { id = !cur_id; eps = []; trans = [] }
end;

let seq r1 r2 succ = r1 (r2 succ);

let alt r1 r2 succ =
  let n = new_node () in begin 
    n.eps <- [r1 succ; r2 succ];
    n
  end;

let rep r succ =
  let n = new_node () in begin 
    n.eps <- [r n; succ];
    n
  end;

(* return [nr] instead *)
let plus r succ = begin 
  let n = new_node () ;
  let nr = r n ;
  n.eps <- [nr; succ];
  nr
end;
  
let eps succ = succ;

let chars c succ = begin 
  let n = new_node () ;
  n.trans <- [(c,succ)];
  n
end;

let compile_re re = begin 
  let final = new_node () ;
  (re final, final)
end;
  
let of_string s =
  let rec aux n =
    if n = String.length s then eps
    else 
      seq (chars (LexSet.singleton (Char.code s.[n]))) (aux (succ n)) in
  aux 0;
    
(* Determinization *)

type state = list node;

let rec add_node state node = 
  if List.memq node state then state else add_nodes [node::state] node.eps
and add_nodes state nodes =
  List.fold_left add_node state nodes;


let transition state =
  (* Merge transition with the same target *)
  let t =
    List.(sort (fun (_,n1) (_,n2) -> n1.id - n2.id)
            (concat_map (fun n -> n.trans) state)) |> LexSet.norm in
  let (_,t) = List.fold_left LexSet.split (LexSet.empty,[]) t in
  (* Epsilon closure of targets *)
  let t = List.map (fun (c,ns) -> (c,add_nodes [] ns)) t in
  (* Canonical ordering *)
  let t = Array.of_list t in begin 
    Array.sort (fun (c1,_) (c2,_) -> compare c1 c2) t;
    (Array.map fst t, Array.map snd t)
  end;

(* It will change the counter *)    
let find_alloc tbl (counter: ref int) x : int =
  try Hashtbl.find tbl x
  with [Not_found ->      begin
    let i = !counter ;
    let _ = incr counter ;
    let _ = Hashtbl.add tbl x i ;
    i
  end];
 
(* let part_tbl = Hashtbl.create 31 *)
let part_id = ref 0;
    
let get_part ~part_tbl (t : array LexSet.t ) = find_alloc part_tbl part_id t;

(*
  {[
  Ulex.compile & t regexps {| {'a' | 'b' }|};
  - : (int * int array * bool array) array =
  [|(0, [|1|], [|false|]); (1, [||], [|true|])|]

  Ulex.compile & t regexps {| {'a' | 'b' ; "ab"}|};
  - : (int * int array * bool array) array =
  [|(2, [|1; 3|], [|false; false|]); (3, [|2|], [|true; false|]);
    (1, [||], [|false; true|]); (1, [||], [|true; false|])|]

  ]}
 *)
let compile ~part_tbl (rs: array regexp) =
  let rs = Array.map compile_re rs in
  let counter = ref 0 in
  let states = Hashtbl.create 31 in
  let states_def = ref [] in
  let rec aux state =
    try Hashtbl.find states state
    with [Not_found ->
      let i = !counter in begin 
        incr counter;
        Hashtbl.add states state i;
        let (part,targets) = transition state ;
        let part = get_part ~part_tbl part ;
        let targets = Array.map aux targets ;
        let finals = Array.map (fun (_,f) -> List.mem f state) rs ;
        states_def := [(i, (part,targets,finals)) :: !states_def];
        i
      end] in
  let init = ref [] in begin 
    Array.iter (fun (i,_) -> init := add_node !init i) rs;
    ignore(aux !init);
    Array.init !counter (fun id -> List.assoc id !states_def)
  end;

(* fetch the data from [part_tbl] *)    
let partitions ~part_tbl () =
  let aux part =
    let seg = ref [] in begin 
    Array.iteri
      (fun i c -> 
	 List.iter (fun (a,b) -> seg := [(a,b,i) :: !seg]) c)
      part;
      List.sort (fun (a1,_,_) (a2,_,_) -> compare a1 a2) !seg
    end in
  let res = ref [] in begin
    Hashtbl.iter (fun part i -> res := [(i, aux part) :: !res]) part_tbl;
    Hashtbl.clear part_tbl;
    !res
  end;



(* Named regexp *)

let named_regexps =
  (Hashtbl.create 13 :  Hashtbl.t string regexp );

let () =
  List.iter (fun (n,c) -> Hashtbl.add named_regexps n (chars c))
    [
      ("eof", LexSet.eof);
      ("xml_letter", LexSet.letter);
      ("xml_digit", LexSet.digit);
      ("xml_extender", LexSet.extender);
      ("xml_base_char", LexSet.base_char);
      ("xml_ideographic", LexSet.ideographic);
      ("xml_combining_char", LexSet.combining_char);
      ("xml_blank", LexSet.blank);

      ("tr8876_ident_char", LexSet.tr8876_ident_char);
    ]; 

let table_prefix = "__table_"  ;
let state_prefix = "__state_";
let partition_prefix = "__partition_";

(* FIXME ghost location introduced *)
let lexer_module_name =
  let _loc = FanLoc.ghost in ref {:ident|$(uid:"Ulexing")|};
  
let gm () = !lexer_module_name; 
let mk_table_name i =
  Printf.sprintf "%s%i" table_prefix i;
let mk_state_name i =
  Printf.sprintf "__state_%i" i;
let mk_partition_name i =
  Printf.sprintf "%s%i" partition_prefix i ;
  
  
(* Decision tree for partitions *)

type decision_tree =
  [ Lte of int *  decision_tree * decision_tree
  | Table of int *  array int
  | Return of int];

let decision l =
  let l = List.map (fun (a,b,i) -> (a,b,Return i)) l in
  let rec merge2 = function
    [ [(a1,b1,d1) ; (a2,b2,d2) :: rest] ->
	let x =
	  if b1 + 1 = a2 then d2
	  else Lte (a2 - 1,Return (-1), d2) in
	[(a1,b2, Lte (b1,d1, x)) :: (merge2 rest)]
    | rest -> rest ] in
  let rec aux = function
    [ [ _;_::_ ] as l -> aux (merge2 l)
    | [(a,b,d)] -> Lte (a - 1, Return (-1), Lte (b, d, Return (-1)))
    | _ -> Return (-1)] in
  aux l;

let limit = 8192;

let decision_table l =
  let rec aux m accu = function
    [ [ ((a,b,i) as x)::rem] when (b < limit && i < 255)-> 
	aux (min a m) [x::accu] rem
    | rem -> (m,accu,rem) ]  in
  match aux max_int [] l  with
  [ (_,[], _) -> decision l
  | (min,([(_,max,_)::_] as l1), l2) ->
      let arr = Array.create (max-min+1) 0 in begin 
        List.iter (fun (a,b,i) -> for j = a to b do arr.(j-min) <- i + 1 done) l1;
        Lte (min-1, Return (-1), Lte (max, Table (min,arr), decision l2))
      end ];

let rec simplify min max = function
  [ Lte (i,yes,no) ->
      if i >= max then
        simplify min max yes 
      else
        if i < min then
          simplify min max no
        else
          Lte (i, simplify min i yes, simplify (i+1) max no)
  | x -> x];
		   
    
(*  the generated code depends on [next] [backtrack] [start] [Error] *)


let _loc = FanLoc.ghost;

  

let get_tables ~tables () = begin 
  let t = Hashtbl.fold (fun key x accu -> [(x,key)::accu]) tables [] ;
  Hashtbl.clear tables;
  t
end;


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
  {:exp| $str:s |}
end;

let table (n,t) = {:stru| let $lid:n = $(output_byte_array t) |};
let binding_table (n,t) = {:binding|  $lid:n = $(output_byte_array t) |};



let partition ~counter ~tables (i,p) =
  let rec gen_tree = function 
    [ Lte (i,yes,no) ->
	{:exp| if (c <= $`int:i) 
	then $(gen_tree yes) else $(gen_tree no) |}
    | Return i ->
	{:exp| $`int:i |}
    | Table (offset, t) ->
	let c = if offset = 0 then {:exp| c |} 
	else {:exp| (c - $`int:offset) |} in
	{:exp| Char.code ($(lid: table_name ~tables ~counter t).[$c]) - 1|} ] in
  let body = gen_tree (simplify LexSet.min_code LexSet.max_code (decision_table p)) in
  let f = mk_partition_name i in
  {:stru| let $lid:f = fun c -> $body |};

let binding_partition ~counter ~tables (i,p) = 
  let rec gen_tree = function 
    [ Lte (i,yes,no) ->
	{:exp| if (c <= $`int:i) 
	then $(gen_tree yes) else $(gen_tree no) |}
    | Return i ->
	{:exp| $`int:i |}
    | Table (offset, t) ->
	let c = if offset = 0 then {:exp| c |} 
	else {:exp| (c - $`int:offset) |} in
	{:exp| Char.code ($(lid: table_name ~tables ~counter t).[$c]) - 1|} ] in
  let body = gen_tree
      (simplify LexSet.min_code LexSet.max_code (decision_table p)) in
  let f = mk_partition_name i in
  {:binding|  $lid:f = fun c -> $body |};

(* Code generation for the automata *)

let best_final final =
  let fin = ref None in begin 
  Array.iteri 
    (fun i b -> if b && (!fin = None) then fin := Some i else ()) final;
  !fin
  end;


(* FIXME,
   1. it seems some *states* are missing __state_1, __state_2
   will be skimmed, 2. how to remove [rec] warning,
   3. provide a `rec antiquot 
 *)  
let gen_definition _loc l =

  let call_state auto state = with exp
    let (_,trans,final) = auto.(state) in
    if Array.length trans = 0 then
      match best_final final with
      [ Some i -> {| $`int:i |}
      | None -> assert false]
    else
      let f = mk_state_name state in
      {| $lid:f lexbuf |} in

  (* generate states transition *)
  let gen_state auto _loc i (part,trans,final) : option binding  =
    let f = mk_state_name i in 
    let p = mk_partition_name part in
    let cases =
      Array.mapi 
        (fun i j -> {:case| $`int:i -> $(call_state auto j) |})
        trans in
    let cases = or_of_list
        (Array.to_list cases @
         [{:case| _ -> $(id:gm()).backtrack lexbuf|}]) in
    
    let body =
      {:exp|
      match ($lid:p ($(id:gm()).next lexbuf)) with
      [ $cases ]  
      (* [ $cases | _ -> $(id:gm()).backtrack lexbuf ] *)
      |} in
    let ret (body:exp) =
      {:binding| $lid:f = fun lexbuf -> $body |} in
    match best_final final with
    [ None -> Some (ret body)
    | Some i -> 
	if Array.length trans = 0 then (* {:binding||} *) None else
	Some
          (ret
	     {:exp| begin  $(id:gm()).mark lexbuf $`int:i;  $body end |}) ] in

  let part_tbl = Hashtbl.create 30 in
  let brs = Array.of_list l in
  let rs = Array.map fst brs in
  let auto = compile ~part_tbl  rs in
  
  let cases = Array.mapi (fun i (_,e) -> {:case| $`int:i -> $e |}) brs in
  let table_counter = ref 0 in 
  let tables = Hashtbl.create 31 in
  let states = Array.filter_mapi (gen_state  auto _loc) auto in
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
      (List.sort (fun (i0,_) (i1,_) -> compare i0 i1) (get_tables ~tables ())) in
  let (b,states) =
    let len = Array.length states in
    match len with
    [ 1 ->
      (`ReNil _loc,states.(0))
    | 0 -> failwithf "FanLexTools.states length = 0 "
    | _ -> (`Recursive _loc, and_of_list (Array.to_list states)) ] in
  let cases =
    or_of_list
      (Array.to_list cases @ [{:case| _ -> raise $(id:gm()).Error|}]) in
  let rest =
    binds tables
      (binds parts
       {:exp|
       let $rec:b $states in
       ( $(id:gm()).start lexbuf;
         match $(lid:mk_state_name 0) lexbuf with
         [ $cases ] )|}) in
  {:exp| fun lexbuf -> $rest |};


