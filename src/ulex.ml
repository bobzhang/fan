(* NFA *)

type node = { 
  id : int; 
  mutable eps : node list; 
  mutable trans : (Cset.t * node) list;
}

(* Compilation regexp -> NFA *)

type regexp = node -> node

let cur_id = ref 0
let new_node () =
  incr cur_id;
  { id = !cur_id; eps = []; trans = [] }

let seq r1 r2 succ = r1 (r2 succ)

let alt r1 r2 succ =
  let n = new_node () in
  n.eps <- [r1 succ; r2 succ];
  n

let rep r succ =
  let n = new_node () in
  n.eps <- [r n; succ];
  n

let plus r succ =
  let n = new_node () in
  let nr = r n in
  n.eps <- [nr; succ];
  nr

let eps succ = succ

let chars c succ =
  let n = new_node () in
  n.trans <- [c,succ];
  n

let compile_re re =
  let final = new_node () in
  (re final, final)

(* Determinization *)

type state = node list

let rec add_node state node = 
  if List.memq node state then state else add_nodes (node::state) node.eps
and add_nodes state nodes =
  List.fold_left add_node state nodes


let transition state =
  (* Merge transition with the same target *)
  let rec norm = function
    | (c1,n1)::((c2,n2)::q as l) ->
	if n1 == n2 then norm ((Cset.union c1 c2,n1)::q)
	else (c1,n1)::(norm l)
    | l -> l in
  let t = List.concat (List.map (fun n -> n.trans) state) in
  let t = norm (List.sort (fun (_c1,n1) (_c2,n2) -> n1.id - n2.id) t) in

  (* Split char sets so as to make them disjoint *)
  let  split (all,t) ((c0 : Cset.t),n0) = 
    let t = 
      [(Cset.difference c0 all, [n0])] @
      List.map (fun (c,ns) -> (Cset.intersection c c0, n0::ns)) t @
      List.map (fun (c,ns) -> (Cset.difference c c0, ns)) t in
    (Cset.union all c0,
    List.filter (fun (c,_ns) -> not (Cset.is_empty c)) t) in

  let (_,t) = List.fold_left split (Cset.empty,[]) t in

  (* Epsilon closure of targets *)
  let t = List.map (fun (c,ns) -> (c,add_nodes [] ns)) t in

  (* Canonical ordering *)
  let t = Array.of_list t in
  Array.sort (fun (c1,_ns1) (c2,_ns2) -> compare c1 c2) t;
  Array.map fst t, Array.map snd t

let find_alloc tbl counter x =
  try Hashtbl.find tbl x
  with Not_found ->
    let i = !counter in
    incr counter;
    Hashtbl.add tbl x i;
    i
 
let part_tbl = Hashtbl.create 31
let part_id = ref 0
let get_part (t : Cset.t array) = find_alloc part_tbl part_id t


let compile rs =
  let rs = Array.map compile_re rs in
  let counter = ref 0 in
  let states = Hashtbl.create 31 in
  let states_def = ref [] in
  let rec aux state =
    try Hashtbl.find states state
    with Not_found ->
      let i = !counter in
      incr counter;
      Hashtbl.add states state i;
      let (part,targets) = transition state in
      let part = get_part part in
      let targets = Array.map aux targets in
      let finals = Array.map (fun (_,f) -> List.mem f state) rs in
      states_def := (i, (part,targets,finals)) :: !states_def;
      i
  in
  let init = ref [] in
  Array.iter (fun (i,_) -> init := add_node !init i) rs;
  ignore (aux !init);
  Array.init !counter (fun id -> List.assoc id !states_def)
  
let partitions () =
  let aux part =
    let seg = ref [] in
    Array.iteri
      (fun i c -> 
	 List.iter (fun (a,b) -> seg := (a,b,i) :: !seg) c)
      part;
     List.sort (fun (a1,_,_) (a2,_,_) -> compare a1 a2) !seg in
  let res = ref [] in
  Hashtbl.iter (fun part i -> res := (i, aux part) :: !res) part_tbl;
  Hashtbl.clear part_tbl;
  !res



(* Named regexp *)

let named_regexps =
  (Hashtbl.create 13 : (string, regexp) Hashtbl.t)

let () =
  List.iter (fun (n,c) -> Hashtbl.add named_regexps n (chars c))
    [
      "eof", Cset.eof;
      "xml_letter", Cset.letter;
      "xml_digit", Cset.digit;
      "xml_extender", Cset.extender;
      "xml_base_char", Cset.base_char;
      "xml_ideographic", Cset.ideographic;
      "xml_combining_char", Cset.combining_char;
      "xml_blank", Cset.blank;

      "tr8876_ident_char", Cset.tr8876_ident_char;
    ] 

(* Decision tree for partitions *)

type decision_tree =
  | Lte of int * decision_tree * decision_tree
  | Table of int * int array
  | Return of int

let decision l =
  let l = List.map (fun (a,b,i) -> (a,b,Return i)) l in
  let rec merge2 = function
    | (a1,b1,d1) :: (a2,b2,d2) :: rest ->
	let x =
	  if b1 + 1 = a2 then d2
	  else Lte (a2 - 1,Return (-1), d2)
	in
	(a1,b2, Lte (b1,d1, x)) :: (merge2 rest)
    | rest -> rest in
  let rec aux = function
    | _::_::_ as l -> aux (merge2 l)
    | [(a,b,d)] -> Lte (a - 1, Return (-1), Lte (b, d, Return (-1)))
    | _ -> Return (-1)
  in
  aux l

let limit = 8192

let decision_table l =
  let rec aux m accu = function
    | ((a,b,i) as x)::rem when (b < limit && i < 255)-> 
	aux (min a m) (x::accu) rem
    | rem -> m,accu,rem in
  match (aux max_int [] l : int * 'a list * 'b list) with
    | _,[], _ -> decision l
    | min,((_,max,_)::_ as l1), l2 ->
	let arr = Array.create (max-min+1) 0 in
	List.iter (fun (a,b,i) -> for j = a to b do arr.(j-min) <- i + 1 done) l1;
	Lte (min-1, Return (-1), Lte (max, Table (min,arr), decision l2))

let rec simplify min max = function
  | Lte (i,yes,no) ->
      if i >= max then simplify min max yes 
      else if i < min then simplify min max no
      else Lte (i, simplify min i yes, simplify (i+1) max no)
  | x -> x
		   
    
