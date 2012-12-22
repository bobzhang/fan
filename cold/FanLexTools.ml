open LibUtil
type node = 
  {
  id: int;
  mutable eps: node list;
  mutable trans: (LexSet.t* node) list} 
type regexp = node -> node 
let cur_id = ref 0
let new_node () =
  incr cur_id; { id = (cur_id.contents); eps = []; trans = [] }
let seq r1 r2 succ = r1 (r2 succ)
let alt r1 r2 succ = let n = new_node () in n.eps <- [r1 succ; r2 succ]; n
let rep r succ = let n = new_node () in n.eps <- [r n; succ]; n
let plus r succ =
  let n = new_node () in let nr = r n in n.eps <- [nr; succ]; nr
let eps succ = succ
let chars c succ = let n = new_node () in n.trans <- [(c, succ)]; n
let compile_re re = let final = new_node () in ((re final), final)
let of_string s =
  let rec aux n =
    if n = (String.length s)
    then eps
    else seq (chars (LexSet.singleton (Char.code (s.[n])))) (aux (succ n)) in
  aux 0
type state = node list 
let rec add_node state node =
  if List.memq node state then state else add_nodes (node :: state) node.eps
and add_nodes state nodes = List.fold_left add_node state nodes
let transition state =
  let t =
    (let open List in
       sort (fun (_,n1)  (_,n2)  -> n1.id - n2.id)
         (concat_map (fun n  -> n.trans) state))
      |> LexSet.norm in
  let (_,t) = List.fold_left LexSet.split (LexSet.empty, []) t in
  let t = List.map (fun (c,ns)  -> (c, (add_nodes [] ns))) t in
  let t = Array.of_list t in
  Array.sort (fun (c1,_)  (c2,_)  -> compare c1 c2) t;
  ((Array.map fst t), (Array.map snd t))
let find_alloc tbl (counter : int ref) x =
  (try Hashtbl.find tbl x
   with
   | Not_found  ->
       let i = counter.contents in
       let _ = incr counter in let _ = Hashtbl.add tbl x i in i : int )
let part_id = ref 0
let get_part ~part_tbl  (t : LexSet.t array) = find_alloc part_tbl part_id t
let compile ~part_tbl  (rs : regexp array) =
  let rs = Array.map compile_re rs in
  let counter = ref 0 in
  let states = Hashtbl.create 31 in
  let states_def = ref [] in
  let rec aux state =
    try Hashtbl.find states state
    with
    | Not_found  ->
        let i = counter.contents in
        (incr counter;
         Hashtbl.add states state i;
         (let (part,targets) = transition state in
          let part = get_part ~part_tbl part in
          let targets = Array.map aux targets in
          let finals = Array.map (fun (_,f)  -> List.mem f state) rs in
          states_def := ((i, (part, targets, finals)) ::
            (states_def.contents));
          i)) in
  let init = ref [] in
  Array.iter (fun (i,_)  -> init := (add_node init.contents i)) rs;
  ignore (aux init.contents);
  Array.init counter.contents (fun id  -> List.assoc id states_def.contents)
let partitions ~part_tbl  () =
  let aux part =
    let seg = ref [] in
    Array.iteri
      (fun i  c  ->
         List.iter (fun (a,b)  -> seg := ((a, b, i) :: (seg.contents))) c)
      part;
    List.sort (fun (a1,_,_)  (a2,_,_)  -> compare a1 a2) seg.contents in
  let res = ref [] in
  Hashtbl.iter (fun part  i  -> res := ((i, (aux part)) :: (res.contents)))
    part_tbl;
  Hashtbl.clear part_tbl;
  res.contents
let named_regexps: (string,regexp) Hashtbl.t = Hashtbl.create 13
let () =
  List.iter (fun (n,c)  -> Hashtbl.add named_regexps n (chars c))
    [("eof", LexSet.eof);
    ("xml_letter", LexSet.letter);
    ("xml_digit", LexSet.digit);
    ("xml_extender", LexSet.extender);
    ("xml_base_char", LexSet.base_char);
    ("xml_ideographic", LexSet.ideographic);
    ("xml_combining_char", LexSet.combining_char);
    ("xml_blank", LexSet.blank);
    ("tr8876_ident_char", LexSet.tr8876_ident_char)]
let table_prefix = "__table_"
let state_prefix = "__state_"
let partition_prefix = "__partition_"
let lexer_module_name =
  let _loc = FanLoc.ghost in ref (Ast.IdUid (_loc, "Ulexing"))
let gm () = lexer_module_name.contents
let mk_table_name i = Printf.sprintf "%s%i" table_prefix i
let mk_state_name i = Printf.sprintf "__state_%i" i
let mk_partition_name i = Printf.sprintf "%s%i" partition_prefix i
type decision_tree =  
  | Lte of int* decision_tree* decision_tree
  | Table of int* int array
  | Return of int 
let decision l =
  let l = List.map (fun (a,b,i)  -> (a, b, (Return i))) l in
  let rec merge2 =
    function
    | (a1,b1,d1)::(a2,b2,d2)::rest ->
        let x =
          if (b1 + 1) = a2 then d2 else Lte ((a2 - 1), (Return (-1)), d2) in
        (a1, b2, (Lte (b1, d1, x))) :: (merge2 rest)
    | rest -> rest in
  let rec aux =
    function
    | _::_::_ as l -> aux (merge2 l)
    | (a,b,d)::[] ->
        Lte ((a - 1), (Return (-1)), (Lte (b, d, (Return (-1)))))
    | _ -> Return (-1) in
  aux l
let limit = 8192
let decision_table l =
  let rec aux m accu =
    function
    | ((a,b,i) as x)::rem when (b < limit) && (i < 255) ->
        aux (min a m) (x :: accu) rem
    | rem -> (m, accu, rem) in
  match aux max_int [] l with
  | (_,[],_) -> decision l
  | (min,((_,max,_)::_ as l1),l2) ->
      let arr = Array.create ((max - min) + 1) 0 in
      (List.iter
         (fun (a,b,i)  -> for j = a to b do arr.(j - min) <- i + 1 done) l1;
       Lte
         ((min - 1), (Return (-1)),
           (Lte (max, (Table (min, arr)), (decision l2)))))
let rec simplify min max =
  function
  | Lte (i,yes,no) ->
      if i >= max
      then simplify min max yes
      else
        if i < min
        then simplify min max no
        else Lte (i, (simplify min i yes), (simplify (i + 1) max no))
  | x -> x
module Ast = Camlp4Ast
let _loc = FanLoc.ghost
let get_tables ~tables  () =
  let t = Hashtbl.fold (fun key  x  accu  -> (x, key) :: accu) tables [] in
  Hashtbl.clear tables; t
let table_name ~tables  ~counter  t =
  try mk_table_name (Hashtbl.find tables t)
  with
  | Not_found  ->
      (incr counter;
       Hashtbl.add tables t counter.contents;
       mk_table_name counter.contents)
let output_byte buf b =
  let open Buffer in
    ignore
      ((((buf +> '\\') +> (Char.chr (48 + (b / 100)))) +>
          (Char.chr (48 + ((b / 10) mod 10))))
         +> (Char.chr (48 + (b mod 10))))
let output_byte_array v =
  let b = Buffer.create ((Array.length v) * 5) in
  for i = 0 to (Array.length v) - 1 do
    (output_byte b ((v.(i)) land 255);
     if (i land 15) = 15 then Buffer.add_string b "\\\n    " else ())
  done;
  (let s = Buffer.contents b in Ast.ExStr (_loc, s))
let table (n,t) =
  Ast.StVal
    (_loc, Ast.ReNil,
      (Ast.BiEq
         (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, n)))),
           (output_byte_array t))))
let binding_table (n,t) =
  Ast.BiEq
    (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, n)))), (output_byte_array t))
let partition ~counter  ~tables  (i,p) =
  let rec gen_tree =
    function
    | Lte (i,yes,no) ->
        Ast.ExIfe
          (_loc,
            (Ast.ExApp
               (_loc,
                 (Ast.ExApp
                    (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "<=")))),
                      (Ast.ExId (_loc, (Ast.IdLid (_loc, "c")))))),
                 (Ast.ExInt (_loc, (string_of_int i))))), (gen_tree yes),
            (gen_tree no))
    | Return i -> Ast.ExInt (_loc, (string_of_int i))
    | Table (offset,t) ->
        let c =
          if offset = 0
          then Ast.ExId (_loc, (Ast.IdLid (_loc, "c")))
          else
            Ast.ExApp
              (_loc,
                (Ast.ExApp
                   (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "-")))),
                     (Ast.ExId (_loc, (Ast.IdLid (_loc, "c")))))),
                (Ast.ExInt (_loc, (string_of_int offset)))) in
        Ast.ExApp
          (_loc,
            (Ast.ExApp
               (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "-")))),
                 (Ast.ExApp
                    (_loc,
                      (Ast.ExId
                         (_loc,
                           (Ast.IdAcc
                              (_loc, (Ast.IdUid (_loc, "Char")),
                                (Ast.IdLid (_loc, "code")))))),
                      (Ast.ExSte
                         (_loc,
                           (Ast.ExId
                              (_loc,
                                (Ast.IdLid
                                   (_loc, (table_name ~tables ~counter t))))),
                           c)))))), (Ast.ExInt (_loc, "1"))) in
  let body =
    gen_tree (simplify LexSet.min_code LexSet.max_code (decision_table p)) in
  let f = mk_partition_name i in
  Ast.StVal
    (_loc, Ast.ReNil,
      (Ast.BiEq
         (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, f)))),
           (Ast.ExFun
              (_loc,
                (Ast.McArr
                   (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "c")))),
                     (Ast.ExNil _loc), body)))))))
let binding_partition ~counter  ~tables  (i,p) =
  let rec gen_tree =
    function
    | Lte (i,yes,no) ->
        Ast.ExIfe
          (_loc,
            (Ast.ExApp
               (_loc,
                 (Ast.ExApp
                    (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "<=")))),
                      (Ast.ExId (_loc, (Ast.IdLid (_loc, "c")))))),
                 (Ast.ExInt (_loc, (string_of_int i))))), (gen_tree yes),
            (gen_tree no))
    | Return i -> Ast.ExInt (_loc, (string_of_int i))
    | Table (offset,t) ->
        let c =
          if offset = 0
          then Ast.ExId (_loc, (Ast.IdLid (_loc, "c")))
          else
            Ast.ExApp
              (_loc,
                (Ast.ExApp
                   (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "-")))),
                     (Ast.ExId (_loc, (Ast.IdLid (_loc, "c")))))),
                (Ast.ExInt (_loc, (string_of_int offset)))) in
        Ast.ExApp
          (_loc,
            (Ast.ExApp
               (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "-")))),
                 (Ast.ExApp
                    (_loc,
                      (Ast.ExId
                         (_loc,
                           (Ast.IdAcc
                              (_loc, (Ast.IdUid (_loc, "Char")),
                                (Ast.IdLid (_loc, "code")))))),
                      (Ast.ExSte
                         (_loc,
                           (Ast.ExId
                              (_loc,
                                (Ast.IdLid
                                   (_loc, (table_name ~tables ~counter t))))),
                           c)))))), (Ast.ExInt (_loc, "1"))) in
  let body =
    gen_tree (simplify LexSet.min_code LexSet.max_code (decision_table p)) in
  let f = mk_partition_name i in
  Ast.BiEq
    (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, f)))),
      (Ast.ExFun
         (_loc,
           (Ast.McArr
              (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "c")))),
                (Ast.ExNil _loc), body)))))
let best_final final =
  let fin = ref None in
  Array.iteri
    (fun i  b  -> if b && (fin.contents = None) then fin := (Some i) else ())
    final;
  fin.contents
let gen_definition _loc l =
  let call_state auto state =
    let (_,trans,final) = auto.(state) in
    if (Array.length trans) = 0
    then
      match best_final final with
      | Some i -> Ast.ExInt (_loc, (string_of_int i))
      | None  -> assert false
    else
      (let f = mk_state_name state in
       Ast.ExApp
         (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, f)))),
           (Ast.ExId (_loc, (Ast.IdLid (_loc, "lexbuf")))))) in
  let gen_state auto _loc i (part,trans,final) =
    let f = mk_state_name i in
    let p = mk_partition_name part in
    let cases =
      Array.mapi
        (fun i  j  ->
           Ast.McArr
             (_loc, (Ast.PaInt (_loc, (string_of_int i))), (Ast.ExNil _loc),
               (call_state auto j))) trans in
    let cases = Array.to_list cases in
    let body =
      Ast.ExMat
        (_loc,
          (Ast.ExApp
             (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, p)))),
               (Ast.ExApp
                  (_loc,
                    (Ast.ExId
                       (_loc,
                         (Ast.IdAcc
                            (_loc, (gm ()), (Ast.IdLid (_loc, "next")))))),
                    (Ast.ExId (_loc, (Ast.IdLid (_loc, "lexbuf")))))))),
          (Ast.McOr
             (_loc, (Ast.mcOr_of_list cases),
               (Ast.McArr
                  (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc),
                    (Ast.ExApp
                       (_loc,
                         (Ast.ExId
                            (_loc,
                              (Ast.IdAcc
                                 (_loc, (gm ()),
                                   (Ast.IdLid (_loc, "backtrack")))))),
                         (Ast.ExId (_loc, (Ast.IdLid (_loc, "lexbuf"))))))))))) in
    let ret body =
      Ast.BiEq
        (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, f)))),
          (Ast.ExFun
             (_loc,
               (Ast.McArr
                  (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "lexbuf")))),
                    (Ast.ExNil _loc), body))))) in
    match best_final final with
    | None  -> ret body
    | Some i ->
        if (Array.length trans) = 0
        then Ast.BiNil _loc
        else
          ret
            (Ast.ExSeq
               (_loc,
                 (Ast.ExSem
                    (_loc,
                      (Ast.ExApp
                         (_loc,
                           (Ast.ExApp
                              (_loc,
                                (Ast.ExId
                                   (_loc,
                                     (Ast.IdAcc
                                        (_loc, (gm ()),
                                          (Ast.IdLid (_loc, "mark")))))),
                                (Ast.ExId
                                   (_loc, (Ast.IdLid (_loc, "lexbuf")))))),
                           (Ast.ExInt (_loc, (string_of_int i))))), body)))) in
  let part_tbl = Hashtbl.create 30 in
  let brs = Array.of_list l in
  let rs = Array.map fst brs in
  let auto = compile ~part_tbl rs in
  let cases =
    Array.mapi
      (fun i  (_,e)  ->
         Ast.McArr
           (_loc, (Ast.PaInt (_loc, (string_of_int i))), (Ast.ExNil _loc), e))
      brs in
  let table_counter = ref 0 in
  let tables = Hashtbl.create 31 in
  let states = Array.mapi (gen_state auto _loc) auto in
  let partitions =
    List.sort (fun (i0,_)  (i1,_)  -> compare i0 i1)
      (partitions ~part_tbl ()) in
  let parts =
    List.map (binding_partition ~counter:table_counter ~tables) partitions in
  let tables =
    List.map (fun (i,arr)  -> binding_table ((mk_table_name i), arr))
      (List.sort (fun (i0,_)  (i1,_)  -> compare i0 i1)
         (get_tables ~tables ())) in
  let b =
    let len = Array.length states in
    if len > 1 then Ast.ReRecursive else Ast.ReNil in
  Ast.ExFun
    (_loc,
      (Ast.McArr
         (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "lexbuf")))),
           (Ast.ExNil _loc),
           (Ast.ExLet
              (_loc, Ast.ReNil, (Ast.biAnd_of_list tables),
                (Ast.ExLet
                   (_loc, Ast.ReNil, (Ast.biAnd_of_list parts),
                     (Ast.ExLet
                        (_loc, b, (Ast.biAnd_of_list (Array.to_list states)),
                          (Ast.ExSeq
                             (_loc,
                               (Ast.ExSem
                                  (_loc,
                                    (Ast.ExApp
                                       (_loc,
                                         (Ast.ExId
                                            (_loc,
                                              (Ast.IdAcc
                                                 (_loc, (gm ()),
                                                   (Ast.IdLid (_loc, "start")))))),
                                         (Ast.ExId
                                            (_loc,
                                              (Ast.IdLid (_loc, "lexbuf")))))),
                                    (Ast.ExMat
                                       (_loc,
                                         (Ast.ExApp
                                            (_loc,
                                              (Ast.ExId
                                                 (_loc,
                                                   (Ast.IdLid
                                                      (_loc,
                                                        (mk_state_name 0))))),
                                              (Ast.ExId
                                                 (_loc,
                                                   (Ast.IdLid
                                                      (_loc, "lexbuf")))))),
                                         (Ast.McOr
                                            (_loc,
                                              (Ast.mcOr_of_list
                                                 (Array.to_list cases)),
                                              (Ast.McArr
                                                 (_loc, (Ast.PaAny _loc),
                                                   (Ast.ExNil _loc),
                                                   (Ast.ExApp
                                                      (_loc,
                                                        (Ast.ExId
                                                           (_loc,
                                                             (Ast.IdLid
                                                                (_loc,
                                                                  "raise")))),
                                                        (Ast.ExId
                                                           (_loc,
                                                             (Ast.IdAcc
                                                                (_loc,
                                                                  (gm ()),
                                                                  (Ast.IdUid
                                                                    (_loc,
                                                                    "Error")))))))))))))))))))))))))))