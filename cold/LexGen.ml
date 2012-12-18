module Ast = Camlp4Ast
let _loc = FanLoc.ghost
open Ulex
let tables = Hashtbl.create 31
let tables_counter = ref 0
let get_tables () =
  let t = Hashtbl.fold (fun key  x  accu  -> (x, key) :: accu) tables [] in
  Hashtbl.clear tables; t
let table_name t =
  try Hashtbl.find tables t
  with
  | Not_found  ->
      (incr tables_counter;
       (let n = Printf.sprintf "__ulex_table_%i" tables_counter.contents in
        Hashtbl.add tables t n; n))
let output_byte buf b =
  Buffer.add_char buf '\\';
  Buffer.add_char buf (Char.chr (48 + (b / 100)));
  Buffer.add_char buf (Char.chr (48 + ((b / 10) mod 10)));
  Buffer.add_char buf (Char.chr (48 + (b mod 10)))
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
let partition_name i = Printf.sprintf "__ulex_partition_%i" i
let partition (i,p) =
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
                              (_loc, (Ast.IdLid (_loc, (table_name t))))), c)))))),
            (Ast.ExInt (_loc, "1"))) in
  let body = gen_tree (simplify (-1) Cset.max_code (decision_table p)) in
  let f = partition_name i in
  Ast.StVal
    (_loc, Ast.ReNil,
      (Ast.BiEq
         (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, f)))),
           (Ast.ExFun
              (_loc,
                (Ast.McArr
                   (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "c")))),
                     (Ast.ExNil _loc), body)))))))
let best_final final =
  let fin = ref None in
  Array.iteri
    (fun i  b  -> if b && (fin.contents = None) then fin := (Some i) else ())
    final;
  fin.contents
let call_state auto state =
  match auto.(state) with
  | (_,trans,final) ->
      if (Array.length trans) = 0
      then
        (match best_final final with
         | Some i -> Ast.ExInt (_loc, (string_of_int i))
         | None  -> assert false)
      else
        (let f = Printf.sprintf "__ulex_state_%i" state in
         Ast.ExApp
           (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, f)))),
             (Ast.ExId (_loc, (Ast.IdLid (_loc, "lexbuf"))))))
let gen_state auto _loc i (part,trans,final) =
  let f = Printf.sprintf "__ulex_state_%i" i in
  let p = partition_name part in
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
                          (_loc, (Ast.IdUid (_loc, "Ulexing")),
                            (Ast.IdLid (_loc, "next")))))),
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
                               (_loc, (Ast.IdUid (_loc, "Ulexing")),
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
                                      (_loc, (Ast.IdUid (_loc, "Ulexing")),
                                        (Ast.IdLid (_loc, "mark")))))),
                              (Ast.ExId (_loc, (Ast.IdLid (_loc, "lexbuf")))))),
                         (Ast.ExInt (_loc, (string_of_int i))))), body))))
let gen_definition _loc l =
  let brs = Array.of_list l in
  let rs = Array.map fst brs in
  let auto = Ulex.compile rs in
  let cases =
    Array.mapi
      (fun i  (_,e)  ->
         Ast.McArr
           (_loc, (Ast.PaInt (_loc, (string_of_int i))), (Ast.ExNil _loc), e))
      brs in
  let states = Array.mapi (gen_state auto _loc) auto in
  Ast.ExFun
    (_loc,
      (Ast.McArr
         (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "lexbuf")))),
           (Ast.ExNil _loc),
           (Ast.ExLet
              (_loc, Ast.ReRecursive,
                (Ast.biAnd_of_list (Array.to_list states)),
                (Ast.ExSeq
                   (_loc,
                     (Ast.ExSem
                        (_loc,
                          (Ast.ExApp
                             (_loc,
                               (Ast.ExId
                                  (_loc,
                                    (Ast.IdAcc
                                       (_loc, (Ast.IdUid (_loc, "Ulexing")),
                                         (Ast.IdLid (_loc, "start")))))),
                               (Ast.ExId (_loc, (Ast.IdLid (_loc, "lexbuf")))))),
                          (Ast.ExMat
                             (_loc,
                               (Ast.ExApp
                                  (_loc,
                                    (Ast.ExId
                                       (_loc,
                                         (Ast.IdLid (_loc, "__ulex_state_0")))),
                                    (Ast.ExId
                                       (_loc, (Ast.IdLid (_loc, "lexbuf")))))),
                               (Ast.McOr
                                  (_loc,
                                    (Ast.mcOr_of_list (Array.to_list cases)),
                                    (Ast.McArr
                                       (_loc, (Ast.PaAny _loc),
                                         (Ast.ExNil _loc),
                                         (Ast.ExApp
                                            (_loc,
                                              (Ast.ExId
                                                 (_loc,
                                                   (Ast.IdLid (_loc, "raise")))),
                                              (Ast.ExId
                                                 (_loc,
                                                   (Ast.IdAcc
                                                      (_loc,
                                                        (Ast.IdUid
                                                           (_loc, "Ulexing")),
                                                        (Ast.IdUid
                                                           (_loc, "Error")))))))))))))))))))))))
let char_int s =
  let i = int_of_string s in
  if (i >= 0) && (i <= Cset.max_code)
  then i
  else failwith ("Invalid Unicode code point: " ^ s)
let regexp_for_string s =
  let rec aux n =
    if n = (String.length s)
    then Ulex.eps
    else
      Ulex.seq (Ulex.chars (Cset.singleton (Char.code (s.[n]))))
        (aux (succ n)) in
  aux 0