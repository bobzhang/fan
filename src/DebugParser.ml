module IdDebugParser = struct
  let name = "Camlp4DebugParser";
  let version = Sys.ocaml_version;
end;

open FanSig;
  
module MakeDebugParser (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  module StringSet = Set.Make String;

  let debug_mode =
    try
      let str = Sys.getenv "STATIC_CAMLP4_DEBUG" in
      let rec loop acc i =
        try
          let pos = String.index_from str i ':' in
          loop (StringSet.add (String.sub str i (pos - i)) acc) (pos + 1)
        with
        [ Not_found ->
            StringSet.add (String.sub str i (String.length str - i)) acc ] in
      let sections = loop StringSet.empty 0 in
      if StringSet.mem "*" sections then fun _ -> True
      else fun x -> StringSet.mem x sections
    with [ Not_found -> fun _ -> False ];

  let rec apply accu =
    fun
    [ [] -> accu
    | [x :: xs] ->
        let _loc = Ast.loc_of_expr x
        in apply <:expr< $accu $x >> xs ];

  let mk_debug_mode _loc = fun [ None -> <:expr< Debug.mode >>
                                 | Some m -> <:expr< $uid:m.Debug.mode >> ];

  let mk_debug _loc m fmt section args =
    let call = apply <:expr< Debug.printf $str:section $str:fmt >> args in
      <:expr< if $(mk_debug_mode _loc m) $str:section then $call else () >>;


  EXTEND Gram
    GLOBAL: expr;
    expr:
    [ [ m = start_debug; section = LIDENT; fmt = STRING;
        args = LIST0 expr Level "."; x = end_or_in ->
      match (x, debug_mode section) with
      [ (None,   False) -> <:expr< () >>
      | (Some e, False) -> e
      | (None, _) -> mk_debug _loc m fmt section args
      | (Some e, _) -> <:expr< let () = $(mk_debug _loc m fmt section args) in $e >> ]
    ] ];
    end_or_in:
    [ [ "end" -> None
      | "in"; e = expr -> Some e
    ] ];
    start_debug:
    [ [ LIDENT "debug" -> None
      | LIDENT "camlp4_debug" -> Some "Camlp4"
    ] ];
  END;

end;


(*   EXTEND Gram GLOBAL: expr; *)
(*     expr: *)
(*     [ [  `(XY  ("a"|"b"))  ]]; *)
(*   END *)
  
(* let _ = let _ = (expr : 'expr Gram.Entry.t) in *)
(*         (Gram.extend ( (expr : 'expr Gram.Entry.t) ) ( *)
(*           ((fun () *)
(*               -> *)
(*              (None , ( *)
(*               [(None , None , ( *)
(*                 [(( *)
(*                   [( *)
(*                    (Gram.Stoken *)
(*                      (( function | XY ("a" | "b") -> (true) | _ -> (false) ), *)
(*                       "XY (\"a\" | \"b\")")) )] ), ( *)
(*                   (Gram.Action.mk ( *)
(*                     fun (__camlp4_0 : *)
(*                       Gram.Token.t) -> *)
(*                      fun (_loc : *)
(*                        Gram.FanLoc.t) -> *)
(*                       (match __camlp4_0 with *)
(*                        | XY ("a" | "b") -> (() : 'expr) *)
(*                        | _ -> assert false) )) ))] ))] ))) () ) )) *)
