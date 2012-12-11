(* open Format; *)
open Fan.Syntax;
open Lib;
open LibUtil;
(* module Ast = Camlp4Ast;   *)

let debug_mode =
  try
    let str = Sys.getenv "STATIC_CAMLP4_DEBUG" in
    let rec loop acc i =
      try
        let pos = String.index_from str i ':' in
        loop (SSet.add (String.sub str i (pos - i)) acc) (pos + 1)
      with
      [ Not_found ->
          SSet.add (String.sub str i (String.length str - i)) acc ] in
    let sections = loop SSet.empty 0 in
    if SSet.mem "*" sections then fun _ -> true
    else fun x -> SSet.mem x sections
  with [ Not_found -> fun _ -> false ];

let mk_debug_mode _loc =
  fun
    [ None -> {:expr| Debug.mode |}
    | Some m -> {:expr| $uid:m.Debug.mode |} ];

let mk_debug _loc m fmt section args =
  let call = Expr.apply {:expr| Debug.printf $str:section $str:fmt |} args in
    {:expr| if $(mk_debug_mode _loc m) $str:section then $call else () |};

let apply () =   
{:extend| Gram local: start_debug end_or_in ;  
  expr "expr":
   [ start_debug{m}; `LID section; `STR (_, fmt);
     L0 expr Level "."{args}; end_or_in{x} ->
    match (x, debug_mode section) with
    [ (None,   false) -> {| () |}
    | (Some e, false) -> e
    | (None, _) -> mk_debug _loc m fmt section args
    | (Some e, _) -> {| let () = $(mk_debug _loc m fmt section args) in $e |} ]  ] 
  end_or_in:
  [ "end" -> None
  | "in"; expr{e} -> Some e  ] 
  start_debug:
  [ `LID "debug" -> None
  | `LID "camlp4_debug" -> Some "Camlp4"  ]  |};

AstParsers.register_parser ("debug",apply);  
  



















