(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)

module Make (Structure : Structure.S) = struct
  module Dump  = Print.MakeDump Structure;
  module Print = Print.Make Structure;
  module Tools = Tools.Make Structure;
  open Format;
  open Structure;
  open Tools;

  type t 'a = internal_entry;

  let name e = e.ename;

  let print ppf e = fprintf ppf "%a@\n" Print.entry e;
  let dump ppf e = fprintf ppf "%a@\n" Dump.entry e;

  (* let find e s = Find.entry e s; *)
  let trace_parser = ref False;
  let mk g n =
    { egram = g;
      ename = n;
      estart = empty_entry n;
      econtinue _ _ _ = parser [];
      edesc = Dlevels [] };

  let action_parse entry ts : Action.t =
    try 
      let p =
        if !trace_parser then
          Format.fprintf
        else Format.ifprintf in 
      let () = p Format.err_formatter "@[<4>%s@ " entry.ename in
      let res = entry.estart 0 ts in
      let () =  p Format.err_formatter "@]@." in 
      res 
    with
    [ Stream.Failure ->
        FanLoc.raise (get_prev_loc ts)
          (Stream.Error ("illegal begin of " ^ entry.ename))
    | FanLoc.Exc_located _ _ as exc -> raise exc
    | exc -> FanLoc.raise (get_prev_loc ts) exc ];

  let lex entry loc cs = entry.egram.glexer loc cs;

  let lex_string entry loc str = lex entry loc (Stream.of_string str);

  let filter entry ts =
    keep_prev_loc (Token.Filter.filter (get_filter entry.egram) ts);

  let parse_origin_tokens entry ts = Action.get (action_parse entry ts);

  let filter_and_parse_tokens entry ts = parse_origin_tokens entry (filter entry ts);

  let parse entry loc cs = filter_and_parse_tokens entry (lex entry loc cs);

  let parse_string entry loc str =
    filter_and_parse_tokens entry (lex_string entry loc str);

  let of_parser g n (p : Stream.t (Token.t * token_info) -> 'a) : t 'a =
    let f ts = Action.mk (p ts) in
    { egram = g;
      ename = n;
      estart _ = f;
      econtinue _ _ _ = parser [];
      edesc = Dparser f };

  let setup_parser e (p : Stream.t (Token.t * token_info) -> 'a) =
    let f ts = Action.mk (p ts) in do {
      e.estart <- fun _ -> f;
      e.econtinue <- fun _ _ _ -> parser [];
      e.edesc <- Dparser f
    };

  let clear e =
    do {
      e.estart <- fun _ -> parser [];
      e.econtinue <- fun _ _ _ -> parser [];
      e.edesc <- Dlevels []
    };

  let obj x = x;

end;
