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
module Make (Token : FanSig.Camlp4Token) = struct
  type t = (Stream.t (string * FanLoc.t) * Queue.t (string * FanLoc.t));
  let mk () =
    let q = Queue.create () in
    let f _ =
      debug comments "take...@\n" in
      try Some (Queue.take q) with [ Queue.Empty -> None ]
    in (Stream.from f, q);

  let filter (_, q) =
    let rec self =
      parser
      [ [< (FanSig.COMMENT x, loc); 'xs >] ->
            do { Queue.add (x, loc) q;
                 debug comments "add: %S at %a@\n" x FanLoc.dump loc in
                 self xs }
      | [< x; 'xs >] ->
          (* debug comments "Found %a at %a@." Token.print x FanLoc.dump loc in *)
          [< x; 'self xs >]
      | [< >] -> [< >] ]
    in self;

  let take_list (_, q) =
    let rec self accu =
      if Queue.is_empty q then accu else self [Queue.take q :: accu]
    in self [];

  let take_stream = fst;

  let define token_fiter comments_strm =
    debug comments "Define a comment filter@\n" in
    Token.Filter.define_filter token_fiter
      (fun previous strm -> previous (filter comments_strm strm));

end;
