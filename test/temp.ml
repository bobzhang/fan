open Format

module P = Camlp4.MakePreCast.Make (FanLoc) (FanLexer.Make)
open P
open Token
open FanSig
let eoi_arith = Gram.Entry.mk "eoi_arith";;

(* let _ = (eoi_arith : 'eoi_arith Gram.Entry.t) in *)
  Gram.extend (eoi_arith : 'eoi_arith Gram.Entry.t)
    ((fun () ->
        (None,
         [ (None, None,
            [ ([ Gram.Stoken
                   (((function | INT ((_, _)) -> true | _ -> false),
                     "INT ((_, _))")) ],
               (Gram.Action.mk
                  (fun (__camlp4_0 : Gram.Token.t) (_loc : Gram.Loc.t) ->
                     match __camlp4_0 with
                     | INT ((x, _)) -> (x : 'eoi_arith)
                     | _ -> assert false))) ]) ]))
       ())

(* let _= *)
(*   let _ = (eoi_arith :'eoi_arith Gram.t  ) in *)
(*   Gram.extend (eoi_arith :'eoi_arith Gram.t  ) *)
(*     ((fun () -> (None, [(None, None, *)
(*         [([`Stoken (((function | `INT (_,_) -> true | _ -> false)), (`Normal, *)
(*              "`INT (_,_)"))], *)
(*         (Gram.mk_action *)
(*            (fun __camlp4_0 -> *)
(*               fun (_loc : FanLoc.t ) -> *)
(*                 match __camlp4_0 with *)
(*                 | `INT (x,_) -> (x :'eoi_arith ) *)
(*                 | _ -> assert false)))])])) ()) *)
let _ = begin 
  print_int (Gram.parse eoi_arith FanLoc.ghost (Stream.of_channel stdin));
  prerr_endline "finished."
end    



















