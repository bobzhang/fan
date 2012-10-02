
open Longident;
let li s = Lident s;
let li_loc s loc = Ploc.add (Lident s) loc;


let dot l s = Ldot l s;
let apply l s = Lapply l s;
