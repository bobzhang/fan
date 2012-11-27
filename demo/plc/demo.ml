

#load "demo/plc/plc.cmxs";;

{:plc|
witch(X) :- female(X), burns(X).
  
burns(X) :- wooden(X).
  
wooden(X) :- floats(X).
  
floats(X) :- sameweight(duck,X).
  
female(girl).
  
female(guinevere).
  
sameweight(duck,girl). (* by experiment *)
|};

let a = 3;

(*
type atom = Guinevere | Girl | Duck
val string_of_atom : atom -> string
val wooden_o : (atom -> unit) -> unit
val wooden_c : (unit -> unit) -> atom -> unit
val witch_o : (atom -> unit) -> unit
val witch_c : (unit -> unit) -> atom -> unit
val sameweight_oo : (atom -> atom -> 'a) -> 'a
val sameweight_co : (atom -> unit) -> atom -> unit
val sameweight_oc : (atom -> unit) -> atom -> unit
val sameweight_cc : (unit -> unit) -> atom -> atom -> unit
val floats_o : (atom -> unit) -> unit
val floats_c : (unit -> unit) -> atom -> unit
val female_o : (atom -> unit) -> unit
val female_c : (unit -> unit) -> atom -> unit
val burns_o : (atom -> unit) -> unit
val burns_c : (unit -> unit) -> atom -> unit
val a : int
*)


  
(*
  type atom =  
  | Guinevere
  | Girl
  | Duck 
let string_of_atom =
  function | Guinevere  -> "guinevere" | Girl  -> "girl" | Duck  -> "duck"
let rec wooden_o _f = floats_o (fun _arg0  -> _f _arg0)
and wooden_c _f _arg0 = floats_c (fun ()  -> _f ()) _arg0
and witch_o _f = female_o (fun _arg0  -> burns_c (fun ()  -> _f _arg0) _arg0)
and witch_c _f _arg0 =
  female_c (fun ()  -> burns_c (fun ()  -> _f ()) _arg0) _arg0
and sameweight_oo _f = _f Duck Girl
and sameweight_co _f _arg0 = match _arg0 with | Duck  -> _f Girl | _ -> ()
and sameweight_oc _f _arg0 = match _arg0 with | Girl  -> _f Duck | _ -> ()
and sameweight_cc _f _arg0 _arg1 =
  match (_arg0, _arg1) with | (Duck ,Girl ) -> _f () | _ -> ()
and floats_o _f = sameweight_co (fun _arg0  -> _f _arg0) Duck
and floats_c _f _arg0 = sameweight_cc (fun ()  -> _f ()) Duck _arg0
and female_o _f = _f Girl; _f Guinevere
and female_c _f _arg0 =
  (match _arg0 with | Girl  -> _f () | _ -> ());
  (match _arg0 with | Guinevere  -> _f () | _ -> ())
and burns_o _f = wooden_o (fun _arg0  -> _f _arg0)
and burns_c _f _arg0 = wooden_c (fun ()  -> _f ()) _arg0
let a = 3
 *)  

