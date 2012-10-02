open Format;
open Camlp4.PreCast;
value _loc = Loc.ghost ;
value debug = ref False;
value conversion_table : Hashtbl.t string string = Hashtbl.create 50;
value bug_main_address = "hongboz@seas.upenn.edu";


















