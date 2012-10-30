
(* open LibUtil; *)

prerr_endline "started..";

prerr_endline
  (BatPervasives.dump
     (fun s -> (LibUtil.Stream.junk s;  LibUtil.Stream.peek s))
     (Gram.filter [< (`INT (3,"3"),FanLoc.ghost) >])) ;


prerr_endline "finished";
