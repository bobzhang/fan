


          ]
     (* t patt "`A ((\"x\"|\"y\" as n),s)"; *)
     (* t patt "`A $x"; *)
     (* {:delete|Gram expr:[a|b|c]|} *)
     (* {:delete|Gram expr:[`INT32(_,s) | `INT64(_,s)]|}; *)
   (* let _ = *)
   (*   Gram.delete_rule expr *)
   (*  [`Stoken *)
   (*     (((function | `INT32 (_,_)|`INT64 (_,_) -> true | _ -> false)), *)
   (*       (`Normal, "`INT32 (_,_)|`INT64 (_,_)"))] *)
   (* {:delete|Gram expr:[`INT32(_,s) (\* | `INT64(_,s) *\)]|} *)
     (*   Gram.delete_rule expr *)
    (* [`Stoken *)
    (*    (((function | `INT32 (_,_) -> true | _ -> false)), *)
    (*      (`Normal, "`INT32 (_,_)"))] *)
     (* let _ = *)
     (* Gram.delete_rule expr [`Snterm (Gram.obj (a : 'a Gram.t ))]; *)
     (* Gram.delete_rule expr [`Snterm (Gram.obj (b : 'b Gram.t ))]; *)
     (* Gram.delete_rule expr [`Snterm (Gram.obj (c : 'c Gram.t ))] *)
     (* {:delete|Gram expr:[a]|} *)
     (* let _ = Gram.delete_rule expr [`Snterm (Gram.obj (a : 'a Gram.t ))] *)




















