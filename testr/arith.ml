



let arith: Gram.t int = Gram.mk "arith" ;
let eoi_arith: Gram.t int = Gram.mk "eoi_arith";
  
EXTEND Gram 
  arith:
  [ "plus"
  [ SELF{x}; "+"; SELF{y} -> x + y
  | SELF{x}; "-"; SELF{y} -> x - y]    
  |"simple"
  [ `INT(x,_) -> x ]]
  eoi_arith:
  [ [arith{x};";"->x]]  
  END  ;

let _ = begin 
  print_int (Gram.parse eoi_arith FanLoc.ghost (Stream.of_channel stdin));
  prerr_endline "finished."
end;

(*
  {egram =
  {gfilter = {is_kwd = <fun>; filter = <fun>}; gkeywords = <abstr>;
   glexer = <fun>; warning_verbose = {contents = true};
   error_verbose = {contents = false}};
 ename = "eoi_arith"; estart = <fun>; econtinue = <fun>;
 edesc =
  Dlevels
   [{assoc = `LA; lname = None; lsuffix = DeadEnd;
     lprefix =
      Node
       {node =
         `Snterm
           {egram =
             {gfilter = {is_kwd = <fun>; filter = <fun>};
              gkeywords = <abstr>; glexer = <fun>;
              warning_verbose = {contents = true};
              error_verbose = {contents = false}};
            ename = "arith"; estart = <fun>; econtinue = <fun>;
            edesc =
             Dlevels
              [{assoc = `LA; lname = Some "plus";
                lsuffix =
                 Node
                  {node = `Skeyword "+";
                   son =
                    Node
                     {node = `Sself; son = LocAct (<abstr>, []);
                      brother = DeadEnd};
                   brother =
                    Node
                     {node = `Skeyword "-";
                      son =
                       Node
                        {node = `Sself; son = LocAct (<abstr>, []);
                         brother = DeadEnd};
                      brother = DeadEnd}};
                lprefix = DeadEnd};
               {assoc = `LA; lname = Some "simple"; lsuffix = DeadEnd;
                lprefix =
                 Node
                  {node = `Stoken (<fun>, (`Normal, "`INT (_,_)"));
                   son = LocAct (<abstr>, []); brother = DeadEnd}}]};
        son =
         Node
          {node = `Skeyword ";"; son = LocAct (<abstr>, []);
           brother = DeadEnd};
        brother = DeadEnd}}]}
 *)
















