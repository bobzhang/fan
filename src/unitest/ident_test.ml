(*
  {[
     sep_dot_exp [] {|A.B.g.U.E.h.i|};
     - : (loc * string list * exp) list =
     [(, ["A"; "B"], ExId (, Lid (, "g")));
      (, ["U"; "E"], ExId (, Lid (, "h"))); (, [], ExId (, Lid (, "i")))]

       sep_dot_exp [] {|A.B.g.i|};
     - : (loc * string list * exp) list =
     [(, ["A"; "B"], ExId (, Lid (, "g"))); (, [], ExId (, Lid (, "i")))]

       sep_dot_exp [] {|$(uid:"").i|};
     - : (loc * string list * exp) list =
     [(, [""], ExId (, Lid (, "i")))]  ]}
  *)
