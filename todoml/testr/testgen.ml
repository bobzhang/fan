
let _loc = FanLoc.ghost ;
let u = Lib.Ctyp.of_str_item {:str_item| type u = [A of int | B of string ]|};

Gen.gen_print [`Single ("u",u)];

(* {:ctyp| $tup:x|} *)
    

















