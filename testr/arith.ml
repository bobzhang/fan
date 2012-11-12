
let expr_eoi = Gram.mk "expr";

{|Gram LOCAL:expr;
  expr:
  {"minus" LA
    [SELF{x};"-";SELF{y} -> x -. y
    |SELF{x};"+";SELF{y} -> x +. y]
  |"times" LA
    [SELF{x};"*";SELF{y} -> x *. y
    |SELF{x};"/";SELF{y} -> x /. y]  
  | "power" RA
    [SELF{x};"**";SELF{y} -> x ** y]
  | "simple"
    ["("; SELF{x}; ")" -> x
    | `INT(x,_) -> float_of_int x ] }
  expr_eoi:  [expr{x};`EOI -> x ]  
|};

print_float (Gram.parse_string expr_eoi FanLoc.string_loc  "3+3*2/1**3" ) ;

(* the left [SELF] of the two [minus] and [power] correspond to
   a call to the next level.
   In the level [minus], the right [SELF] also do a call to the
   next level, this is due to the fact LA makes the [continue]
   function is called. On the other hand, for the level [power],
   the right [SELF] corresponds a call to the current level.

   At end, the [SELF] in [simple] do a call to the first level,
   [namely] minus in this grammar
 *)
