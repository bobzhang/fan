
let g = Gram.create_gram ~annot:"arith"
    ~keywords:["-";"+";"*";"/";"**";"(";")"] ();
{:create|(g:Gram.t) expr_eoi expr|};
{:extend| 
  expr:
  { [S{x};"-";S{y} -> x -. y
    | S{x};"+";S{y} -> x +. y]
    [S{x};"*";S{y} -> x *. y
    |S{x};"/";S{y} -> x /. y]  
     RA
    [S{x};"**";S{y} -> x ** y]
    ["("; S{x}; ")" -> x
    | `INT(x,_) -> float_of_int x ] }
  expr_eoi:  [expr{x};`EOI -> x ]  
|};

(* print_float (Gram.parse_string expr_eoi   "3 + 3 * 2/ 1 ** 3" ) ; *)

(* the left [SELF] of the two [minus] and [power] correspond to
   a call to the next level.
   In the level [minus], the right [SELF] also do a call to the
   next level, this is due to the fact LA makes the [continue]
   function is called. On the other hand, for the level [power],
   the right [SELF] corresponds a call to the current level.

   At end, the [SELF] in [simple] do a call to the first level,
   [namely] minus in this grammar
 *)
(* let expr_eoi = Gram.mk "expr_eoi"; *)
(* let expr = Gram.mk "expr"; *)

(* let g = Gram. *)
