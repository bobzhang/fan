let _loc = FanLoc.ghost;
let f  = Format.std_formatter;
module P = PreCast.Make (struct end) ;
open P;
open LibUtil;  
let open FanParsers in  begin
   pa_r (module P);
   pa_rp (module P);
   pa_q (module P);
   pa_g (module P);
   pa_l (module P);
   pa_m (module P);
end;

P.iter_and_take_callbacks (fun (_, f) -> f ()); 
Gram.dump f Syntax.sem_expr_for_list;  
Gram.parse_string
  Syntax.sem_expr_for_list
  FanLoc.string_loc
  "3;4;5" <:expr< 1 >>  |> FanBasic.p_expr f ;

(* sem_expr_for_list: [   LA suffix: *)
(*                              prefix: *)
(*                                `-expr---";"---SELF] *)
