

module P = PreCast.Make (struct end) ;
let open FanParsers in  begin
   pa_r (module P);
   pa_rp (module P);
   pa_q (module P);
   pa_g (module P);
   pa_l (module P);
   pa_m (module P);
end;
P.iter_and_take_callbacks (fun (_, f) -> f ()); 

open P;

Gram.dump Format.std_formatter Syntax.module_expr;


Gram.parse_string Syntax.module_longident FanLoc.string_loc
  "PreCast.Make";

Gram.parse_string  Syntax.module_expr FanLoc.string_loc
  "PreCast.Make (struct end)";
