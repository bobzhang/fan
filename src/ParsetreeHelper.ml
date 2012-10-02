open Parsetree;
open Longident;
open LibUtil;

let with_loc  txt loc = Location.mkloc txt  loc;
let lident s = Lident s;
let lident_with_loc s loc = with_loc (Lident s) loc;


let ldot l s = Ldot l s;
let lapply l s = Lapply l s;
let mkghloc loc = FanLoc.ghostify loc;



let error loc str = FanLoc.raise loc (Failure str);  

let mksig loc d = {psig_desc = d; psig_loc =  loc};
let mkmod loc d = {pmod_desc = d; pmod_loc =  loc};
let mkexp loc d = {pexp_desc = d; pexp_loc =  loc};
let mkstr loc d = {pstr_desc = d; pstr_loc =  loc};
let mkfield loc d = {pfield_desc = d; pfield_loc =  loc};
let mkcty loc d = {pcty_desc = d; pcty_loc =  loc};
let mkcl loc d = {pcl_desc = d; pcl_loc =  loc};
let mkcf loc d = { pcf_desc = d; pcf_loc =  loc; };
let mkctf loc d = { pctf_desc = d; pctf_loc =  loc; };
let mktyp loc d = {ptyp_desc = d; ptyp_loc =  loc};
let mkpat loc d = {ppat_desc = d; ppat_loc =  loc};
let mkghpat loc d = {ppat_desc = d; ppat_loc =mkghloc loc};
let mkmty loc d = {pmty_desc = d; pmty_loc =  loc};  

    
(* let mkghpat    *)
let mkpolytype t = match t.ptyp_desc with
    [ Ptyp_poly _ _ -> t
    | _ -> { (t) with ptyp_desc = Ptyp_poly [] t } ] ;

(* convert to unsafe
   {[
   array_function_no_loc "Array" "get";;
   - : Longident.t = Longident.Ldot (Longident.Lident "Array", "get")
   ]}
 *)

let array_function_no_loc str name =
    ldot (lident str) (if !FanConfig.unsafe then "unsafe_" ^ name else name) ;
  
let array_function loc str name = with_loc (array_function_no_loc str name) loc;


(*
  {[
  mkli FanLoc.ghost "a" ["b";"c";"d"];;
  - : Longident.t Location.loc =
  {Location.txt =
  Longident.Ldot
  (Longident.Ldot (Longident.Ldot (Longident.Lident "b", "c"), "d"), "a");
  loc = }
  ]}
  FIXME performance problem  
 *)
let mkli sloc s (list: list string) =
  let aux = fun
    [ [] -> lident s
    | [x] -> ldot (lident x) s 
    | [ x;y::z ] ->
        List.fold_left ldot (ldot (lident x ) y) (z@[s])
    ] in
  with_loc (aux list) sloc ;
