(*******************************)
(* Format generator            *) 
(*******************************)
(* [OPrint] unused *)  
open Astfn
open Astn_util

let extract info =
  info |> Listf.concat_map (fun (x:Ctyp.ty_info) -> [x.name_exp;(x.id_ep:>exp)] )


let mkfmt pre sep post fields =
  let s =  pre^ String.concat sep fields ^ post in
  %exp-{Format.fprintf fmt $str:s } 
  
let mk_variant cons params =
    let len = List.length params in
    let pre =
        match cons  with
        | Some cons when len >= 1  -> 
            mkfmt ("@[<1>("^cons^"@ ") "@ " ")@]" @@ Listf.init len (fun _ -> "%a")
        | Some cons ->
            mkfmt cons "" "" []
        | None -> mkfmt "@[<1>(" ",@," ")@]" @@ Listf.init len (fun _ -> "%a") in
    appl_of_list (pre :: extract params)
    
let mk_record cols = 
    let pre = cols
       |> List.map (fun (x:Ctyp.record_col) -> x.label^":%a" )
       |>  mkfmt "@[<hv 1>{" ";@," "}@]" in
    appl_of_list (pre :: 
                  (cols
                  |> List.map(fun  (x:Ctyp.record_col) -> x.info )
                  |> extract )) (* apply pre *)  


let default : Derive_stru.param =  {
    
    arity = 1;
    default  = None;
    id =  (`Pre "pp_print_");
    names = ["fmt"] ;
    mk_record = Some mk_record;
    annot = Some (fun s ->
      (%ctyp-{Format.formatter -> $lid:s -> unit}, %ctyp-{unit}));
    mk_variant = Some mk_variant;
    plugin_name = "Print";
    excludes = [];
    builtin_tbl = [
    (%ctyp-{int}, %exp-{Format.pp_print_int});
    (%ctyp-{int32}, %exp-{fun fmt -> Format.fprintf "%ld"});
    (%ctyp-{int64}, %exp-{fun fmt -> Format.fprintf "%Ld"});
    (%ctyp-{nativeint}, %exp-{fun fmt -> Format.fprintf "%nd"});
    (%ctyp-{float}, %exp-{Format.pp_print_float});
    (%ctyp-{string}, %exp-{fun fmt -> Format.fprintf fmt "%S"});
    (%ctyp-{bool}, %exp-{Format.pp_print_bool});
    (%ctyp-{char}, %exp-{Format.pp_print_char});
    (%ctyp-{unit}, %exp-{fun fmt (_:unit)-> Format.fprintf fmt "()"});
    (%ctyp-{list}, %exp-{fun mf_a fmt lst -> 
      Format.fprintf fmt "@[<1>[%a]@]"
        (fun fmt  -> List.iter (fun x  -> Format.fprintf fmt "%a@ " mf_a x)) lst});
    (%ctyp-{option},
     %exp-{fun mf_a fmt v -> 
       match v with
       | None  -> Format.fprintf fmt "None"
       | Some v -> Format.fprintf fmt "Some @[%a@]" mf_a v})
      (* %exp-@check{....}*)
  ];
  }


;;

(* open Astf *)
(* %builtin{ *)
(* print  *)
(* };; *)
