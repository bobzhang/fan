open StdFan

type anti_cxt = 
  {
  cxt: string;
  sep: string option;
  mutable decorations: string;
  content: string} 

let pp_print_anti_cxt: Format.formatter -> anti_cxt -> unit =
  fun fmt  { cxt = _a0; sep = _a1; decorations = _a2; content = _a3 }  ->
    Format.fprintf fmt
      "@[<hv 1>{cxt:%a;@,sep:%a;@,decorations:%a;@,content:%a}@]"
      pp_print_string _a0 (pp_print_option pp_print_string) _a1
      pp_print_string _a2 pp_print_string _a3

let mk_anti ?(c= "")  ?sep  loc n s =
  let c = { cxt = c; decorations = n; content = s; sep } in `Ant (loc, c)

let add_context s c = { s with decorations = (s.decorations ^ c) }

let destruct_poly s =
  let n = String.length s in
  if n = 0
  then invalid_arg "destruct_poly length=0"
  else if (s.[0]) = '`' then Some (String.sub s 1 (n - 1)) else None