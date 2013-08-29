

(** About Fan's antiquot context management *)  

type anti_cxt = {
    cxt:string;
    sep: string option ;
    mutable decorations:   string; (* keep it simple first*)
    content:string;
  }
(* TODO with ("Print") get rid of dependency on a module as much as possible  *)
  
let pp_print_anti_cxt fmt  {cxt;sep;content;_} =
  let open Format in
  fprintf fmt "cxt:%S;content:%S;%s"
    cxt
    content
    (match sep with
    |None -> ""
    |Some x -> "sep:"^x )
let dummy = {cxt="";sep=None;decorations="";content=""}

let mk_anti ?(c="") ?sep loc n s =
  let c = {
  cxt = c;
  decorations= n;
  content =s ;
  sep;
 } in `Ant(loc,c)
    
let add_context s c =
  {s with decorations = s.decorations ^ c}

(**
   {[
   destruct_poly "`a";
   Some "a"
   ]}
 *)
let destruct_poly s =
  let n = String.length s in
  if n = 0 then
    invalid_arg "destruct_poly length=0"
  else
    if s.[0] = '`' then
      Some (String.sub s 1 (n-1))
    else None
