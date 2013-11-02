


    
let setup_op_parser entry p =
  Gramf.setup_parser entry
    (%parser{| (`Key x | `Sym x) when p x.txt  ->
      let _loc = x.loc in %exp{ $lid{x.txt} }})


let symbolchars =
  ['$'; '!'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; ':'; '<'; '='; '>'; '?';
   '@'; '^'; '|'; '~'; '\\']
    
let symbolchar s i =
  let len = String.length s in
  try
    (for j = i to len - 1 do
      if not (List.mem s.[j] symbolchars) then
        raise Not_found
    done; true)
  with  Not_found -> false

(* [eoi_entry] could be improved   *)
let eoi_entry entry =
  let open! Gstru in
  let g = Gramf.gram_of_entry entry in
  let entry_eoi = (Gramf.mk_dynamic g (Gramf.name entry ^ "_eoi")) in
  begin
    %extend{ entry_eoi: [  entry as x; EOI %{x} ] } ;
    entry_eoi
  end



(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/gramlib.cmo" *)
(* end: *)
