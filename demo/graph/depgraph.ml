(* Usage ocamldep -modules *.ml | depgraph.native > x.dot*)
open Format;
open Graph;

module V = struct
  type t = string;
  let compare = Pervasives.compare;
  let hash = Hashtbl.hash;
  let equal = (=);
end;
module StringDigraph = Imperative.Digraph.Concrete V;

module Display = struct 
  include StringDigraph;
  open StringDigraph;
  let vertex_name v = (V.label v);
  let graph_attributes _ = [];
  let default_vertex_attributes _ = [];
  let vertex_attributes _ = [];
  let default_edge_attributes _ = [];
  let edge_attributes _ = [];
  let get_subgraph _ = None;
end ;
open StringDigraph;
module D = Graphviz.Dot(Display);
open LibUtil;
  
let dot_output g  =
   let () =  D.fprint_graph std_formatter g in
   pp_print_flush std_formatter ();

let g = Gram.create_gram ()   ;
{:extend.create|(g:Gram.t) path_line path_line_eoi|};    

{:extend|(g:Gram.t)
  path_line_eoi:
  [ path_line{x} ; `EOI -> x ]
  path_line:
  [ `LID name ;"."; `LID ext; ":";L0 [`UID x -> x]{modules} ->
    (name,ext,modules)] 
|};  


(* let lowercase s = string_map Char.lowercase s *)
let filter =
  Array.filter_map
    (fun x -> if Filename.check_suffix x ".ml"
    then Some (String.lowercase (Filename.chop_suffix x ".ml"))
    else None )
    (Sys.readdir ".") |>SSet.of_array;


let _ =
  let g = create () in 
  try
    while true do
      let line = input_line stdin in 
      let (name,_ext,deps) = Gram.parse_string path_line_eoi  line in
      List.iter (fun dep ->
        if (SSet.mem (String.lowercase name) filter)
          && (SSet.mem (String.lowercase dep) filter)
        then add_edge g (name^"_") (String.lowercase dep ^ "_")) deps 
    done
  with End_of_file -> begin
    prerr_endline "writing to dump.dot"; 
    dot_output g ;
    prerr_endline "finished";
  end;
