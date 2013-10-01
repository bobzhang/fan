

let with_open_out_file x f =
  match x with
  | Some file ->
      let oc = open_out_bin file in
      begin f oc; flush oc; close_out oc end
  | None ->
      (set_binary_mode_out stdout true; f stdout; flush stdout) 

(**
   The [.ml] file can be recognized as an ast directly, the format
   is
   {
   magic number;
   filename;
   ast
   }
   when [fname] is "-" it means the file is from an standard input or pipe.
   An empty name would marshallized.

   Use case cat - | fan -printer -impl -
   redirect the standard input to fan
 *)
let dump_pt magic fname pt oc =
  begin
    output_string oc magic;
    output_value oc (if fname="-" then "" else fname);
    output_value oc pt
  end



let simple_wrap init_loc cs pa  =
  let rec loop loc =
    let (pl, stopped_at_directive) = pa loc cs in
    match stopped_at_directive with
    | Some new_loc ->
        if pl = [] then  (loop (Location_util.join_end new_loc))
        else  pl @ (loop (Location_util.join_end new_loc))
    | None -> pl in
  loop init_loc

    
(* local variables: *)
(* compile-command: "cd .. && pmake common/fan_util.cmo" *)
(* end: *)
