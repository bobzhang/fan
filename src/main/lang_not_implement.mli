(** [undef] DDSl *)


(**
fan0 -printer o -str 'let dump_ident = %undef{}'
let dump_ident =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "From_string278147"
         "dump_ident")
 *) 
