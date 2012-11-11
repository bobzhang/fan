open LibUtil
let keywords_of_gram { Grammar.Structure.gkeywords = gkeywords;_} =
  pp Format.std_formatter "@[<hv0>%a@]"
    (fun f  tbl  ->
       Hashtbl.iter (fun s  i  -> pp f "@[<2>%s@;-->@;%i@]@;" s i.contents)
         tbl) gkeywords