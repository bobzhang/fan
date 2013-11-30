

Based on the tree structure, in the module *Gparser* there are two  functions *start_parser_of_entry* and *continue_parser_of_entry*

```ocaml
val start_parser_of_entry :  entry ->  int -> Gaction.t Tokenf.parse 
val continue_parser_of_entry :  entry -> int -> Gaction.t cont_parse
```
Note that the parser building process is very fast, there is not too much work involved, for simplicity, currently,
everytime we call *extend_single* by *extend* DDSL, such building process is done immediately as follows:

```ocaml
let extend_single entry
    (lb  : Gdefs.single_extend_statement) =
  let olevel = scan_olevel entry lb in
  let elev = insert_olevel entry lb.label olevel in
  (entry.levels <-  elev;
   entry.start <-Gparser.start_parser_of_entry entry;
   entry.continue <- Gparser.continue_parser_of_entry entry)
```

The parsing behavior is driven by *action_parse* , which is essentially:

```ocaml
entry.start 0 stream
```

------

there is a trade-off here, everytime we do the insertion, and rebuild  the parser is a bit wasteful, if not working this way, the user has to finalize the parser -- which is a burdern to the user
