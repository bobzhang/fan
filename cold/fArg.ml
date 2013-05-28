type key = string 

type doc = string 

type usage_msg = string 

type anon_fun = string -> unit 

type spec =  
  | Unit of (unit -> unit)
  | Bool of (bool -> unit)
  | Set of bool ref
  | Clear of bool ref
  | String of (string -> unit)
  | Set_string of string ref
  | Int of (int -> unit)
  | Set_int of int ref
  | Float of (float -> unit)
  | Set_float of float ref
  | Tuple of spec list
  | Symbol of string list* (string -> unit)
  | Rest of (string -> unit) 

exception Bad of string

exception Help of string

type error =  
  | Unknown of string
  | Wrong of string* string* string
  | Missing of string
  | Message of string 

exception Stop of error

open Printf

let rec assoc3 x l =
  match l with
  | [] -> raise Not_found
  | (y1,y2,_)::_ when y1 = x -> y2
  | _::t -> assoc3 x t

let make_symlist prefix sep suffix l =
  match l with
  | [] -> "<none>"
  | h::t ->
      (List.fold_left (fun x  y  -> x ^ (sep ^ y)) (prefix ^ h) t) ^ suffix

let print_spec buf (key,spec,doc) =
  if (String.length doc) > 0
  then
    match spec with
    | Symbol (l,_) ->
        bprintf buf "  %s %s%s\n" key (make_symlist "{" "|" "}" l) doc
    | _ -> bprintf buf "  %s %s\n" key doc
  else ()

let help_action () = raise (Stop (Unknown "-help"))

let add_help speclist =
  let add1 =
    try begin ignore (assoc3 "-help" speclist); [] end
    with
    | Not_found  ->
        [("-help", (Unit help_action), " Display this list of options")]
  and add2 =
    try begin ignore (assoc3 "--help" speclist); [] end
    with
    | Not_found  ->
        [("--help", (Unit help_action), " Display this list of options")] in
  speclist @ (add1 @ add2)

let usage_b buf speclist errmsg =
  begin
    bprintf buf "%s\n" errmsg; List.iter (print_spec buf) (add_help speclist)
  end

let usage_string speclist errmsg =
  let b = Buffer.create 200 in
  begin usage_b b speclist errmsg; Buffer.contents b end

let usage speclist errmsg = eprintf "%s" (usage_string speclist errmsg)

let current = ref 0

let parse_argv ?(current= current)  argv speclist anonfun errmsg =
  let l = Array.length argv in
  let b = Buffer.create 200 in
  let initpos = current.contents in
  let stop error =
    let progname = if initpos < l then argv.(initpos) else "(?)" in
    begin
      (match error with
       | Unknown "-help" -> ()
       | Unknown "--help" -> ()
       | Unknown s -> bprintf b "%s: unknown option `%s'.\n" progname s
       | Missing s ->
           bprintf b "%s: option `%s' needs an argument.\n" progname s
       | Wrong (opt,arg,expected) ->
           bprintf b "%s: wrong argument `%s'; option `%s' expects %s.\n"
             progname arg opt expected
       | Message s -> bprintf b "%s: %s.\n" progname s);
      usage_b b speclist.contents errmsg;
      if (error = (Unknown "-help")) || (error = (Unknown "--help"))
      then raise (Help (Buffer.contents b))
      else raise (Bad (Buffer.contents b))
    end in
  begin
    incr current;
    while current.contents < l do
      (let s = argv.(current.contents) in
       if ((String.length s) >= 1) && ((s.[0]) = '-')
       then
         let action =
           try assoc3 s speclist.contents
           with | Not_found  -> stop (Unknown s) in
         begin
           (try
              let rec treat_action =
                function
                | Unit f -> f ()
                | Bool f when (current.contents + 1) < l ->
                    let arg = argv.(current.contents + 1) in
                    begin
                      (try f (bool_of_string arg)
                       with
                       | Invalid_argument "bool_of_string" ->
                           raise (Stop (Wrong (s, arg, "a boolean"))));
                      incr current
                    end
                | Set r -> r := true
                | Clear r -> r := false
                | String f when (current.contents + 1) < l ->
                    begin f (argv.(current.contents + 1)); incr current end
                | Symbol (symb,f) when (current.contents + 1) < l ->
                    let arg = argv.(current.contents + 1) in
                    if List.mem arg symb
                    then
                      begin f (argv.(current.contents + 1)); incr current end
                    else
                      raise
                        (Stop
                           (Wrong
                              (s, arg,
                                ("one of: " ^ (make_symlist "" " " "" symb)))))
                | Set_string r when (current.contents + 1) < l ->
                    begin
                      r := (argv.(current.contents + 1)); incr current
                    end
                | Int f when (current.contents + 1) < l ->
                    let arg = argv.(current.contents + 1) in
                    begin
                      (try f (int_of_string arg)
                       with
                       | Failure "int_of_string" ->
                           raise (Stop (Wrong (s, arg, "an integer"))));
                      incr current
                    end
                | Set_int r when (current.contents + 1) < l ->
                    let arg = argv.(current.contents + 1) in
                    begin
                      (try r := (int_of_string arg)
                       with
                       | Failure "int_of_string" ->
                           raise (Stop (Wrong (s, arg, "an integer"))));
                      incr current
                    end
                | Float f when (current.contents + 1) < l ->
                    let arg = argv.(current.contents + 1) in
                    begin
                      (try f (float_of_string arg)
                       with
                       | Failure "float_of_string" ->
                           raise (Stop (Wrong (s, arg, "a float"))));
                      incr current
                    end
                | Set_float r when (current.contents + 1) < l ->
                    let arg = argv.(current.contents + 1) in
                    begin
                      (try r := (float_of_string arg)
                       with
                       | Failure "float_of_string" ->
                           raise (Stop (Wrong (s, arg, "a float"))));
                      incr current
                    end
                | Tuple specs -> List.iter treat_action specs
                | Rest f ->
                    while current.contents < (l - 1) do
                      begin f (argv.(current.contents + 1)); incr current end
                      done
                | _ -> raise (Stop (Missing s)) in
              treat_action action
            with | Bad m -> stop (Message m) | Stop e -> stop e);
           incr current
         end
       else
         begin
           (try anonfun s with | Bad m -> stop (Message m)); incr current
         end)
      done
  end

let parse l f msg =
  try parse_argv Sys.argv l f msg
  with | Bad msg -> begin eprintf "%s" msg; exit 2 end
  | Help msg -> begin printf "%s" msg; exit 0 end

let second_word s =
  let len = String.length s in
  let rec loop n =
    if n >= len then len else if (s.[n]) = ' ' then loop (n + 1) else n in
  try loop (String.index s ' ') with | Not_found  -> len

let max_arg_len cur (kwd,spec,doc) =
  match spec with
  | Symbol _ -> max cur (String.length kwd)
  | _ -> max cur ((String.length kwd) + (second_word doc))

let add_padding len ksd =
  match ksd with
  | (_,_,"") -> ksd
  | (kwd,(Symbol (_,_) as spec),msg) ->
      let cutcol = second_word msg in
      let spaces = String.make ((len - cutcol) + 3) ' ' in
      (kwd, spec, ("\n" ^ (spaces ^ msg)))
  | (kwd,spec,msg) ->
      let cutcol = second_word msg in
      let spaces = String.make ((len - (String.length kwd)) - cutcol) ' ' in
      let prefix = String.sub msg 0 cutcol in
      let suffix = String.sub msg cutcol ((String.length msg) - cutcol) in
      (kwd, spec, (prefix ^ (spaces ^ suffix)))

let align speclist =
  let completed = add_help speclist in
  let len = List.fold_left max_arg_len 0 completed in
  List.map (add_padding len) completed