type spec_list = (string * Arg.spec * string) list

open Format

let rec action_arg =
                                                                  fun s ->
                                                                   fun sl ->
                                                                    function
                                                                    | Arg.Unit
                                                                    (f) ->
                                                                    if 
                                                                    (s = "")
                                                                    then
                                                                     begin
                                                                    (
                                                                    (f () )
                                                                    );
                                                                    (
                                                                    Some (sl))
                                                                    end 
                                                                    else
                                                                    (None)
                                                                    | Arg.Bool
                                                                    (f) ->
                                                                    if 
                                                                    (s = "") then
                                                                    (
                                                                    (match
                                                                    sl with
                                                                    | (s ::
                                                                    sl) ->
                                                                    (
                                                                    try
                                                                    (
                                                                    (f (
                                                                    (bool_of_string
                                                                    s) ))
                                                                    );
                                                                    (
                                                                    Some (sl))
                                                                    with
                                                                    Invalid_argument
                                                                    ("bool_of_string") ->
                                                                    (None))
                                                                    | 
                                                                    [] ->
                                                                    (None))
                                                                    )
                                                                    else
                                                                    (try
                                                                    (
                                                                    (f (
                                                                    (bool_of_string
                                                                    s) ))
                                                                    );
                                                                    (Some
                                                                    (sl))
                                                                    with
                                                                    Invalid_argument
                                                                    ("bool_of_string") ->
                                                                    (None))
                                                                    | Arg.Set
                                                                    (r) ->
                                                                    if 
                                                                    (s = "")
                                                                    then
                                                                     begin
                                                                    (
                                                                    (r :=
                                                                    true )
                                                                    );
                                                                    (
                                                                    Some (sl))
                                                                    end 
                                                                    else
                                                                    (None)
                                                                    | Arg.Clear
                                                                    (r) ->
                                                                    if 
                                                                    (s = "")
                                                                    then
                                                                     begin
                                                                    (
                                                                    (r :=
                                                                    false )
                                                                    );
                                                                    (
                                                                    Some (sl))
                                                                    end 
                                                                    else
                                                                    (None)
                                                                    | Arg.Rest
                                                                    (f) ->
                                                                    (
                                                                    (List.iter
                                                                    f (
                                                                    ( s ) ::
                                                                    sl  ))
                                                                    );
                                                                    (
                                                                    Some
                                                                    ((
                                                                    [])))
                                                                    | Arg.String
                                                                    (f) ->
                                                                    if 
                                                                    (s = "") then
                                                                    (
                                                                    (match
                                                                    sl with
                                                                    | (s ::
                                                                    sl) ->
                                                                    (
                                                                    (f s)
                                                                    );
                                                                    (
                                                                    Some (sl))
                                                                    | 
                                                                    [] ->
                                                                    (None))
                                                                    )
                                                                    else begin
                                                                    (
                                                                    (f s)
                                                                    );
                                                                    (
                                                                    Some (sl))
                                                                    end
                                                                    | Arg.Set_string
                                                                    (r) ->
                                                                    if 
                                                                    (s = "") then
                                                                    (
                                                                    (match
                                                                    sl with
                                                                    | (s ::
                                                                    sl) ->
                                                                    (
                                                                    (r := s)
                                                                    );
                                                                    (
                                                                    Some (sl))
                                                                    | 
                                                                    [] ->
                                                                    (None))
                                                                    )
                                                                    else begin
                                                                    (
                                                                    (r := s)
                                                                    );
                                                                    (
                                                                    Some (sl))
                                                                    end
                                                                    | Arg.Int
                                                                    (f) ->
                                                                    if 
                                                                    (s = "") then
                                                                    (
                                                                    (match
                                                                    sl with
                                                                    | (s ::
                                                                    sl) ->
                                                                    (
                                                                    try
                                                                    (
                                                                    (f (
                                                                    (int_of_string
                                                                    s) ))
                                                                    );
                                                                    (
                                                                    Some (sl))
                                                                    with
                                                                    Failure
                                                                    ("int_of_string") ->
                                                                    (None))
                                                                    | 
                                                                    [] ->
                                                                    (None))
                                                                    )
                                                                    else
                                                                    (try
                                                                    (
                                                                    (f (
                                                                    (int_of_string
                                                                    s) ))
                                                                    );
                                                                    (Some
                                                                    (sl))
                                                                    with
                                                                    Failure
                                                                    ("int_of_string") ->
                                                                    (None))
                                                                    | Arg.Set_int
                                                                    (r) ->
                                                                    if 
                                                                    (s = "") then
                                                                    (
                                                                    (match
                                                                    sl with
                                                                    | (s ::
                                                                    sl) ->
                                                                    (
                                                                    try
                                                                    (
                                                                    (r := (
                                                                    (int_of_string
                                                                    s) ))
                                                                    );
                                                                    (
                                                                    Some (sl))
                                                                    with
                                                                    Failure
                                                                    ("int_of_string") ->
                                                                    (None))
                                                                    | 
                                                                    [] ->
                                                                    (None))
                                                                    )
                                                                    else
                                                                    (try
                                                                    (
                                                                    (r := (
                                                                    (int_of_string
                                                                    s) ))
                                                                    );
                                                                    (Some
                                                                    (sl))
                                                                    with
                                                                    Failure
                                                                    ("int_of_string") ->
                                                                    (None))
                                                                    | Arg.Float
                                                                    (f) ->
                                                                    if 
                                                                    (s = "") then
                                                                    (
                                                                    (match
                                                                    sl with
                                                                    | (s ::
                                                                    sl) ->
                                                                    (
                                                                    (f (
                                                                    (float_of_string
                                                                    s) ))
                                                                    );
                                                                    (
                                                                    Some (sl))
                                                                    | 
                                                                    [] ->
                                                                    (None))
                                                                    )
                                                                    else begin
                                                                    (
                                                                    (f (
                                                                    (float_of_string
                                                                    s) ))
                                                                    );
                                                                    (
                                                                    Some (sl))
                                                                    end
                                                                    | Arg.Set_float
                                                                    (r) ->
                                                                    if 
                                                                    (s = "") then
                                                                    (
                                                                    (match
                                                                    sl with
                                                                    | (s ::
                                                                    sl) ->
                                                                    (
                                                                    (r := (
                                                                    (float_of_string
                                                                    s) ))
                                                                    );
                                                                    (
                                                                    Some (sl))
                                                                    | 
                                                                    [] ->
                                                                    (None))
                                                                    )
                                                                    else begin
                                                                    (
                                                                    (r := (
                                                                    (float_of_string
                                                                    s) ))
                                                                    );
                                                                    (
                                                                    Some (sl))
                                                                    end
                                                                    | Arg.Tuple
                                                                    (specs) ->
                                                                    let rec action_args =
                                                                    fun s ->
                                                                    fun sl ->
                                                                    function
                                                                    | [] ->
                                                                    (
                                                                    Some (sl))
                                                                    | 
                                                                    (spec ::
                                                                    spec_list) ->
                                                                    (
                                                                    match
                                                                    (action_arg
                                                                    s sl
                                                                    spec) with
                                                                    | None ->
                                                                    (action_args
                                                                    "" [] 
                                                                    spec_list)
                                                                    | Some
                                                                    ((s ::
                                                                    sl)) ->
                                                                    (action_args
                                                                    s sl
                                                                    spec_list)
                                                                    | 
                                                                    Some (sl) ->
                                                                    (action_args
                                                                    "" sl
                                                                    spec_list)) in
                                                                    (action_args
                                                                    s sl
                                                                    specs)
                                                                    | 
                                                                    Arg.Symbol
                                                                    (syms, f) ->
                                                                    (
                                                                    match
                                                                    if 
                                                                    (s = "") then
                                                                    sl
                                                                    else
                                                                    ( s ) ::
                                                                    sl  with
                                                                    | (s ::
                                                                    sl) when
                                                                    (List.mem
                                                                    s syms) ->
                                                                    (
                                                                    (f s)
                                                                    );
                                                                    (
                                                                    Some (sl))
                                                                    | 
                                                                    _ ->
                                                                    (None))


let common_start =
 fun s1 ->
  fun s2 ->
   let rec loop =
    fun i ->
     if (( (i == ( (String.length s1) )) ) || ( (i == ( (String.length s2) ))
          )) then
      i
     else if (( (String.get s1 i) ) == ( (String.get s2 i) )) then
           (
           (loop ( (i + 1) ))
           )
     else i in
   (loop 0)

let parse_arg =
              fun fold ->
               fun s ->
                fun sl ->
                 (fold (
                   fun (name, action, _) ->
                    fun acu ->
                     let i = (common_start s name) in
                     if (i == ( (String.length name) )) then
                      (
                      (try
                        (action_arg (
                          (String.sub s i ( (( (String.length s) ) - i) )) )
                          sl action)
                       with
                       Arg.Bad (_) -> acu)
                      )
                     else acu ) None )

let rec parse_aux =
                                         fun fold ->
                                          fun anon_fun ->
                                           function
                                           | [] -> ([])
                                           | (s :: sl) ->
                                              if ((
                                                   (( (String.length s) ) >
                                                     1) ) && (
                                                   (( (String.get s 0) ) =
                                                     '-') )) then
                                               (
                                               (match
                                                  (parse_arg fold s sl) with
                                                | Some (sl) ->
                                                   (parse_aux fold anon_fun
                                                     sl)
                                                | None ->
                                                   ( s ) ::
                                                    (parse_aux fold anon_fun
                                                      sl) )
                                               )
                                              else begin
                                               (
                                               ((anon_fun s) : unit)
                                               );
                                               (parse_aux fold anon_fun sl)
                                              end

let align_doc =
                                                    fun key ->
                                                     fun s ->
                                                      let s =
                                                       let rec loop =
                                                        fun i ->
                                                         if (i = (
                                                              (String.length
                                                                s) )) then
                                                          ""
                                                         else if ((
                                                                   (String.get
                                                                    s i) ) =
                                                                   ' ') then
                                                               (
                                                               (loop (
                                                                 (i + 1) ))
                                                               )
                                                         else
                                                          (String.sub s i (
                                                            ((
                                                              (String.length
                                                                s) ) - i) )) in
                                                       (loop 0) in
                                                      let (p, s) =
                                                       if ((
                                                            (String.length s)
                                                            ) > 0) then
                                                        (
                                                        if ((
                                                             (String.get s 0)
                                                             ) = '<') then
                                                         (
                                                         let rec loop =
                                                          fun i ->
                                                           if (i = (
                                                                (String.length
                                                                  s) )) then
                                                            ("", s)
                                                           else if ((
                                                                    (String.get
                                                                    s i) ) <>
                                                                    '>') then
                                                                 (
                                                                 (loop (
                                                                   (i + 1) ))
                                                                 )
                                                           else
                                                            let p =
                                                             (String.sub s 0
                                                               ( (i + 1) )) in
                                                            let rec loop =
                                                             fun i ->
                                                              if (i >= (
                                                                   (String.length
                                                                    s) )) then
                                                               (p, "")
                                                              else if 
                                                                    ((
                                                                    (String.get
                                                                    s i) ) =
                                                                    ' ') then
                                                                    (
                                                                    (loop (
                                                                    (i + 1)
                                                                    ))
                                                                    )
                                                              else
                                                               (p, (
                                                                (String.sub s
                                                                  i (
                                                                  ((
                                                                    (String.length
                                                                    s) ) - i)
                                                                  )) )) in
                                                            (loop ( (i + 1)
                                                              )) in
                                                         (loop 0)
                                                         )
                                                        else ("", s)
                                                        )
                                                       else ("", "") in
                                                      let tab =
                                                       (String.make (
                                                         (max 1 (
                                                           ((
                                                             (16 - (
                                                               (String.length
                                                                 key) )) ) -
                                                             (
                                                             (String.length
                                                               p) )) )) )
                                                         ' ') in
                                                      (p ^ ( (tab ^ s) ))


let make_symlist =
 fun l ->
  (match l with
   | [] -> "<none>"
   | (h :: t) ->
      ((
        (List.fold_left ( fun x -> fun y -> (x ^ ( ("|" ^ y) )) ) ( ("{" ^ h)
          ) t) ) ^ "}"))

let print_usage_list =
                           fun l ->
                            (List.iter (
                              fun (key, spec, doc) ->
                               (match spec with
                                | Arg.Symbol (symbs, _) ->
                                   let s = (make_symlist symbs) in
                                   let synt = (key ^ ( (" " ^ s) )) in
                                   (eprintf "  %s %s\n" synt (
                                     (align_doc synt doc) ))
                                | _ ->
                                   (eprintf "  %s %s\n" key (
                                     (align_doc key doc) ))) ) l)

let remaining_args =
                                                                    fun argv ->
                                                                    let rec loop =
                                                                    fun l ->
                                                                    fun i ->
                                                                    if 
                                                                    (i == (
                                                                    (Array.length
                                                                    argv) )) then
                                                                    l
                                                                    else
                                                                    (loop (
                                                                    ( (
                                                                    argv.(i)
                                                                    ) ) :: l 
                                                                    ) (
                                                                    (i + 1)
                                                                    )) in
                                                                    (List.rev
                                                                    (
                                                                    (loop [] 
                                                                    (
                                                                    ((
                                                                    !Arg.current
                                                                    ) + 1) ))
                                                                    ))


let init_spec_list = (ref [] )

let ext_spec_list = (ref [] )

let init =
                                                                fun spec_list ->
                                                                 (init_spec_list
                                                                   :=
                                                                   spec_list)


let add =
 fun name ->
  fun spec ->
   fun descr ->
    (ext_spec_list := ( ( (name, spec, descr) ) :: !ext_spec_list  ))


let fold =
 fun f ->
  fun init ->
   let spec_list = (( !init_spec_list ) @ ( !ext_spec_list )) in
   let specs =
    (Sort.list ( fun (k1, _, _) -> fun (k2, _, _) -> (k1 >= k2) ) spec_list) in
   (List.fold_right f specs init)

let parse =
                                    fun anon_fun ->
                                     fun argv ->
                                      let remaining_args =
                                       (remaining_args argv) in
                                      (parse_aux fold anon_fun
                                        remaining_args)

let ext_spec_list =
                                                          fun ()
                                                            ->
                                                           !ext_spec_list
