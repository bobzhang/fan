open Astf
open Ast_gen
let _loc = Locf.ghost
let output_memory_actions =
  function
  | (mvs : Lexgen.memory_action list) ->
      (List.map
         (function
          | (x : Lexgen.memory_action) ->
              (match x with
               | Copy (tgt,src) ->
                   (`Assign
                      (_loc,
                        (`ArrayDot
                           (_loc,
                             (`Field
                                (_loc, (`Lid (_loc, "lexbuf")),
                                  (`Lid (_loc, "lex_mem")))),
                             (`Int (_loc, (string_of_int tgt))))),
                        (`ArrayDot
                           (_loc,
                             (`Field
                                (_loc, (`Lid (_loc, "lexbuf")),
                                  (`Lid (_loc, "lex_mem")))),
                             (`Int (_loc, (string_of_int src)))))) :> 
                   Astf.exp)
               | Set tgt ->
                   (`Assign
                      (_loc,
                        (`ArrayDot
                           (_loc,
                             (`Field
                                (_loc, (`Lid (_loc, "lexbuf")),
                                  (`Lid (_loc, "lex_mem")))),
                             (`Int (_loc, (string_of_int tgt))))),
                        (`Field
                           (_loc, (`Lid (_loc, "lexbuf")),
                             (`Lid (_loc, "lex_curr_pos"))))) :> Astf.exp)))
         mvs : exp list)
let lex_state = function | i -> "__ocaml_lex_state" ^ (string_of_int i)
let output_moves =
  function
  | (moves : (Lexgen.automata_move* Lexgen.memory_action list) array) ->
      (let output_action =
         function
         | (mems : Lexgen.memory_action list) ->
             (function
              | (r : Lexgen.automata_move) ->
                  ((output_memory_actions mems) @
                     ((match r with
                       | Backtrack  ->
                           [(`Assign
                               (_loc,
                                 (`Field
                                    (_loc, (`Lid (_loc, "lexbuf")),
                                      (`Lid (_loc, "lex_curr_pos")))),
                                 (`Field
                                    (_loc, (`Lid (_loc, "lexbuf")),
                                      (`Lid (_loc, "lex_last_pos"))))) :> 
                           Astf.exp);
                           (`Field
                              (_loc, (`Lid (_loc, "lexbuf")),
                                (`Lid (_loc, "lex_last_action"))) :> 
                           Astf.exp)]
                       | Goto n ->
                           [(`App
                               (_loc, (`Lid (_loc, (lex_state n))),
                                 (`Unit _loc)) :> Astf.exp)])) : exp list)) in
       let output_clause ?pats  =
         function
         | (mems : Lexgen.memory_action list) ->
             (function
              | (r : Lexgen.automata_move) ->
                  let pat =
                    match pats with
                    | Some pats ->
                        bar_of_list
                          (List.map
                             (function
                              | x ->
                                  (`Int (_loc, (string_of_int x)) :> 
                                  Astf.pat)) pats)
                    | None  -> (`Any _loc :> Astf.pat) in
                  let action = seq_sem (output_action mems r) in
                  (`Case (_loc, pat, (action :> Astf.exp)) :> Astf.case)) in
       let (t
         :(Lexgen.automata_move,(Lexgen.memory_action list* int list))
            Hashtbl.t)=
         Hashtbl.create 17 in
       let add_move =
         function
         | i ->
             (function
              | (m,mems) ->
                  let (mems,r) =
                    try Hashtbl.find t m with | Not_found  -> (mems, []) in
                  Hashtbl.replace t m (mems, (i :: r))) in
       (for i = 0 to 256 do add_move i (moves.(i)) done;
        (let most_frequent = ref Lexgen.Backtrack
         and most_mems = ref []
         and size = ref 0 in
         Hashtbl.iter
           (function
            | m ->
                (function
                 | (mems,pats) ->
                     let size_m = List.length pats in
                     if size_m > (!size)
                     then
                       (most_frequent := m; most_mems := mems; size := size_m)))
           t;
         (Hashtbl.fold
            (function
             | m ->
                 (function
                  | (mems,pats) ->
                      (function
                       | acc ->
                           if m <> (!most_frequent)
                           then (output_clause ~pats:(List.rev pats) mems m)
                             :: acc
                           else acc))) t [])
           @ [output_clause (!most_mems) (!most_frequent)])) : case list)
let output_trans =
  function
  | (i : int) ->
      (function
       | (trans : Lexgen.automata) ->
           let output_tag_actions =
             function
             | (mvs : Lexgen.tag_action list) ->
                 (List.map
                    (function
                     | Lexgen.SetTag (t,m) ->
                         (`Assign
                            (_loc,
                              (`ArrayDot
                                 (_loc,
                                   (`Field
                                      (_loc, (`Lid (_loc, "lexbuf")),
                                        (`Lid (_loc, "lex_mem")))),
                                   (`Int (_loc, (string_of_int t))))),
                              (`ArrayDot
                                 (_loc,
                                   (`Field
                                      (_loc, (`Lid (_loc, "lexbuf")),
                                        (`Lid (_loc, "lex_mem")))),
                                   (`Int (_loc, (string_of_int m)))))) :> 
                         Astf.exp)
                     | EraseTag t ->
                         (`Assign
                            (_loc,
                              (`ArrayDot
                                 (_loc,
                                   (`Field
                                      (_loc, (`Lid (_loc, "lexbuf")),
                                        (`Lid (_loc, "lex_mem")))),
                                   (`Int (_loc, (string_of_int t))))),
                              (`Int (_loc, "-1"))) :> Astf.exp)) mvs : 
                 exp list) in
           let e =
             match trans with
             | Perform (n,mvs) ->
                 seq_sem
                   ((output_tag_actions mvs) @
                      [(`Int (_loc, (string_of_int n)) :> Astf.exp)])
             | Shift (trans,move) ->
                 let moves = bar_of_list (output_moves move) in
                 seq_sem
                   (match trans with
                    | Remember (n,mvs) ->
                        (output_tag_actions mvs) @
                          [(`Assign
                              (_loc,
                                (`Field
                                   (_loc, (`Lid (_loc, "lexbuf")),
                                     (`Lid (_loc, "lex_last_pos")))),
                                (`Field
                                   (_loc, (`Lid (_loc, "lexbuf")),
                                     (`Lid (_loc, "lex_curr_pos"))))) :> 
                          Astf.exp);
                          (`Assign
                             (_loc,
                               (`Field
                                  (_loc, (`Lid (_loc, "lexbuf")),
                                    (`Lid (_loc, "lex_last_action")))),
                               (`Int (_loc, (string_of_int n)))) :> Astf.exp);
                          (`Match
                             (_loc,
                               (`App
                                  (_loc,
                                    (`Lid (_loc, "__ocaml_lex_next_char")),
                                    (`Unit _loc))), moves) :> Astf.exp)]
                    | No_remember  ->
                        [(`Match
                            (_loc,
                              (`App
                                 (_loc,
                                   (`Lid (_loc, "__ocaml_lex_next_char")),
                                   (`Unit _loc))), moves) :> Astf.exp)]) in
           (`Bind
              (_loc, (`Lid (_loc, (lex_state i))),
                (`Fun (_loc, (`Case (_loc, (`Unit _loc), (e :> Astf.exp)))))) :> 
             Astf.bind))
let output_args =
  function
  | (args : string list) ->
      (function
       | e ->
           List.fold_right
             (function
              | a ->
                  (function
                   | b ->
                       (`Fun
                          (_loc,
                            (`Case (_loc, (`Lid (_loc, a)), (b :> Astf.exp)))) :> 
                       Astf.exp))) args e)
let output_automata =
  function
  | (transitions : Lexgen.automata array) ->
      ((transitions |>
          (Array.mapi
             (function | i -> (function | auto -> output_trans i auto))))
         |> Array.to_list : bind list)
let offset =
  function
  | e ->
      (function
       | i ->
           if i = 0
           then e
           else
             (`App
                (_loc, (`App (_loc, (`Lid (_loc, "+")), (e :> Astf.exp))),
                  (`Int (_loc, (string_of_int i)))) :> Astf.exp))
let output_env =
  function
  | (env : Automata_def.t_env) ->
      (let output_tag_access =
         function
         | ((x : Automata_def.tag_base),d) ->
             offset
               (match x with
                | Mem i ->
                    (`ArrayDot
                       (_loc,
                         (`Field
                            (_loc, (`Lid (_loc, "lexbuf")),
                              (`Lid (_loc, "lex_mem")))),
                         (`Int (_loc, (string_of_int i)))) :> Astf.exp)
                | Start  ->
                    (`Field
                       (_loc, (`Lid (_loc, "lexbuf")),
                         (`Lid (_loc, "lex_start_pos"))) :> Astf.exp)
                | End  ->
                    (`Field
                       (_loc, (`Lid (_loc, "lexbuf")),
                         (`Lid (_loc, "lex_curr_pos"))) :> Astf.exp)) d in
       (env |>
          (List.sort
             (function
              | x ->
                  (function
                   | y ->
                       (match (x, y) with
                        | (((p1,_),_),((p2,_),_)) ->
                            if Locf.strictly_before p1 p2 then (-1) else 1)))))
         |>
         (List.map
            (function
             | (((loc,_) as id),v) ->
                 let (id :pat)= `Lid id in
                 (match (v : Automata_def.ident_info) with
                  | Ident_string (o,nstart,nend) ->
                      let sub =
                        if o
                        then
                          (`Dot
                             (_loc, (`Uid (_loc, "Lexing")),
                               (`Lid (_loc, "sub_lexeme_opt"))) :> Astf.exp)
                        else
                          (`Dot
                             (_loc, (`Uid (_loc, "Lexing")),
                               (`Lid (_loc, "sub_lexeme"))) :> Astf.exp) in
                      let nstart = output_tag_access nstart in
                      let nend = output_tag_access nend in
                      (`Bind
                         (_loc, (id :> Astf.pat),
                           (`App
                              (_loc,
                                (`App
                                   (_loc,
                                     (`App
                                        (_loc, (sub :> Astf.exp),
                                          (`Lid (_loc, "lexbuf")))),
                                     (nstart :> Astf.exp))),
                                (nend :> Astf.exp)))) :> Astf.bind)
                  | Ident_char (o,nstart) ->
                      let sub =
                        if o
                        then
                          (`Dot
                             (_loc, (`Uid (_loc, "Lexing")),
                               (`Lid (_loc, "sub_lexeme_char_opt"))) :> 
                          Astf.exp)
                        else
                          (`Dot
                             (_loc, (`Uid (_loc, "Lexing")),
                               (`Lid (_loc, "sub_lexeme_char"))) :> Astf.exp) in
                      let nstart = output_tag_access nstart in
                      (`Bind
                         (loc, (id :> Astf.pat),
                           (`App
                              (loc,
                                (`App
                                   (loc, (sub :> Astf.exp),
                                     (`Lid (loc, "lexbuf")))),
                                (nstart :> Astf.exp)))) :> Astf.bind)))) : 
      bind list)
let output_entry =
  function
  | _loc ->
      (function
       | ({ Lexgen.auto_mem_size = auto_mem_size;
            auto_initial_state = (init_num,init_moves); auto_actions },
          (transitions : Lexgen.automata array)) ->
           (let binds =
              and_of_list
                ((`Bind
                    (_loc, (`Lid (_loc, "__ocaml_lex_next_char")),
                      (`Fun
                         (_loc,
                           (`Case
                              (_loc, (`Unit _loc),
                                (`IfThenElse
                                   (_loc,
                                     (`App
                                        (_loc,
                                          (`App
                                             (_loc, (`Lid (_loc, ">=")),
                                               (`Field
                                                  (_loc,
                                                    (`Lid (_loc, "lexbuf")),
                                                    (`Lid
                                                       (_loc, "lex_curr_pos")))))),
                                          (`Field
                                             (_loc, (`Lid (_loc, "lexbuf")),
                                               (`Lid (_loc, "lex_buffer_len")))))),
                                     (`Seq
                                        (_loc,
                                          (`IfThenElse
                                             (_loc,
                                               (`Field
                                                  (_loc,
                                                    (`Lid (_loc, "lexbuf")),
                                                    (`Lid
                                                       (_loc,
                                                         "lex_eof_reached")))),
                                               (`Int (_loc, "256")),
                                               (`Seq
                                                  (_loc,
                                                    (`Sem
                                                       (_loc,
                                                         (`App
                                                            (_loc,
                                                              (`Field
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "refill_buff")))),
                                                              (`Lid
                                                                 (_loc,
                                                                   "lexbuf")))),
                                                         (`App
                                                            (_loc,
                                                              (`Lid
                                                                 (_loc,
                                                                   "__ocaml_lex_next_char")),
                                                              (`Unit _loc))))))))))),
                                     (`Seq
                                        (_loc,
                                          (`LetIn
                                             (_loc, (`Negative _loc),
                                               (`Bind
                                                  (_loc, (`Lid (_loc, "i")),
                                                    (`Field
                                                       (_loc,
                                                         (`Lid
                                                            (_loc, "lexbuf")),
                                                         (`Lid
                                                            (_loc,
                                                              "lex_curr_pos")))))),
                                               (`Seq
                                                  (_loc,
                                                    (`Sem
                                                       (_loc,
                                                         (`Assign
                                                            (_loc,
                                                              (`Field
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "lex_curr_pos")))),
                                                              (`App
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "+")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "i")))),
                                                                   (`Int
                                                                    (_loc,
                                                                    "1")))))),
                                                         (`App
                                                            (_loc,
                                                              (`Dot
                                                                 (_loc,
                                                                   (`Uid
                                                                    (_loc,
                                                                    "Char")),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "code")))),
                                                              (`StringDot
                                                                 (_loc,
                                                                   (`Field
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lex_buffer")))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "i"))))))))))))))))))))) :> 
                Astf.bind) :: (output_automata transitions)) in
            let actions =
              seq_sem
                ((`LetIn
                    (_loc, (`Negative _loc),
                      (`Bind
                         (_loc, (`Lid (_loc, "pos")),
                           (`Field
                              (_loc, (`Lid (_loc, "lexbuf")),
                                (`Lid (_loc, "lex_curr_pos")))))),
                      (`Seq
                         (_loc,
                           (`Sem
                              (_loc,
                                (`Assign
                                   (_loc,
                                     (`Field
                                        (_loc, (`Lid (_loc, "lexbuf")),
                                          (`Lid (_loc, "lex_start_pos")))),
                                     (`Lid (_loc, "pos")))),
                                (`Sem
                                   (_loc,
                                     (`Assign
                                        (_loc,
                                          (`Field
                                             (_loc, (`Lid (_loc, "lexbuf")),
                                               (`Lid (_loc, "lex_last_pos")))),
                                          (`Lid (_loc, "pos")))),
                                     (`Assign
                                        (_loc,
                                          (`Field
                                             (_loc, (`Lid (_loc, "lexbuf")),
                                               (`Lid
                                                  (_loc, "lex_last_action")))),
                                          (`Int (_loc, "-1"))))))))))) :> 
                Astf.exp) ::
                (let init = output_memory_actions init_moves in
                 if auto_mem_size > 0
                 then
                   (`Assign
                      (_loc,
                        (`Field
                           (_loc, (`Lid (_loc, "lexbuf")),
                             (`Lid (_loc, "lex_mem")))),
                        (`App
                           (_loc,
                             (`App
                                (_loc,
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Array")),
                                       (`Lid (_loc, "create")))),
                                  (`Int (_loc, (string_of_int auto_mem_size))))),
                             (`Int (_loc, "-1"))))) :> Astf.exp)
                   :: init
                 else init)) in
            (`Fun
               (_loc,
                 (`Case
                    (_loc,
                      (`Constraint
                         (_loc, (`Lid (_loc, "lexbuf")),
                           (`Dot
                              (_loc, (`Uid (_loc, "Lexing")),
                                (`Lid (_loc, "lexbuf")))))),
                      (`LetIn
                         (_loc, (`Positive _loc), (binds :> Astf.bind),
                           (`Seq
                              (_loc,
                                (`Sem
                                   (_loc, (actions :> Astf.exp),
                                     (`LetIn
                                        (_loc, (`Negative _loc),
                                          (`Bind
                                             (_loc,
                                               (`Lid
                                                  (_loc,
                                                    "__ocaml_lex_result")),
                                               (`App
                                                  (_loc,
                                                    (`Lid
                                                       (_loc,
                                                         (lex_state init_num))),
                                                    (`Unit _loc))))),
                                          (`Seq
                                             (_loc,
                                               (`Sem
                                                  (_loc,
                                                    (`Assign
                                                       (_loc,
                                                         (`Field
                                                            (_loc,
                                                              (`Lid
                                                                 (_loc,
                                                                   "lexbuf")),
                                                              (`Lid
                                                                 (_loc,
                                                                   "lex_start_p")))),
                                                         (`Field
                                                            (_loc,
                                                              (`Lid
                                                                 (_loc,
                                                                   "lexbuf")),
                                                              (`Lid
                                                                 (_loc,
                                                                   "lex_curr_p")))))),
                                                    (`Sem
                                                       (_loc,
                                                         (`Assign
                                                            (_loc,
                                                              (`Field
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "lex_curr_p")))),
                                                              (`RecordWith
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "pos_cnum")),
                                                                    (`App
                                                                    (_loc,
                                                                    (`App
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "+")),
                                                                    (`Field
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lex_abs_pos")))))),
                                                                    (`Field
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lex_curr_pos")))))))),
                                                                   (`Field
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lex_curr_p")))))))),
                                                         (`Match
                                                            (_loc,
                                                              (`Lid
                                                                 (_loc,
                                                                   "__ocaml_lex_result")),
                                                              (`Bar
                                                                 (_loc,
                                                                   ((auto_actions
                                                                    |>
                                                                    (List.map
                                                                    (function
                                                                    | 
                                                                    (num,env,act)
                                                                    ->
                                                                    let e =
                                                                    Ast_gen.binds
                                                                    (output_env
                                                                    env) act in
                                                                    (`Case
                                                                    (_loc,
                                                                    (`Int
                                                                    (_loc,
                                                                    (string_of_int
                                                                    num))),
                                                                    (e :> 
                                                                    Astf.exp)) :> 
                                                                    Astf.case))))
                                                                    |>
                                                                    bar_of_list),
                                                                   (`Case
                                                                    (_loc,
                                                                    (`Any
                                                                    _loc),
                                                                    (`App
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "failwith")),
                                                                    (`App
                                                                    (_loc,
                                                                    (`App
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "^")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "__MODULE__")))),
                                                                    (`App
                                                                    (_loc,
                                                                    (`App
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "^")),
                                                                    (`Str
                                                                    (_loc,
                                                                    ".")))),
                                                                    (`App
                                                                    (_loc,
                                                                    (`App
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "^")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "__BIND__")))),
                                                                    (`Str
                                                                    (_loc,
                                                                    " lexing: empty token"))))))))))))))))))))))))))))))))) :> 
              Astf.exp) : Astf.exp))
