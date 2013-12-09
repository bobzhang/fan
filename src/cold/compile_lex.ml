open Astf
open Ast_gen
let _loc = Locf.ghost
let auto_binds =
  [(`Bind
      (_loc, (`Lid (_loc, "__ocaml_lex_init_lexbuf")),
        (`Fun
           (_loc,
             (`Case
                (_loc, (`Lid (_loc, "mem_size")),
                  (`LetIn
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
                                           (`Lid (_loc, "lex_mem")))),
                                      (`App
                                         (_loc,
                                           (`App
                                              (_loc,
                                                (`Dot
                                                   (_loc,
                                                     (`Uid (_loc, "Array")),
                                                     (`Lid (_loc, "create")))),
                                                (`Lid (_loc, "mem_size")))),
                                           (`Int (_loc, "-1")))))),
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
                                                   (_loc,
                                                     (`Lid (_loc, "lexbuf")),
                                                     (`Lid
                                                        (_loc,
                                                          "lex_last_pos")))),
                                                (`Lid (_loc, "pos")))),
                                           (`Assign
                                              (_loc,
                                                (`Field
                                                   (_loc,
                                                     (`Lid (_loc, "lexbuf")),
                                                     (`Lid
                                                        (_loc,
                                                          "lex_last_action")))),
                                                (`Int (_loc, "-1"))))))))))))))))))) : 
  Astf.bind );
  (`Bind
     (_loc, (`Lid (_loc, "__ocaml_lex_next_char")),
       (`Fun
          (_loc,
            (`Case
               (_loc, (`Uid (_loc, "()")),
                 (`IfThenElse
                    (_loc,
                      (`App
                         (_loc,
                           (`App
                              (_loc, (`Lid (_loc, ">=")),
                                (`Field
                                   (_loc, (`Lid (_loc, "lexbuf")),
                                     (`Lid (_loc, "lex_curr_pos")))))),
                           (`Field
                              (_loc, (`Lid (_loc, "lexbuf")),
                                (`Lid (_loc, "lex_buffer_len")))))),
                      (`Seq
                         (_loc,
                           (`IfThenElse
                              (_loc,
                                (`Field
                                   (_loc, (`Lid (_loc, "lexbuf")),
                                     (`Lid (_loc, "lex_eof_reached")))),
                                (`Int (_loc, "256")),
                                (`Seq
                                   (_loc,
                                     (`Sem
                                        (_loc,
                                          (`App
                                             (_loc,
                                               (`Field
                                                  (_loc,
                                                    (`Lid (_loc, "lexbuf")),
                                                    (`Lid
                                                       (_loc, "refill_buff")))),
                                               (`Lid (_loc, "lexbuf")))),
                                          (`App
                                             (_loc,
                                               (`Lid
                                                  (_loc,
                                                    "__ocaml_lex_next_char")),
                                               (`Uid (_loc, "()")))))))))))),
                      (`Seq
                         (_loc,
                           (`LetIn
                              (_loc, (`Negative _loc),
                                (`Bind
                                   (_loc, (`Lid (_loc, "i")),
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
                                                  (_loc,
                                                    (`Lid (_loc, "lexbuf")),
                                                    (`Lid
                                                       (_loc, "lex_curr_pos")))),
                                               (`App
                                                  (_loc,
                                                    (`App
                                                       (_loc,
                                                         (`Lid (_loc, "+")),
                                                         (`Lid (_loc, "i")))),
                                                    (`Int (_loc, "1")))))),
                                          (`App
                                             (_loc,
                                               (`Dot
                                                  (_loc,
                                                    (`Uid (_loc, "Char")),
                                                    (`Lid (_loc, "code")))),
                                               (`StringDot
                                                  (_loc,
                                                    (`Field
                                                       (_loc,
                                                         (`Lid
                                                            (_loc, "lexbuf")),
                                                         (`Lid
                                                            (_loc,
                                                              "lex_buffer")))),
                                                    (`Lid (_loc, "i"))))))))))))))))))))) : 
  Astf.bind )]
let output_memory_actions (mvs : Lexgen.memory_action list) =
  (List.map
     (fun (x : Lexgen.memory_action)  ->
        match x with
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
                      (`Int (_loc, (string_of_int src)))))) : Astf.exp )
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
                      (`Lid (_loc, "lex_curr_pos"))))) : Astf.exp )) mvs : 
  exp list )
let lex_state i = "__ocaml_lex_state" ^ (string_of_int i)
let output_action (mems : Lexgen.memory_action list)
  (r : Lexgen.automata_move) =
  ((output_memory_actions mems) @
     (match r with
      | Backtrack  ->
          [(`Assign
              (_loc,
                (`Field
                   (_loc, (`Lid (_loc, "lexbuf")),
                     (`Lid (_loc, "lex_curr_pos")))),
                (`Field
                   (_loc, (`Lid (_loc, "lexbuf")),
                     (`Lid (_loc, "lex_last_pos"))))) : Astf.exp );
          (`Field
             (_loc, (`Lid (_loc, "lexbuf")),
               (`Lid (_loc, "lex_last_action"))) : Astf.exp )]
      | Goto n ->
          [(`App (_loc, (`Lid (_loc, (lex_state n))), (`Uid (_loc, "()"))) : 
          Astf.exp )]) : exp list )
let output_clause (pats : int list) (mems : Lexgen.memory_action list)
  (r : Lexgen.automata_move) =
  let pat =
    bar_of_list
      (List.map (fun x  -> (`Int (_loc, (string_of_int x)) : Astf.pat )) pats) in
  let action = seq_sem (output_action mems r) in
  (`Case (_loc, pat, action) : Astf.case )
let output_default_clause mems r =
  let action = seq_sem (output_action mems r) in
  (`Case (_loc, (`Any _loc), action) : Astf.case )
let output_moves
  (moves : (Lexgen.automata_move* Lexgen.memory_action list) array) =
  (let t = Hashtbl.create 17 in
   let add_move i (m,mems) =
     let (mems,r) = try Hashtbl.find t m with | Not_found  -> (mems, []) in
     Hashtbl.replace t m (mems, (i :: r)) in
   for i = 0 to 256 do add_move i (moves.(i)) done;
   (let most_frequent = ref Lexgen.Backtrack
    and most_mems = ref []
    and size = ref 0 in
    Hashtbl.iter
      (fun m  (mems,pats)  ->
         let size_m = List.length pats in
         if size_m > (!size)
         then (most_frequent := m; most_mems := mems; size := size_m)) t;
    (Hashtbl.fold
       (fun m  (mems,pats)  acc  ->
          if m <> (!most_frequent)
          then (output_clause (List.rev pats) mems m) :: acc
          else acc) t [])
      @ [output_default_clause (!most_mems) (!most_frequent)]) : case list )
let output_trans (i : int) (trans : Lexgen.automata) =
  let output_tag_actions (mvs : Lexgen.tag_action list) =
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
                      (`Int (_loc, (string_of_int m)))))) : Astf.exp )
        | EraseTag t ->
            (`Assign
               (_loc,
                 (`ArrayDot
                    (_loc,
                      (`Field
                         (_loc, (`Lid (_loc, "lexbuf")),
                           (`Lid (_loc, "lex_mem")))),
                      (`Int (_loc, (string_of_int t))))),
                 (`Int (_loc, "-1"))) : Astf.exp )) mvs : exp list ) in
  let e =
    match trans with
    | Perform (n,mvs) ->
        let es = output_tag_actions mvs in
        seq_sem (es @ [(`Int (_loc, (string_of_int n)) : Astf.exp )])
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
                            (`Lid (_loc, "lex_curr_pos"))))) : Astf.exp );
                 (`Assign
                    (_loc,
                      (`Field
                         (_loc, (`Lid (_loc, "lexbuf")),
                           (`Lid (_loc, "lex_last_action")))),
                      (`Int (_loc, (string_of_int n)))) : Astf.exp );
                 (`Match
                    (_loc,
                      (`App
                         (_loc, (`Lid (_loc, "__ocaml_lex_next_char")),
                           (`Uid (_loc, "()")))), moves) : Astf.exp )]
           | No_remember  ->
               [(`Match
                   (_loc,
                     (`App
                        (_loc, (`Lid (_loc, "__ocaml_lex_next_char")),
                          (`Uid (_loc, "()")))), moves) : Astf.exp )]) in
  (`Bind
     (_loc, (`Lid (_loc, (lex_state i))),
       (`Fun (_loc, (`Case (_loc, (`Uid (_loc, "()")), e))))) : Astf.bind )
let output_args (args : string list) e =
  List.fold_right
    (fun a  b  ->
       (`Fun (_loc, (`Case (_loc, (`Lid (_loc, a)), b))) : Astf.exp )) args e
let output_automata (transitions : Lexgen.automata array) =
  ((transitions |> (Array.mapi (fun i  auto  -> output_trans i auto))) |>
     Array.to_list : bind list )
let output_env (env : Automata_def.t_env) =
  (let env =
     List.sort
       (fun x  y  ->
          match (x, y) with
          | (((p1,_),_),((p2,_),_)) ->
              if Locf.strictly_before p1 p2 then (-1) else 1) env in
   let output_tag_access (x : (Automata_def.tag_base* int)) =
     match x with
     | (Automata_def.Mem i,d) ->
         (`App
            (_loc,
              (`App
                 (_loc, (`Lid (_loc, "+")),
                   (`ArrayDot
                      (_loc,
                        (`Field
                           (_loc, (`Lid (_loc, "lexbuf")),
                             (`Lid (_loc, "lex_mem")))),
                        (`Int (_loc, (string_of_int i))))))),
              (`Int (_loc, (string_of_int d)))) : Astf.exp )
     | (Start ,d) ->
         (`App
            (_loc,
              (`App
                 (_loc, (`Lid (_loc, "+")),
                   (`Field
                      (_loc, (`Lid (_loc, "lexbuf")),
                        (`Lid (_loc, "lex_start_pos")))))),
              (`Int (_loc, (string_of_int d)))) : Astf.exp )
     | (End ,d) ->
         (`App
            (_loc,
              (`App
                 (_loc, (`Lid (_loc, "+")),
                   (`Field
                      (_loc, (`Lid (_loc, "lexbuf")),
                        (`Lid (_loc, "lex_curr_pos")))))),
              (`Int (_loc, (string_of_int d)))) : Astf.exp ) in
   List.map
     (fun (((loc,_) as id),v)  ->
        let (id :pat)= `Lid id in
        match (v : Automata_def.ident_info ) with
        | Ident_string (o,nstart,nend) ->
            let sub =
              if o
              then
                (`Dot
                   (_loc, (`Uid (_loc, "Lexing")),
                     (`Lid (_loc, "sub_lexeme_opt"))) : Astf.exp )
              else
                (`Dot
                   (_loc, (`Uid (_loc, "Lexing")),
                     (`Lid (_loc, "sub_lexeme"))) : Astf.exp ) in
            let nstart = output_tag_access nstart in
            let nend = output_tag_access nend in
            (`Bind
               (_loc, id,
                 (`App
                    (_loc,
                      (`App
                         (_loc, (`App (_loc, sub, (`Lid (_loc, "lexbuf")))),
                           nstart)), nend))) : Astf.bind )
        | Ident_char (o,nstart) ->
            let sub =
              if o
              then
                (`Dot
                   (_loc, (`Uid (_loc, "Lexing")),
                     (`Lid (_loc, "sub_lexeme_char_opt"))) : Astf.exp )
              else
                (`Dot
                   (_loc, (`Uid (_loc, "Lexing")),
                     (`Lid (_loc, "sub_lexeme_char"))) : Astf.exp ) in
            let nstart = output_tag_access nstart in
            (`Bind
               (loc, id,
                 (`App
                    (loc, (`App (loc, sub, (`Lid (loc, "lexbuf")))), nstart))) : 
              Astf.bind )) env : bind list )
let output_entry _loc
  ({ Lexgen.auto_mem_size = auto_mem_size;
     auto_initial_state = (init_num,init_moves); auto_actions },(transitions
                                                                  :
                                                                  Lexgen.automata
                                                                    array))
  =
  (let binds = and_of_list (auto_binds @ (output_automata transitions)) in
   let actions =
     seq_sem
       ((`App
           (_loc, (`Lid (_loc, "__ocaml_lex_init_lexbuf")),
             (`Int (_loc, (string_of_int auto_mem_size)))) : Astf.exp ) ::
       (output_memory_actions init_moves)) in
   (`Fun
      (_loc,
        (`Case
           (_loc,
             (`Constraint
                (_loc, (`Lid (_loc, "lexbuf")),
                  (`Dot
                     (_loc, (`Uid (_loc, "Lexing")), (`Lid (_loc, "lexbuf")))))),
             (`LetIn
                (_loc, (`Positive _loc), binds,
                  (`Seq
                     (_loc,
                       (`Sem
                          (_loc, actions,
                            (`LetIn
                               (_loc, (`Negative _loc),
                                 (`Bind
                                    (_loc,
                                      (`Lid (_loc, "__ocaml_lex_result")),
                                      (`App
                                         (_loc,
                                           (`Lid (_loc, (lex_state init_num))),
                                           (`Uid (_loc, "()")))))),
                                 (`Seq
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`Assign
                                              (_loc,
                                                (`Field
                                                   (_loc,
                                                     (`Lid (_loc, "lexbuf")),
                                                     (`Lid
                                                        (_loc, "lex_start_p")))),
                                                (`Field
                                                   (_loc,
                                                     (`Lid (_loc, "lexbuf")),
                                                     (`Lid
                                                        (_loc, "lex_curr_p")))))),
                                           (`Sem
                                              (_loc,
                                                (`Assign
                                                   (_loc,
                                                     (`Field
                                                        (_loc,
                                                          (`Lid
                                                             (_loc, "lexbuf")),
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
                                                                    (
                                                                    `App
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
                                                                    (
                                                                    `Field
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
                                                          ((auto_actions |>
                                                              (List.map
                                                                 (fun
                                                                    (num,env,act)
                                                                     ->
                                                                    let n =
                                                                    string_of_int
                                                                    num in
                                                                    match 
                                                                    output_env
                                                                    env
                                                                    with
                                                                    | 
                                                                    [] ->
                                                                    (`Case
                                                                    (_loc,
                                                                    (`Int
                                                                    (_loc, n)),
                                                                    act) : 
                                                                    Astf.case )
                                                                    | 
                                                                    xs ->
                                                                    let bind
                                                                    =
                                                                    and_of_list
                                                                    xs in
                                                                    (`Case
                                                                    (_loc,
                                                                    (`Int
                                                                    (_loc, n)),
                                                                    (`LetIn
                                                                    (_loc,
                                                                    (`Negative
                                                                    _loc),
                                                                    bind,
                                                                    act))) : 
                                                                    Astf.case ))))
                                                             |> bar_of_list),
                                                          (`Case
                                                             (_loc,
                                                               (`Any _loc),
                                                               (`App
                                                                  (_loc,
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "failwith")),
                                                                    (
                                                                    `App
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
                                                                    " lexing: empty token"))))))))))))))))))))))))))))))))) : 
     Astf.exp ) : Astf.exp )
