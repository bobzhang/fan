open Ast

open AstLoc

open Lexgen

let _loc = FanLoc.ghost

let auto_defs: Ast.stru =
  `Sem
    (_loc,
      (`Value
         (_loc, (`ReNil _loc),
           (`Bind
              (_loc, (`Lid (_loc, "__ocaml_lex_init_lexbuf")),
                (`Fun
                   (_loc,
                     (`Case
                        (_loc, (`Lid (_loc, "lexbuf")),
                          (`Fun
                             (_loc,
                               (`Case
                                  (_loc, (`Lid (_loc, "mem_size")),
                                    (`LetIn
                                       (_loc, (`ReNil _loc),
                                         (`Bind
                                            (_loc, (`Lid (_loc, "pos")),
                                              (`Field
                                                 (_loc,
                                                   (`Lid (_loc, "lexbuf")),
                                                   (`Dot
                                                      (_loc,
                                                        (`Uid
                                                           (_loc, "Lexing")),
                                                        (`Lid
                                                           (_loc,
                                                             "lex_curr_pos")))))))),
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
                                                             (`Dot
                                                                (_loc,
                                                                  (`Uid
                                                                    (_loc,
                                                                    "Lexing")),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "lex_mem")))))),
                                                        (`App
                                                           (_loc,
                                                             (`App
                                                                (_loc,
                                                                  (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Array")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "create")))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "mem_size")))),
                                                             (`Int
                                                                (_loc, "-1")))))),
                                                   (`Sem
                                                      (_loc,
                                                        (`Assign
                                                           (_loc,
                                                             (`Field
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                  (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Lexing")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lex_start_pos")))))),
                                                             (`Lid
                                                                (_loc, "pos")))),
                                                        (`Sem
                                                           (_loc,
                                                             (`Assign
                                                                (_loc,
                                                                  (`Field
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Lexing")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lex_last_pos")))))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "pos")))),
                                                             (`Assign
                                                                (_loc,
                                                                  (`Field
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Lexing")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lex_last_action")))))),
                                                                  (`Int
                                                                    (_loc,
                                                                    "-1")))))))))))))))))))))))))),
      (`Value
         (_loc, (`Recursive _loc),
           (`Bind
              (_loc, (`Lid (_loc, "__ocaml_lex_next_char")),
                (`Fun
                   (_loc,
                     (`Case
                        (_loc, (`Lid (_loc, "lexbuf")),
                          (`IfThenElse
                             (_loc,
                               (`App
                                  (_loc,
                                    (`App
                                       (_loc, (`Lid (_loc, ">=")),
                                         (`Field
                                            (_loc, (`Lid (_loc, "lexbuf")),
                                              (`Dot
                                                 (_loc,
                                                   (`Uid (_loc, "Lexing")),
                                                   (`Lid
                                                      (_loc, "lex_curr_pos")))))))),
                                    (`Field
                                       (_loc, (`Lid (_loc, "lexbuf")),
                                         (`Dot
                                            (_loc, (`Uid (_loc, "Lexing")),
                                              (`Lid (_loc, "lex_buffer_len")))))))),
                               (`Seq
                                  (_loc,
                                    (`IfThenElse
                                       (_loc,
                                         (`Field
                                            (_loc, (`Lid (_loc, "lexbuf")),
                                              (`Dot
                                                 (_loc,
                                                   (`Uid (_loc, "Lexing")),
                                                   (`Lid
                                                      (_loc,
                                                        "lex_eof_reached")))))),
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
                                                             (`Dot
                                                                (_loc,
                                                                  (`Uid
                                                                    (_loc,
                                                                    "Lexing")),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "refill_buff")))))),
                                                        (`Lid
                                                           (_loc, "lexbuf")))),
                                                   (`App
                                                      (_loc,
                                                        (`Lid
                                                           (_loc,
                                                             "__ocaml_lex_next_char")),
                                                        (`Lid
                                                           (_loc, "lexbuf")))))))))))),
                               (`Seq
                                  (_loc,
                                    (`Sem
                                       (_loc,
                                         (`LetIn
                                            (_loc, (`ReNil _loc),
                                              (`Bind
                                                 (_loc, (`Lid (_loc, "i")),
                                                   (`Field
                                                      (_loc,
                                                        (`Lid
                                                           (_loc, "lexbuf")),
                                                        (`Dot
                                                           (_loc,
                                                             (`Uid
                                                                (_loc,
                                                                  "Lexing")),
                                                             (`Lid
                                                                (_loc,
                                                                  "lex_curr_pos")))))))),
                                              (`LetIn
                                                 (_loc, (`ReNil _loc),
                                                   (`Bind
                                                      (_loc,
                                                        (`Lid (_loc, "c")),
                                                        (`StringDot
                                                           (_loc,
                                                             (`Field
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                  (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Lexing")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lex_buffer")))))),
                                                             (`Lid
                                                                (_loc, "i")))))),
                                                   (`Assign
                                                      (_loc,
                                                        (`Field
                                                           (_loc,
                                                             (`Lid
                                                                (_loc,
                                                                  "lexbuf")),
                                                             (`Dot
                                                                (_loc,
                                                                  (`Uid
                                                                    (_loc,
                                                                    "Lexing")),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "lex_curr_pos")))))),
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
                                                                (_loc, "1")))))))))),
                                         (`App
                                            (_loc,
                                              (`Dot
                                                 (_loc,
                                                   (`Uid (_loc, "Char")),
                                                   (`Lid (_loc, "code")))),
                                              (`Lid (_loc, "c")))))))))))))))))))

let output_pats (pats : int list) =
  bar_of_list
    (List.map (fun x  -> (`Int (_loc, (string_of_int x)) : Ast.pat )) pats)

let output_mem_access (i : int) =
  (`ArrayDot
     (_loc,
       (`Field
          (_loc, (`Lid (_loc, "lexbuf")),
            (`Dot (_loc, (`Uid (_loc, "Lexing")), (`Lid (_loc, "lex_mem")))))),
       (`Int (_loc, (string_of_int i)))) : Ast.exp )

let cur_pos: Ast.exp =
  `Field
    (_loc, (`Lid (_loc, "lexbuf")),
      (`Dot (_loc, (`Uid (_loc, "Lexing")), (`Lid (_loc, "lex_curr_pos")))))

let last_pos: Ast.exp =
  `Field
    (_loc, (`Lid (_loc, "lexbuf")),
      (`Dot (_loc, (`Uid (_loc, "Lexing")), (`Lid (_loc, "lex_last_pos")))))

let last_action: Ast.exp =
  `Field
    (_loc, (`Lid (_loc, "lexbuf")),
      (`Dot (_loc, (`Uid (_loc, "Lexing")), (`Lid (_loc, "last_action")))))

let lex_state i =
  let state = "__ocaml_lex_state" ^ (string_of_int i) in
  (`App (_loc, (`Lid (_loc, state)), (`Lid (_loc, "lexbuf"))) : Ast.exp )

let output_memory_actions (mvs : memory_action list) =
  (List.map
     (function
      | Copy (tgt,src) ->
          let u = output_mem_access tgt in
          let v = output_mem_access src in (`Assign (_loc, u, v) : Ast.exp )
      | Set tgt ->
          let u = output_mem_access tgt in
          (`Assign (_loc, u, cur_pos) : Ast.exp )) mvs : exp list )

let output_action (mems : memory_action list) (r : automata_move) =
  ((output_memory_actions mems) @
     (match r with
      | Backtrack  ->
          [(`Assign (_loc, cur_pos, last_pos) : Ast.exp ); last_action]
      | Goto n -> [lex_state n]) : exp list )

let output_clause (pats : int list) (mems : memory_action list)
  (r : automata_move) =
  let pat = output_pats pats in
  let action = seq_sem (output_action mems r) in
  (`Case (_loc, pat, action) : Ast.case )

let output_default_clause mems r =
  let action = seq_sem (output_action mems r) in
  (`Case (_loc, (`Any _loc), action) : Ast.case )

let output_moves (moves : (automata_move * memory_action list) array) =
  (let t = Hashtbl.create 17 in
   let add_move i (m,mems) =
     let (mems,r) = try Hashtbl.find t m with | Not_found  -> (mems, []) in
     Hashtbl.replace t m (mems, (i :: r)) in
   for i = 0 to 256 do add_move i (moves.(i)) done;
   (let most_frequent = ref Backtrack and most_mems = ref [] and size = ref 0 in
    Hashtbl.iter
      (fun m  (mems,pats)  ->
         let size_m = List.length pats in
         if size_m > size.contents
         then (most_frequent := m; most_mems := mems; size := size_m)) t;
    (Hashtbl.fold
       (fun m  (mems,pats)  acc  ->
          if m <> most_frequent.contents
          then (output_clause (List.rev pats) mems m) :: acc
          else acc) t [])
      @ [output_default_clause most_mems.contents most_frequent.contents]) : 
  case list )

let output_tag_actions (mvs : Lexgen.tag_action list) =
  (List.map
     (function
      | Lexgen.SetTag (t,m) ->
          let u = output_mem_access t in
          let v = output_mem_access m in (`Assign (_loc, u, v) : Ast.exp )
      | Lexgen.EraseTag t ->
          let u = output_mem_access t in
          (`Assign (_loc, u, (`Int (_loc, "-1"))) : Ast.exp )) mvs : 
  exp list )

let output_trans (i : int) (trans : automata) =
  let state = "__ocaml_lex_state" ^ (string_of_int i) in
  let e =
    match trans with
    | Perform (n,mvs) ->
        let es = output_tag_actions mvs in
        seq_sem (es @ [(`Int (_loc, (string_of_int n)) : Ast.exp )])
    | Shift (trans,move) ->
        let moves = bar_of_list (output_moves move) in
        seq_sem
          (match trans with
           | Remember (n,mvs) ->
               let es = output_tag_actions mvs in
               es @
                 [(`Assign (_loc, last_pos, cur_pos) : Ast.exp );
                 (`Assign
                    (_loc, last_action, (`Int (_loc, (string_of_int n)))) : 
                 Ast.exp );
                 (`Match
                    (_loc,
                      (`App
                         (_loc, (`Lid (_loc, "__ocaml_lex_next_char")),
                           (`Lid (_loc, "lexbuf")))), moves) : Ast.exp )]
           | No_remember  ->
               [(`Match
                   (_loc,
                     (`App
                        (_loc, (`Lid (_loc, "__ocaml_lex_next_char")),
                          (`Lid (_loc, "lexbuf")))), moves) : Ast.exp )]) in
  (`Bind
     (_loc, (`Lid (_loc, state)),
       (`Fun (_loc, (`Case (_loc, (`Lid (_loc, "lexbuf")), e))))) : Ast.binding )

let output_args (args : string list) e =
  List.fold_right
    (fun a  b  ->
       (`Fun (_loc, (`Case (_loc, (`Lid (_loc, a)), b))) : Ast.exp )) args e

let output_automata (transitions : Lexgen.automata array) =
  (let bind =
     and_of_list
       (Array.to_list
          (Array.mapi (fun i  auto  -> output_trans i auto) transitions)) in
   let acts: Ast.stru = `Value (_loc, (`Recursive _loc), bind) in
   sem auto_defs acts : stru )