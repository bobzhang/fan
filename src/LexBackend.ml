
open Ast
open AstLoc
open Lexgen
  
let _loc = FanLoc.ghost
    
let auto_defs = {:stru|
  let __ocaml_lex_init_lexbuf lexbuf mem_size =
    let pos = lexbuf.Lexing.lex_curr_pos in
    (lexbuf.Lexing.lex_mem <- Array.create mem_size (-1) ;
     lexbuf.Lexing.lex_start_pos <- pos ;
     lexbuf.Lexing.lex_last_pos <- pos ;
     lexbuf.Lexing.lex_last_action <- (-1))

  let rec __ocaml_lex_next_char lexbuf =
    if lexbuf.Lexing.lex_curr_pos >= lexbuf.Lexing.lex_buffer_len then
      begin
        if lexbuf.Lexing.lex_eof_reached then
          256
        else begin
          lexbuf.Lexing.refill_buff lexbuf ;
          __ocaml_lex_next_char lexbuf
        end
      end else begin
        let i = lexbuf.Lexing.lex_curr_pos in
        let c = lexbuf.Lexing.lex_buffer.[i] in
        lexbuf.Lexing.lex_curr_pos <- i+1 ;
        Char.code c
      end
|}

let output_pats (pats:int list) =
  bar_of_list (List.map (fun x -> {:pat|$`int:x|}) pats)


let output_mem_access (i:int) = {:exp|lexbuf.Lexing.lex_mem.($`int:i)|}
let cur_pos  = {:exp|lexbuf.Lexing.lex_curr_pos |}
let last_pos = {:exp|lexbuf.Lexing.lex_last_pos |}
let last_action = {:exp|lexbuf.Lexing.last_action |}

let lex_state i =
  let state = "__ocaml_lex_state"^string_of_int i in
  {:exp| $lid:state lexbuf |}
    
let output_memory_actions (mvs:memory_action list) : exp list =
  List.map
    (function
      | Copy(tgt,src) ->
          let u = output_mem_access tgt in
          let v = output_mem_access src in 
          {:exp| $u <- $v |}
      | Set tgt ->
          let u = output_mem_access tgt in
          {:exp| $u <- $cur_pos |}) mvs
    

(* length at least  one*)    
let output_action (mems:memory_action list) (r:automata_move) : exp list  =
   output_memory_actions mems @
  (match r with
  | Backtrack ->
      [ {:exp|$cur_pos <- $last_pos |};
        last_action]
  | Goto n ->
      [lex_state n]
   )
let output_clause
    (pats:int list)
    (mems:memory_action list)
    (r:automata_move) =
  let pat = output_pats pats in
  let action = seq_sem (output_action mems r) in 
  {:case| $pat:pat -> $action |}

let output_default_clause mems r =
  let action = seq_sem (output_action mems r) in
  {:case| _ -> $action |}

(* output at least one case *)    
let output_moves (moves: (automata_move * memory_action list) array) : case list =
  let t = Hashtbl.create 17 in
  let add_move i (m,mems) =
    let (mems,r) = try Hashtbl.find t m with Not_found -> (mems,[]) in
    Hashtbl.replace t m (mems,(i::r)) in
  begin
    for i = 0 to 256 do
      add_move i moves.(i)
    done ;
    let most_frequent = ref Backtrack
    and most_mems = ref []
    and size = ref 0 in
    (Hashtbl.iter
       (fun m (mems,pats) ->
         let size_m = List.length pats in
         if size_m > !size then begin
           most_frequent := m ;
           most_mems := mems;
           size := size_m
         end
       )
       t;
     (Hashtbl.fold 
        (fun m (mems,pats) acc ->
          if m <> !most_frequent then
            output_clause (List.rev pats) mems m  :: acc
          else acc
        ) t []) @ [output_default_clause !most_mems !most_frequent])
  end
    
let output_tag_actions (mvs:Lexgen.tag_action list) : exp list =
  List.map
    (function
      | Lexgen.SetTag(t,m) ->
          let u = output_mem_access t in
          let v = output_mem_access m in 
          {:exp| $u <- $v |}
      | Lexgen.EraseTag(t) ->
          let u = output_mem_access t in 
          {:exp| $u <- -1 |}) mvs

    
let output_trans (i:int) (trans:automata)=
  let state = "__ocaml_lex_state"^(string_of_int i) in
  let e =
    match trans with
  | Perform(n,mvs) ->
      let es = output_tag_actions mvs in 
      seq_sem (es@ [{:exp| $`int:n|}]) 

  | Shift(trans,move) ->
      let moves = bar_of_list (output_moves move) in
      seq_sem
          (match trans with
          | Remember(n,mvs) ->
              let es = output_tag_actions mvs in
              (es@
               [{:exp| $last_pos  <- $cur_pos |};
                {:exp| $last_action <- $`int:n |};
                {:exp| match __ocaml_lex_next_char lexbuf with | $moves |}
              ])
          | No_remember ->
              [{:exp| match __ocaml_lex_next_char lexbuf with | $moves |}]) in
  {:binding| $lid:state lexbuf = $e  |}
      
let output_args (args:string list) e =
  List.fold_right (fun a b -> {:exp| fun $lid:a -> $b |}) args e
    
let output_automata (transitions:Lexgen.automata array) : stru  =
  let bind= (and_of_list
               (Array.to_list (Array.mapi (fun i auto -> output_trans i auto) transitions))) in
  let acts = {:stru| let rec $bind |} in
  sem auto_defs acts
    

(* let output_entry *)
(*     (tr:Common.line_tracker) *)
(*     { auto_name; *)
(*     auto_args; *)
(*     auto_mem_size; *)
(*     auto_initial_state=(init_num,init_moves); *)
(*     auto_actions; } = *)
(*   let actions = output_memory_actions init_moves in *)
  
(*   {:exp| *)
(*   __ocaml_lex_init_lexbuf lexbuf $`int:auto_mem_size|} *)
(*     :: actions @ *)
(*   [ {:exp| lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p |}; *)
(*     {:exp| lexbuf.Lexing.lex_curr_p <- *)
(*       { (lexbuf.Lexing.lex_curr_p) with *)
(*         Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos } *)
(*       |}; *)
(*     {:exp| match __ocaml_lex_result with *)
      
(*     |} *)
(*   ] *)
      
(* (\* let output_def *\) *)
(* (\*     (tr:Common.line_tracker) *\) *)
(* (\*     (header:LexSyntax.location) *\) *)
(* (\*     (entry_points: Lexgen.automata_entry list) *\) *)
(* (\*     (transitions:Lexgen.automata array) *\) *)
(* (\*     (trailer:LexSyntax.location) = begin *\) *)
(* (\*       output_automata transitions; *\) *)
(* (\*       match entry_points with *\) *)
(* (\*       | [] -> () *\) *)
(* (\*       | e::es -> *\) *)
          
             
(* (\*     end *\) *)

  
    

    
    

    
(* (\* let output_mem_access i = *\) *)
(* (\*   {:exp|lexbuf.Lexing.lex_mem.($`int:i)|} *\) *)
    
