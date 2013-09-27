
(** Fan's lexer using [lex] DDSL *)  


val token :  Lexing.lexbuf ->   (Ftoken.t * FLoc.t )


(** In initial stage
    [Lexing.lexeme_start_p] returns
    {[ Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 ]}
    for a string input or a channel input (from_string, from_channel). *)    
val from_lexbuf : Lexing.lexbuf -> ( Ftoken.t * FLoc.t   ) Fstream.t




