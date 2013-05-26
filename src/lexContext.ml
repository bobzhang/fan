

type context = {
    loc        :  FanLoc.position ;
    (** record the start position when enter into a quotation or antiquotaion *)
    in_comment : bool     ;
    antiquots  : bool     ;
    lexbuf     : lexbuf   ;
    buffer     : Buffer.t
  }


(** store the current lexeme into lexbuf *)      
let store c =
  Buffer.add_string c.buffer (Lexing.lexeme c.lexbuf)
      
(** return the string of the buffer, and [reset] the buffer  *)
let buff_contents c =
  let contents = Buffer.contents c.buffer in
  (Buffer.reset c.buffer; contents)

(** [store] and apply [f] *)
let store_parse f c =  
  (store c ; f c c.lexbuf)
    
