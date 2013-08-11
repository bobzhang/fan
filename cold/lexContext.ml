type context = 
  {
  loc: FLoc.position;
  in_comment: bool;
  antiquots: bool;
  lexbuf: lexbuf;
  buffer: Buffer.t} 
let store c = Buffer.add_string c.buffer (Lexing.lexeme c.lexbuf)
let buff_contents c =
  let contents = Buffer.contents c.buffer in Buffer.reset c.buffer; contents
let store_parse f c = store c; f c c.lexbuf