let  valch (x) = (( (Char.code x) ) - ( (Char.code '0') ))
let  valch_hex (x) =
  
  let  d = (Char.code x) in
  if (d >= 97) then ( (d - 87) )
  else if (d >= 65) then ( (d - 55) )
  else (d - 48)
let rec  skip_indent ((__strm : _ Stream.t )) =
  
  (match (Stream.peek __strm)
  with
  | Some(' ' |'\t') -> ( (Stream.junk __strm) ); (skip_indent __strm)
  | _ -> ())
let  skip_opt_linefeed ((__strm : _ Stream.t )) =
  
  (match (Stream.peek __strm)
  with
  | Some('\n') -> ( (Stream.junk __strm) ); ()
  | _ -> ())
let  chr (c) =
  if (( (c < 0) ) || ( (c > 255) )) then ( (failwith "invalid char token") )
  else (Char.chr c)
let  backslash ((__strm : _ Stream.t )) =
  
  (match (Stream.peek __strm)
  with
  | Some(((((('\n' |'\r') |'\\') |'\'') |' ') |'"') as x) ->
    ( (Stream.junk __strm) ); x
  | Some('n') -> ( (Stream.junk __strm) ); '\n'
  | Some('r') -> ( (Stream.junk __strm) ); '\r'
  | Some('t') -> ( (Stream.junk __strm) ); '\t'
  | Some('b') -> ( (Stream.junk __strm) ); '\b'
  |
    Some(('0' |('1' |('2' |('3' |('4' |('5' |('6' |('7' |('8' |'9')))))))))
          as c1) ->
    (
    (Stream.junk __strm)
    );
    
    (match (Stream.peek __strm)
    with
    |
      Some(('0' |('1' |('2' |('3' |('4' |('5' |('6' |('7' |('8' |'9')))))))))
            as c2) ->
      (
      (Stream.junk __strm)
      );
      
      (match (Stream.peek __strm)
      with
      |
        Some(('0'
               |('1' |('2' |('3' |('4' |('5' |('6' |('7' |('8' |'9')))))))))
              as c3) ->
        (
        (Stream.junk __strm)
        );
        (chr (
          (( (( (100 * ( (valch c1) )) ) + ( (10 * ( (valch c2) )) )) ) + (
            (valch c3) )) ))
      | _ -> (raise ( Stream.Error ("") )))
    | _ -> (raise ( Stream.Error ("") )))
  | Some('x') ->
    (
    (Stream.junk __strm)
    );
    
    (match (Stream.peek __strm)
    with
    |
      Some(((('0'
               |('1' |('2' |('3' |('4' |('5' |('6' |('7' |('8' |'9')))))))))
              |('a' |('b' |('c' |('d' |('e' |'f'))))))
             |('A' |('B' |('C' |('D' |('E' |'F')))))) as c1) ->
      (
      (Stream.junk __strm)
      );
      
      (match (Stream.peek __strm)
      with
      |
        Some(((('0'
                 |('1' |('2' |('3' |('4' |('5' |('6' |('7' |('8' |'9')))))))))
                |('a' |('b' |('c' |('d' |('e' |'f'))))))
               |('A' |('B' |('C' |('D' |('E' |'F')))))) as c2) ->
        (
        (Stream.junk __strm)
        );
        (chr ( (( (16 * ( (valch_hex c1) )) ) + ( (valch_hex c2) )) ))
      | _ -> (raise ( Stream.Error ("") )))
    | _ -> (raise ( Stream.Error ("") )))
  | _ -> (raise Stream.Failure ))
let  backslash_in_string (strict) (store) ((__strm : _ Stream.t )) =
  
  (match (Stream.peek __strm)
  with
  | Some('\n') -> ( (Stream.junk __strm) ); (skip_indent __strm)
  | Some('\r') ->
    (
    (Stream.junk __strm)
    );
    
    let  s = __strm in begin
    ( (skip_opt_linefeed s) ); (skip_indent s)
    end
  | _ ->
    
    (match 
    (try Some ((backslash __strm)) with
    | Stream.Failure  -> None)
    with
    | Some(x) -> (store x)
    | _ ->
      
      (match (Stream.peek __strm)
      with
      | Some(c) when (not strict) ->
        ( (Stream.junk __strm) ); ( (store '\\') ); (store c)
      | _ -> (failwith "invalid string token"))))
let  char (s) =
  if (( (String.length s) ) = 1) then ( s.[0] )
  else if (( (String.length s) ) = 0) then
        (
        (failwith "invalid char token")
        )
  else
   
   let  (__strm : _ Stream.t ) = (Stream.of_string s) in
   
   (match (Stream.peek __strm)
   with
   | Some('\\') ->
     (
     (Stream.junk __strm)
     );
     
     (try (backslash __strm)
     with
     | Stream.Failure  -> (raise ( Stream.Error ("") )))
   | _ -> (failwith "invalid char token"))
let  string ?strict  (s) =
  
  let  buf = (Buffer.create 23) in
  
  let  store = (Buffer.add_char buf) in
  
  let rec  parse ((__strm : _ Stream.t )) =
  
  (match (Stream.peek __strm)
  with
  | Some('\\') ->
    (
    (Stream.junk __strm)
    );
    
    let  _ =
    
    (try (backslash_in_string ( (strict <> None ) ) store __strm)
    with
    | Stream.Failure  -> (raise ( Stream.Error ("") ))) in (parse __strm)
  | Some(c) ->
    (
    (Stream.junk __strm)
    );
    
    let  s = __strm in begin
    ( (store c) ); (parse s)
    end
  | _ -> (Buffer.contents buf)) in (parse ( (Stream.of_string s) ))