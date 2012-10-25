let  valch (x) = (( (Char.code x) ) - ( (Char.code '0') ))
let  valch_hex (x) =
  
  let  d = (Char.code x) in
  if (d >= 97) then ( (d - 87) )
  else if (d >= 65) then ( (d - 55) )
  else (d - 48)
let rec  skip_indent ((__strm : _ Stream.t )) =
  
  (match (Stream.peek __strm)
  with
  | Some(' ' |'\t') -> begin
    (Stream.junk __strm);
    (skip_indent __strm)
    end | _ -> ())
let  skip_opt_linefeed ((__strm : _ Stream.t )) =
  
  (match (Stream.peek __strm)
  with
  | Some('\n') -> begin
    (Stream.junk __strm);
    ()
    end | _ -> ())
let  chr (c) =
  if (( (c < 0) ) || ( (c > 255) )) then ( (failwith "invalid char token") )
  else (Char.chr c)
let  backslash ((__strm : _ Stream.t )) =
  
  (match (Stream.peek __strm)
  with
  | Some(((((('\n' |'\r') |'\\') |'\'') |' ') |'"') as x) ->
    begin
    (Stream.junk __strm);
    x
    end | Some('n') -> begin
          (Stream.junk __strm);
          '\n'
          end | Some('r') -> begin
                (Stream.junk __strm);
                '\r'
                end | Some('t') -> begin
                      (Stream.junk __strm);
                      '\t'
                      end | Some('b') -> begin
                            (Stream.junk __strm);
                            '\b'
                            end
    | Some(('0'..'9') as c1) ->
      begin
      (Stream.junk __strm);
      
      (match (Stream.peek __strm)
      with
      | Some(('0'..'9') as c2) ->
        begin
        (Stream.junk __strm);
        
        (match (Stream.peek __strm)
        with
        | Some(('0'..'9') as c3) ->
          begin
          (Stream.junk __strm);
          (chr (
            (( (( (100 * ( (valch c1) )) ) + ( (10 * ( (valch c2) )) )) ) + (
              (valch c3) )) ))
          end | _ -> (raise ( Stream.Error ("") )))
        end | _ -> (raise ( Stream.Error ("") )))
      end
    | Some('x') ->
      begin
      (Stream.junk __strm);
      
      (match (Stream.peek __strm)
      with
      | Some(((('0'..'9') |('a'..'f')) |('A'..'F')) as c1) ->
        begin
        (Stream.junk __strm);
        
        (match (Stream.peek __strm)
        with
        | Some(((('0'..'9') |('a'..'f')) |('A'..'F')) as c2) ->
          begin
          (Stream.junk __strm);
          (chr ( (( (16 * ( (valch_hex c1) )) ) + ( (valch_hex c2) )) ))
          end | _ -> (raise ( Stream.Error ("") )))
        end | _ -> (raise ( Stream.Error ("") )))
      end | _ -> (raise Stream.Failure ))
let  backslash_in_string (strict) (store) ((__strm : _ Stream.t )) =
  
  (match (Stream.peek __strm)
  with
  | Some('\n') -> begin
    (Stream.junk __strm);
    (skip_indent __strm)
    end
    | Some('\r') ->
      begin
      (Stream.junk __strm);
      
      let  s = __strm in begin
      (skip_opt_linefeed s);
      (skip_indent s)
      end
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
            begin
            (Stream.junk __strm);
            (store '\\');
            (store c)
            end | _ -> (failwith "invalid string token"))))
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
     begin
     (Stream.junk __strm);
     
     (try (backslash __strm)
     with
     | Stream.Failure  -> (raise ( Stream.Error ("") )))
     end | _ -> (failwith "invalid char token"))
let  string ?strict  (s) =
  
  let  buf = (Buffer.create 23) in
  
  let  store = (Buffer.add_char buf) in
  
  let rec  parse ((__strm : _ Stream.t )) =
  
  (match (Stream.peek __strm)
  with
  | Some('\\') ->
    begin
    (Stream.junk __strm);
    
    let  _ =
    
    (try (backslash_in_string ( (strict <> None ) ) store __strm)
    with
    | Stream.Failure  -> (raise ( Stream.Error ("") ))) in (parse __strm)
    end
    | Some(c) ->
      begin
      (Stream.junk __strm);
      
      let  s = __strm in begin
      (store c);
      (parse s)
      end
      end | _ -> (Buffer.contents buf)) in (parse ( (Stream.of_string s) ))