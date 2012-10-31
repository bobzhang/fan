open Format;
open FanLexer;

let debug_from_string str =
  let loc = FanLoc.mk "<string>" in
  let stream = from_string loc str  in
  try
    Stream.iter (fun (t,loc) ->
    match t with
    [ `EOI -> begin
        fprintf std_formatter "%a@ %a@." FanToken.print t FanLoc.print loc;
        raise (Stream.Error "end")
    end
    | _ ->  fprintf std_formatter "%a@ %a@."
          FanToken.print t FanLoc.print loc ]) stream
  with
  [  Stream.Error "end" -> ()
  | exn -> begin
      eprintf "@[%s@]@." (Printexc.to_string exn);
      raise exn
  end];

let debug_from_file file =
  let loc = FanLoc.mk file in
  let chan = open_in file in
  let stream = Stream.of_channel  chan in 
  let stream = from_stream loc stream in begin 
  try
    Stream.iter (fun (t,loc) ->
    match t with
    [ `EOI -> begin
        close_in chan;
        raise (Stream.Error "end");
    end 
    | _ ->  fprintf std_formatter "%a@ %a@."
          FanToken.print t FanLoc.print loc]) stream
  with
    [Stream.Error _ -> close_in chan]
  end ;

  

