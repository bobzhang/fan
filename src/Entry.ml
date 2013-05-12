
open LibUtil
open Format
open Structure
open Tools
open FanToken

type 'a t  =entry

let name e = e.ename

let print ppf e = fprintf ppf "%a@\n" Print.text#entry e
let dump ppf e = fprintf ppf "%a@\n" Print.dump#entry e


let trace_parser = ref false

let mk_dynamic g n ={
  egram = g;
  ename = n;
  estart = empty_entry n;
  econtinue _ _ _ = (fun _ -> raise XStream.Failure);
  edesc = Dlevels [] ;
  freezed = false;     
}

(* [estart] The main entrance to consume the parser  *)  
let action_parse entry (ts: stream) : Action.t =
  try begin
    let p =
      if !trace_parser then
        Format.fprintf
      else Format.ifprintf ;
    p Format.err_formatter "@[<4>%s@ " entry.ename ;
    let res = entry.estart 0 ts ;
    p Format.err_formatter "@]@." ;
    res
  end
  with
  | XStream.Failure ->
        FanLoc.raise (get_cur_loc ts)
          (XStream.Error ("illegal begin of " ^ entry.ename))
  | FanLoc.Exc_located (_, _) as exc -> begin
      eprintf "%s@." (Printexc.to_string exc);
      raise exc
  end
  | exc -> begin
      eprintf "%s@." (Printexc.to_string exc);
      FanLoc.raise (get_prev_loc ts) exc
  end

(* stream parser is not extensible *)  
let of_parser g n (p : stream -> 'a)   =
  let f ts = Action.mk (p ts) in {
  egram = g;
  ename = n;
  estart _ = f;
  econtinue _ _ _ = fun _ -> raise XStream.Failure;
  edesc = Dparser f;
  freezed = true (* false *);    
}

let setup_parser e (p : stream -> 'a) =
  let f ts = Action.mk (p ts) in begin
    e.estart <- fun _ -> f;
    e.econtinue <- fun _ _ _ -> fun _ -> raise XStream.Failure;
    e.edesc <- Dparser f
  end

let clear e = begin 
  e.estart <- fun _ -> fun _ -> raise XStream.Failure;
  e.econtinue <- fun _ _ _ -> fun _-> raise XStream.Failure;
  e.edesc <- Dlevels []
end

let obj x = x
let repr x = x
