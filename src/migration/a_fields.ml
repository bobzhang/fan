type t =
      { session_id: string;
        time: Time.t;
        user: string;
        credentials: string;
      }
    with fields


(* Field.get Fields.user @@ {session_id = "ghso"; time = Time.now (); user="bob"; credentials="ghos"} *)
;;  
