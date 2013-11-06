let eoi_entry entry =
  let g = Gramf.gram_of_entry entry in
  let entry_eoi = Gramf.mk_dynamic g ((Gramf.name entry) ^ "_eoi") in
  Gramf.extend_single (entry_eoi : 'entry_eoi Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (entry : 'entry Gramf.t ));
           `Token
             (((function | `EOI _ -> true | _ -> false)),
               ({ tag = `EOI; word = Empty } : Tokenf.descr ), "`EOI")],
            ("x\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(x : 'entry)  (_loc : Locf.t)  ->
                    (x : 'entry_eoi )))))]) : Gramf.olevel ));
  entry_eoi
