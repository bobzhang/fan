open Syntaxf
open Astfn
open Util
let dispatch_tbl: (string,ctyp -> exp) Hashtbl.t = Hashtbl.create 31
let current_name: string option ref = ref None
let t = Gramf.mk "t"
let _ =
  Gramf.extend_single
    ({
       entry = (t : 't Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols = [Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                 annot =
                   "match !current_name with\n| None  -> failwith \"No attribute attached to dsl t\"\n| Some n ->\n    let n = String.capitalize n in\n    ((try\n        let f = Hashtbl.find dispatch_tbl n in\n        fun ()  -> Fill.exp _loc (f (Strip.ctyp x))\n      with\n      | Not_found  -> (fun ()  -> failwithf \"%s not registered with t\" n)))\n      ()\n";
                 fn =
                   (Gramf.mk_action
                      (fun (x : 'ctyp)  (_loc : Locf.t)  ->
                         (match !current_name with
                          | None  ->
                              failwith "No attribute attached to dsl t"
                          | Some n ->
                              let n = String.capitalize n in
                              ((try
                                  let f = Hashtbl.find dispatch_tbl n in
                                  fun ()  -> Fill.exp _loc (f (Strip.ctyp x))
                                with
                                | Not_found  ->
                                    (fun ()  ->
                                       failwithf "%s not registered with t" n)))
                                () : 't ) : 'ctyp -> Locf.t -> 't ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let _ =
  let d = Ns.lang in
  let parser = Ast_quotation.make_parser ~lexer:Lex_fan.from_stream t in
  let f loc meta content =
    Ref.protect current_name meta (fun _  -> parser loc meta content) in
  let f2 _loc meta content =
    (`StExp (_loc, (f _loc meta content :>Astf.exp)) :>Astf.stru) in
  Ast_quotation.add { domain = d; name = "t" } Dyn_tag.exp f;
  Ast_quotation.add { domain = d; name = "t" } Dyn_tag.stru f2
