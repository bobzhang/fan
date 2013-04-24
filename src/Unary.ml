
    type loc = FanLoc.t;
    type ant =
        [= `Ant of (loc * FanUtil.anti_cxt)];

    type nil = [= `Nil of loc];
    type ant_nil = [= ant|nil];

    type literal =
    [= `Chr of (loc * string)
    | `Int of (loc * string)
    | `Int32 of (loc * string)
    | `Int64 of (loc * string)
    | `Flo of (loc * string)
    | `Nativeint of (loc * string)
    | `Str of (loc * string)];   

    type rec_flag =
    [= `Recursive of loc 
    | `ReNil of loc 
    | ant];

   type direction_flag =
    [= `To of loc
    | `Downto of loc
    | ant ];

  type mutable_flag =
    [= `Mutable of loc 
    | `MuNil of loc 
    | ant ];

   type private_flag =
    [= `Private of loc 
    | `PrNil of loc 
    | ant ];
   type virtual_flag =
    [= `Virtual of loc 
    | `ViNil of loc 
    | ant ];
   type override_flag =
    [= `Override of loc 
    | `OvNil of loc 
    | ant ];
   type row_var_flag =
    [= `RowVar of loc 
    | `RvNil of loc 
    | ant ];

   type position_flag =
    [= `Positive of loc
    | `Negative of loc
    | `Normal of loc
    |ant];

   type meta_bool =
    [=`True of loc
    |`False of loc
    | ant];

   type 'a meta_option  =
    [= `None 
    |  `Some of 'a
    | ant];
   type 'a meta_list  =
    [= `LNil 
    | `LCons of ('a * 'a meta_list )
    | ant];

   type alident =
    [= `Lid of (loc * string)
    | ant];
   type auident =
    [= `Uid of (loc * string)
    | ant];

   type aident =
    [= alident
    | auident ];

   type astring =
    [= `C of (loc * string)
    | ant ];

   type uident =
    [= `Dot of (loc * uident * uident)
    | `App of (loc * uident * uident)
    | auident];


   (* type uident = *)
   (*  [= `Dot of (loc * uident * uident) *)
   (*  | `App of (loc * uident * uident) *)
   (*  ];    *)
   (* {:ident|A.B.C.d|}
      `Dot
      (`Uid "A"
        `Dot (`Uid "B")
           `Dot("C",`Lid "d"))
    *)
   type ident =
    [= `Dot of (loc * ident * ident) (* i . i *)
    | `App of (loc * ident * ident) (* i i *)
    | alident
    | auident];

   (* type dopath = *)
   (*  [= `Dot of (loc * dopath * dopath) *)
   (*  | auident ] ; *)

   (* A.B.C *)
   type dupath =
    [= `Dot of (loc * dupath * dupath)
    | auident];

   type dlpath=
    [= `Dot of (loc * dupath * alident)
    | alident];



   type sid = [= `Id of (loc * ident)];

   type any = [= `Any of loc];

         
type pat =
  [= nil
  | sid
  | `App of (loc * pat * pat)
  | `Vrn of (loc * string)
  | `Com of (loc * pat * pat)
  | `Sem of (loc * pat * pat)
  | `Par of (loc * pat )
  | any
  | `Record of (loc * rec_pat)
  | ant
  | literal
      
  | `Alias of (loc * pat * alident)  (* (Node x y as n) *)
  | `Array of (loc * pat) (* [| p |] *)
  | `Label of (loc * alident * pat) (* ~s or ~s:(p) *)
        (* ?s or ?s:(p)  ?s:(p = e) or ?(p = e) *)
  | `PaOlbi of (loc * alident * pat * meta_option exp)
  | `Bar of (loc * pat * pat) (* p | p *)
  | `PaRng (* `Range  *)of (loc * pat * pat) (* p .. p *)
  | `Constraint of (loc * pat * ctyp) (* (p : t) *)
  | `ClassPath of (loc * ident) (* #i *)
  | `Lazy of (loc * pat) (* lazy p *)
        (* (module M : ty ) *)      
  | `ModuleUnpack of (loc * auident * meta_option ctyp)]
and rec_pat =
  [= nil
  | `RecBind of (loc * ident * pat)
  | `Sem of (loc  * rec_pat * rec_pat)
  | any
  | ant];
      
