
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
    | `NativeInt of (loc * string)
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
    | `LCons of ('a * meta_list 'a)
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

         
type patt =
  [= nil
  | sid
  | `App of (loc * patt * patt)
  | `Vrn of (loc * string)
  | `Com of (loc * patt * patt)
  | `Sem of (loc * patt * patt)
  | `Tup of (loc * patt )
  | any
  | `Record of (loc * rec_patt)
  | ant
  | literal
      
  | `Alias of (loc * patt * alident)  (* (Node x y as n) *)
  | `Array of (loc * patt) (* [| p |] *)
  | `Label of (loc * alident * patt) (* ~s or ~s:(p) *)
        (* ?s or ?s:(p)  ?s:(p = e) or ?(p = e) *)
  | `PaOlbi of (loc * alident * patt * meta_option exp)
  | `Or of (loc * patt * patt) (* p | p *)
  | `PaRng (* `Range  *)of (loc * patt * patt) (* p .. p *)
  | `Constraint of (loc * patt * ctyp) (* (p : t) *)
  | `ClassPath of (loc * ident) (* #i *)
  | `Lazy of (loc * patt) (* lazy p *)
        (* (module M : ty ) *)      
  | `ModuleUnpack of (loc * auident * meta_option ctyp)]
and rec_patt =
   [= nil
  | `RecBind of (loc * ident * patt)
  | `Sem of (loc  * rec_patt * rec_patt)
  | any
     | ant]  
;
      
