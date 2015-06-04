=====================
Fan's quosi-quotation
=====================


-  Fan's quotation system

Fan's metaprogramming has a `Quotation system <http://brion.inria.fr/gallium/index.php/Quotation>`_ similar to `Camlp4`_.

The differences lie in serveral following aspects

- Concrete Syntax

  - For quotation, Fan uses :code:`%quot@loc{}`, 
    while Camlp4 uses :code:`<:quot@loc< >>`

  - For antiquotation, Fan uses a single ``$`` or ``${..}``, while
    Camlp4 uses ``$$``

  - Fan supports nested quasiquotation, while Camlp4 does not. For example,
    the following quotation is legal  in Fan.

    .. code-block:: ocaml
  
       %exp{ %exp{  $($x) } }

    Which is simliar to Common Lisp style macros.
    
    .. code-block:: lisp

       ``(,,x)       


 ..
    The definition of Fan's abstract syntax is
     available here file:fansrc/fAst.mli.html

- Abstract Syntax Fan adopts polymorphic variants
  for encoding abstract syntax, there are several major benefits

    - subtyping, for user who wants to reuse part of Fan, the
      subtyping provides much more refined types
    - constructor overloading, there's no need to add a prefix for each type
    - polymorphisim, for example, in Fan, to get a location of an ast
      node, the user only need to write ``loc_of``, without writing
      ``loc_of_exp``, ``loc_of_pat``, *etc.* Other ast processing
      libraries are also more generic compared with Camlp4.
    - unqualified, easier to use.
      
  The disadvantage:

    Error message is sometimes a problem, however, in Fan, the
    default quasiquotation will add type annotation automatically for
    the user, we have an optional quasiquotation without type
    annotations for advanced user.

    For example:

      .. code-block:: ocaml

         (** with annotations *)
         %exp{ 3 }
         (** after expansion *)
         (`Int (_loc, "3") : FAst.exp )

         (** without annotations *)
         %exp'{3}
         (** after expansion *)
         `Int (_loc, "3")


- Location handling 
  
  From the user's point of view, Fan has two
  abstract syntax representations, with or without locations. They
  have exactly the same semantics except handling locations. The
  abstract syntax without location is derived from abstract syntax
  with location.  For example, the definition of ``literal`` are as
  follows:

  .. code-block:: ocaml

      (** literal with locations [FAst]*)
      type literal =
        [ `Chr of (loc * string)
        | `Int of (loc * string)
        | `Int32 of (loc * string)
        | `Int64 of (loc * string)
        | `Flo of (loc * string)
        | `Nativeint of (loc * string)
        | `Str of (loc * string)]   
      
      
      (** literal without locations [FAstN]*)
      type literal =
        [ `Chr of string
        | `Int of  string
        | `Int32 of  string
        | `Int64 of string
        | `Flo of string
        | `Nativeint of string
        | `Str of string]      

  For ast without locations, the naming convention is a ``N`` postfix.
  Programming abstract syntax without caring about locations is way
  more easier. Location is important for debugging and meaningful
  error message, however, some scenarios, for example, code
  generation, don't need precise location.
  
  There are serveral helpful functions for location handling

    - ``loc_of``

       It would fetch location from any type in module FAst

    - ``Objs.strip_[type]``
       
       For example, Objs.strip_ctyp would strip the location for
       ctyp, their type signature are as follows: 

       .. code-block:: ocaml

         utop $ Objs.strip_case;;
         - : FAst.case -> FAstN.case = <fun>
         utop $ Objs.strip_exp;;
         - : FAst.exp -> FAstN.exp = <fun>   

    - ``FanAstN.fill_[type]``

       It's opposite to strip

       .. code-block:: ocaml

         utop $ FanAstN.fill_exp;;
         - : FLoc.t -> FAstN.exp -> FAst.exp = <fun>
         utop $ FanAstN.fill_case;;
         - : FLoc.t -> FAstN.case -> FAst.case = <fun>          

       
    There is also a suite of quasiquotation for ast without locations.

    .. code-block::
      
      (** with annotations, [-] means minus locations *)
      %exp-{ 3}
      (** after expansion *)
      (`Int  "3" : FAstN.exp )
      
      (** without annotations *)
      %exp-'{ 3 }
      (** after expansion *)
      `Int  "3"


.. include:: ./link_names.txt
..
   (see [[file:fansrc/fAst.mli.html][fAst]])


..
   ([[https://github.com/bobzhang/ftop][ftop]] would help!)       
