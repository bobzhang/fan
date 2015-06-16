
options
=======

-I

-intf

-impl

-str 

-o

-list

-list_directive

-unsafe

-where

-loc

-v

-plugin investigate https://github.com/janestreet/ocaml_plugin

-loaded-modules
-loaded-parsers
-used-parsers

-dlang

-printer 

-printers


 Options used in bootstrapping
===============================
.. highlight:: sh

::

   public.FAN_PLUGIN_OPTIONS  = $`(mapprefix -plugin, $(FAN_PLUGINS))

::

   ../fan0 -printer o $f.ml > ../cold/$f.ml


.. todo::

   Can plugins change options?
