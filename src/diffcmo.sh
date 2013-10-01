for x in main_annot/*.cmo; do  cmp  $x hot1/$(basename $x) ; done
