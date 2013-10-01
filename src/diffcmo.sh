for x in hot_annot/*.cmo; do  cmp  $x hot_hot/$(basename $x) ; done
