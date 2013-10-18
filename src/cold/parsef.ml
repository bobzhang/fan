open FAst
let antiquot_exp = Fgram.eoi_entry Syntaxf.exp
let antiquot_pat = Fgram.eoi_entry Syntaxf.pat
let antiquot_ident = Fgram.eoi_entry Syntaxf.ident
let exp loc str = Fgram.parse_string antiquot_exp ~loc str
let pat loc str = Fgram.parse_string antiquot_pat ~loc str
let ident loc str = Fgram.parse_string antiquot_ident ~loc str
let anti_filter = Ant.antiquot_expander ~parse_exp:exp ~parse_pat:pat
let exp_filter (x : ep) = anti_filter#exp (x :>exp)
let pat_filter (x : ep) = anti_filter#pat (x :>pat)
let anti_filter_n = AntN.antiquot_expander ~parse_exp:exp ~parse_pat:pat
let exp_filter_n (x : ep) = anti_filter_n#exp (x :>exp)
let pat_filter_n (x : ep) = anti_filter_n#pat (x :>pat)