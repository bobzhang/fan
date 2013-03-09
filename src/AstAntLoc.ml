

{:fans| keep off; derive(WithAntLoc) ;|};

type loc = FanLoc.t;
type ant = [= `Ant of (loc * FanUtil.anti_cxt)];

{:ocaml| {:include| "src/RawAst.ml"|};|};
