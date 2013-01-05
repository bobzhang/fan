;;; -*- lexical-binding: t -*-
;;; a.el --- 

;; Copyright 2013 Hongbo Zhang
;;
;; Author: bobzhang1988@vagvlan536.0838.wlan.wireless-pennnet.upenn.edu
;; Version: $Id: a.el,v 0.0 2013/01/03 21:24:43 bobzhang1988 Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'a)

;;; Code:

(provide 'a)
(eval-when-compile
  (require 'cl))



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################





\([^`]\)\b\(BTrue\|BFalse\|Ant\|ReRecursive\|ReNil\|Ant\|DiTo\|DiDownto\|Ant\|MuMutable\|MuNil\|Ant\|PrPrivate\|PrNil\|Ant\|ViVirtual\|ViNil\|Ant\|OvOverride\|OvNil\|Ant\|RvRowVar\|RvNil\|Ant\|ONone\|OSome\|Ant\|LNil\|LCons\|Ant\|IdAcc\|IdApp\|IdLid\|IdUid\|Ant\|TyNil\|TyAli\|TyAny\|TyApp\|TyArr\|TyCls\|TyLab\|TyId\|TyMan\|TyDcl\|TyObj\|TyOlb\|TyPol\|TyTypePol\|TyQuo\|TyQuP\|TyQuM\|TyAnP\|TyAnM\|TyVrn\|TyRec\|TyCol\|TySem\|TyCom\|TySum\|TyOf\|TyAnd\|TyOr\|TyPrv\|TyMut\|TyTup\|TySta\|TyVrnEq\|TyVrnSup\|TyVrnInf\|TyVrnInfSup\|TyAmp\|TyOfAmp\|TyPkg\|Ant\|PaNil\|PaId\|PaAli\|Ant\|PaAny\|PaApp\|PaArr\|PaCom\|PaSem\|PaChr\|PaInt\|PaInt32\|PaInt64\|PaNativeInt\|PaFlo\|PaLab\|PaOlb\|PaOlbi\|PaOrp\|PaRng\|PaRec\|PaEq\|PaStr\|PaTup\|PaTyc\|PaTyp\|PaVrn\|PaLaz\|PaMod\|ExNil\|ExId\|ExAcc\|Ant\|ExApp\|ExAre\|ExArr\|ExSem\|ExAsf\|ExAsr\|ExAss\|ExChr\|ExCoe\|ExFloy\|ExFor\|ExFun\|ExIfe\|ExInt\|ExInt32\|ExInt64\|ExNativeInt\|ExLab\|ExLaz\|ExLet\|ExLmd\|ExMat\|ExNew\|ExObj\|ExOlb\|ExOvr\|ExRec\|ExSeq\|ExSnd\|ExSte\|ExStr\|ExTry\|ExTup\|ExCom\|ExTyc\|ExVrn\|ExWhi\|ExOpI\|ExFUN\|ExPkg\|MtNil\|MtId\|MtFun\|MtQuo\|MtSig\|MtWit\|MtOf\|Ant\|SgNil\|SgCls\|SgClt\|SgSem\|SgDir\|SgExc\|SgExt\|SgInc\|SgMod\|SgRecMod\|SgMty\|SgOpn\|SgTyp\|SgVal\|Ant\|WcNil\|WcTyp\|WcMod\|WcTyS\|WcMoS\|WcAnd\|Ant\|BiNil\|BiAnd\|BiEq\|Ant\|RbNil\|RbSem\|RbEq\|Ant\|MbNil\|MbAnd\|MbColEq\|MbCol\|Ant\|McNil\|McOr\|McArr\|Ant\|MeNil\|MeId\|MeApp\|MeFun\|MeStr\|MeTyc\|MePkg\|Ant\|StNil\|StCls\|StClt\|StSem\|StDir\|StExc\|StExp\|StExt\|StInc\|StMod\|StRecMod\|StMty\|StOpn\|StTyp\|StVal\|Ant\|CtNil\|CtCon\|CtFun\|CtSig\|CtAnd\|CtCol\|CtEq\|Ant\|CgNil\|CgCtr\|CgSem\|CgInh\|CgMth\|CgVal\|CgVir\|Ant\|CeNil\|CeApp\|CeCon\|CeFun\|CeLet\|CeStr\|CeTyc\|CeAnd\|CeEq\|Ant\|CrNil\|CrSem\|CrCtr\|CrInh\|CrIni\|CrMth\|CrVal\|CrVir\|CrVvr\|Ant\)\b

;;; a.el ends here
\(BTrue\|BFalse\|Ant\|ReRecursive\|ReNil\|Ant\|DiTo\|DiDownTo\|Ant\|MuMutable\|MuNil\|Ant\|PrPrivate\|PrNil\|Ant\|ViVirtual\|ViNil\|Ant\|OvOverride\|OvNil\|Ant\|RvRowVar\|RvNil\|Ant\|ONone\|OSome\|Ant\|LNil\|LCons\|Ant\|Id\w+\|Ty\w+\|Pa\w+\|Ex\w+\|Mt\w+\|Sg\w+\|Wc\w+\|Bi\w+\|Rb\w+\|\Mb\w+\|Mc\w+\|Me\w+\|St\w+\|Ct\w+\|Cg\w+\|Ce\w+\|Cr\w+\)\b