;;; -*- lexical-binding: t -*-
;;; a.el --- 

;; Copyright 2013 Hongbo Zhang
;;
;; Author: bobzhang1988@vagvlan536.1276.wlan.wireless-pennnet.upenn.edu
;; Version: $Id: a.el,v 0.0 2013/01/05 22:06:39 bobzhang1988 Exp $
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


(setq v
      (hash-table/from-list
       "TXmeta" "Smeta"
       "TXlist" "Slist"
       "TXopt" "Sopt"
       "TXtry" "Stry"
       "TXpeek" "Speek"
       "TXself" "Sself"
       "TXkwd" "Skeyword"
       "TXtok" "Stok"
       "TXnext" "Snext"
       "TXrules" "Srules"
       "TXnterm" "Snterm"
       )
      )

;; (setq v
;;       (hash-table/from-list
;;        "IdApp" "App"
;;        "TyApp" "App"
;;        "PaApp" "App"
;;        "ExApp" "App"
;;        "MeApp" "App"
;;        "ExVrn" "Vrn"
;;        "PaVrn" "Vrn"
;;        )
;;       )

;; \(`\(CgNil\|CgCtr\|CgSem\|CgInh\|CgMth\|CeNil\|CeStr\|CeAnd\|CeEq\|CrNil\|CeSem\|CrCtr\|CrInh\|CrIni\|TyNil\|TyAli\|TyOf\|TySum\|TyCom\|TyAnd\|TySta\|TyPrv\|TyMut\|TyTup\|TyPkg\|PaNil\|PaAli\|PaAny\|TyAny\|PaId\|ExNil\|ExId\|ExArr\|PaArr\|ExSem\|PaSem\|For_loop\|Let_in\|Let_module\|Optional_label\|Override_instance\|String_dot\|Local_type_fun\|MtNil\|MtId\|MtOf\|MtSig\)\)\b
;; (setq v
;;   (hash-table/from-list
;;    "CgNil" "Nil"
;;    "CgCtr" "Eq"
;;    "CgSem" "Sem"
;;    "CgInh" "Inherit"
;;    "CgMth" "Method"
;;    "CeNil" "Nil"
;;    "CeStr" "Obj"
;;    "CeAnd" "And"
;;    "CeEq" "Eq"
;;    "CrNil" "Nil"
;;    "CeSem" "Sem"
;;    "CrCtr" "Eq"
;;    "CrInh" "Inherit"
;;    "CrIni" "Initializer"
;;    "TyNil" "Nil"
;;    "TyAli" "Alias"
;;    "TyOf" "Of"
;;    "TySum" "Sum"
;;    "TyCom" "Com"
;;    "TyAnd" "And"
;;    "TySta" "Sta"
;;    "TyPrv" "Private"
;;    "TyMut" "Mutable"
;;    "TyTup" "Tup"
;;    "TyPkg" "Package"
;;    "PaNil" "Nil"
;;    "PaAli" "Alias"
;;    "PaAny" "Any"
;;    "TyAny" "Any"
;;    "PaId" "Id"
;;    "ExNil" "Nil"
;;    "ExId" "Id"
;;    "ExArr" "Array"
;;    "PaArr" "Array"
;;    "ExSem" "Sem"
;;    "PaSem" "Sem"
;;    "For_loop" "For"
;;    "Let_in" "LetIn"
;;    "Let_module" "LetModule"
;;    "Optional_label" "OptLabl"
;;    "Override_instance" "OvrInst"
;;    "String_dot" "StringDot"
;;    "Local_type_fun" "LocalTypeFun"
;;    "MtNil" "Nil"
;;    "MtId" "Id"
;;    "MtOf" "Of"
;;    "MtSig" "Sig"
;;    )
;;   )


;; (setq
;;  v
;;  (hash-table/from-list
;;  "ExLet" "Let_in"
;;  "ExFor" "For_loop"
;;  "ExLmd" "Let_module"
;;  "ExFun" "Fun"
;;  "ExLab" "Label"
;;  "ExMat" "Match"
;;  "ExNew" "New"
;;  "ExObj" "Obj"
;;  "ExOlb" "Optional_label"
;;  "ExOvr" "Override_instance"
;;  "ExRec" "Record"
;;  "ExSeq" "Sequence"
;;  "ExSnd" "Send"
;;  "ExSte" "String_dot"
;;  "ExTry" "Try"
;;  "ExTyc" "Constraint_exp"
;;  "ExWhi" "While"
;;  "ExOpI" "Let_open"
;;  "ExFUN" "Local_type_fun"
;;  "ExPkg" "Package_expr"
;;  ))

;;; a.el ends here
;;; -*- lexical-binding: t -*-
;;; a.el --- 

;; Copyright 2013 bobzhang
;;
;; Author: bobzhang@bobzhangs-iMac.local
;; Version: $Id: a.el,v 0.0 2013/01/06 15:25:43 bobzhang Exp $
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

(defvar *string-map*
  #s(hash-table
            size 30 data
            ( 3 4)))
(setq '*string-map*
  (make-hash-table "ExLet" "Let"))


(defun string-map-assoc (key &rest)
  (let ((*string-map* #s(hash-table size 30 data ("hgo" 4))))
    (gethash "hgo" *string-map* )))
(let ((e ))
  
  )

(setq v
      (hash-table/from-list
       "CgNil" "Nil"
       "CgCtr" "TypeEq"
       "CgSem" "Sem"
       "CgInh"  "Inherit"
       "CgMth" "Method"
       "CgVal" "Value"
       "CgVir" "Virtual"

       "CeNil" "Nil"
       "CeStr" "Obj"
       "CeTyc" "CEConstraint"
       "CeAnd" "And"
       "CeEq" "ClassExprEq"

       "CrNil" "Nil"
       "CrSem" "Sem"
       "CrCtr" "TypeEq"
       "CrInh" "Inherit"
       "CrIni" "Initializer"
       "CrMth" "Method"
       "CrVal" "Value"
       "CrVir" "Virtual"
       "CrVvr" "VirtualMethod"
         ))
;; (setq v
;;       (hash-table/from-list
;;        "StNil" "Nil"
;;        "StCls" "Class"
;;        "StClt" "ClassType"
;;        "StSem" "Sem"
;;        "StDir" "Directive"
;;        "StExc" "Exception"
;;        "StExt" "External"
;;        "StInc" "Include"
;;        "StMod" "Module"
;;        "StRecMod" "RecModule"
;;        "StMty" "ModuleType"
;;        "StOpn" "Open"
;;        "StTyp" "Type"
;;        "StVal" "Value"
;;          ))
;; (setq v
;;       (hash-table/from-list
;;        "WcTyS" "TypeSubst"
;;        "WcMoS" "ModuleSubst"
;;        "McNil" "Nil"
;;        "McOr" "Or"
;;        "McArr" "Case"
;;        "MeNil" "Nil"
;;        "MeId" "Id"
;;        "MeFun" "Functor"
;;        "MeStr" "Struct"
;;        "MeTyc" "ModuleExprConstraint"
;;        "MePkg" "PackageModule"
;;          ))
;; (setq v
;;       (hash-table/from-list
;;        "WcNil" "Nil"
;;        "WcTyp" "TypeEq"
;;        "WcMod" "ModuleEq"
;;        "WcTys" "TypeSubst"
;;        "WcMos" "ModuleSubst"
;;        "WcAnd" "And"
;;        "BiNil" "Nil"
;;        "BiAnd" "And"
;;        "BiEq" "Bind"
;;        "RbNil" "Nil"
;;        "RbSem" "Sem"
;;        "RbEq" "RecBind"
;;        "MbNil" "Nil"
;;        "MbAnd" "And"
;;        "MbColEq" "ModuleBind"
;;        "MbCol" "ModuleConstraint"
;;          ))


;; (setq v
;;       (hash-table/from-list
;;          "SgNil" "Nil"
;;          "SgCls" "Class"
;;          "SgClt" "ClassType"
;;          "SgSem" "Sem"
;;          "SgDir" "Directive"
;;          "SgExc" "Exception"
;;          "SgExt" "External"
;;          "SgInc" "Include"
;;          "SgMod" "Module"
;;          "SgRecMod" "RecModule"
;;          "SgMty" "ModuleType"
;;          "SgOpn" "Open"
;;          "SgTyp" "Type"
;;          "SgVal" "Value"
;;          ))

;;; a.el ends here
