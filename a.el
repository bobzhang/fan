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





(setq
 v
 (hash-table/from-list
 "ExLet" "Let_in"
 "ExFor" "For_loop"
 "ExLmd" "Let_module"
 "ExFun" "Fun"
 "ExLab" "Label"
 "ExMat" "Match"
 "ExNew" "New"
 "ExObj" "Obj"
 "ExOlb" "Optional_label"
 "ExOvr" "Override_instance"
 "ExRec" "Record"
 "ExSeq" "Sequence"
 "ExSnd" "Send"
 "ExSte" "String_dot"
 "ExTry" "Try"
 "ExTyc" "Constraint_exp"
 "ExWhi" "While"
 "ExOpI" "Let_open"
 "ExFUN" "Local_type_fun"
 "ExPkg" "Package_expr"
 ))

;;; a.el ends here
