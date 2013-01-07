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
         "MtFun" "Functor"
         
         ))
;;; a.el ends here
