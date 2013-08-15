;; vl.el --- several vector and list functions

;; Copyright (C) 2013  Tom Koelman
;;
;; Author: Tom Koelman <tkoelman@xs4all.nl>
;; Maintainer: ???
;; Version: 0.1
;; Keywords: data
;; Homepage: http://github.com/tomkoelman/vl
;; Package-Requires: ((loop "1.1"))
;;
;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; This library contains several vector and list functions I missed
;; and find handy.

;;; Code:

(require 'loop)

(defun -list-first-index-and-item-pair-for-pred ( list pred )
  "Returns a (index . item) pair for the first item in LIST for which calling
PRED with the item as an argument returns non-nil."
  (let ((i -1)
        return-item-and-index-pair)
    (loop-for-each item list
      (incf i)
      (when (funcall pred item)
        (setq return-item-and-index-pair (cons i item))
        (loop-break)))
    return-item-and-index-pair))

(defun -list-first-index-for-pred ( list pred )
  "Returns the index of the first item in LIST for which calling
PRED with the item as an argument returns non-nil."
  (car (-list-first-index-and-item-pair-for-pred list pred)))

(defun -list-item-in-list-where-pred-in-other-list ( item-list pred-list pred )
  "Returns the nth item from ITEM-LIST, where n is the index of the first item from PRED-LIST for which PRED holds.
Assumes ITEM-LIST and PRED-LIST are of identical length. Returns
nil when not found."
  (-when-let (index (-list-first-index-for-pred 
                     pred-list
                     pred)))
  (nth index item-list))

(defun -list-item-in-list-where-item-in-other-list ( item-list compare-list compare-item )
  "Returns the nth item from ITEM-LIST, where n is the index of
the first item from COMPARE-LIST which is equal to COMPARE-ITEM.
Assumes ITEM-LIST and PRED-LIST are of identical length. Returns
nil when not found."
  (-list-item-in-list-where-pred-in-other-list 
   item-list 
   compare-list
   (lambda (it) (equal it compare-item))))

(defun -list-guaranteed ( possible-list )
  "Returns (list POSSIBLE-LIST) if POSSIBLE-LIST is atom,
otherwise just returns POSSIBLE-LIST."
  (if (atom possible-list) (list possible-list) possible-list))

(defun -list ( possible-list )
  "Returns a list no matter what."
  (if (atom possible-list)
      (list possible-list)
    (mapcar #'identity possible-list)))

(defun -vector-first-index-and-item-pair-for-pred ( vector pred )
  "Returns a (index . item) pair for the first item in VECTOR for which calling
PRED with the item as an argument returns non-nil."
  (-list-first-index-and-item-pair-for-pred (mapcar 'identity vector) pred))

(defun -vector-first-item-for-pred ( vector pred )
  "Returns the first item in VECTOR for which calling PRED with
the item as an argument returns non-nil."
  (cdr (-vector-first-index-and-item-pair-for-pred vector pred)))

(defun -vector-first-index-for-pred ( vector pred )
  "Returns the index of the first item in VECTOR for which
calling PRED with the item as an argument returns non-nil."
  (car (-vector-first-index-and-item-pair-for-pred vector pred)))

(defmacro cached-form ( cache-var form )
  "Return CACHE-VAR when it is not nil. Otherwise set CACHE-VAR to FORM and return it."
  `(or ,cache-var
       (setq ,cache-var ,form)))

(eval-after-load "lisp-mode"
  '(progn
     (let ((new-keywords '(
                           "-list-first-index-and-item-pair-for-pred"
                           "-list-first-index-for-pred"
                           "-list-guaranteed"
                           "-list"
                           "-list-item-in-list-where-pred-in-other-list"
                           "-list-item-in-list-where-item-in-other-list"
                           "cached-form"
                           "-vector-first-index-and-item-pair-for-pred"
                           "-vector-first-item-for-pred"
                           "-vector-first-index-for-pred"
                           )))
       (font-lock-add-keywords 'emacs-lisp-mode `((,(concat "(\\s-*" (regexp-opt new-keywords 'paren) "\\>")
                                                   1 font-lock-keyword-face)) 'append))
     (--each (buffer-list)
       (with-current-buffer it
         (when (and (eq major-mode 'emacs-lisp-mode)
                    (boundp 'font-lock-mode)
                    font-lock-mode)
           (font-lock-refresh-defaults))))))

(eval-after-load "lisp-mode"
  '(progn
     (let ((new-keywords '(
                           "loop-while"
                           "loop-do-while"
                           "loop-until"
                           "loop-for-each"
                           "loop-break"
                           "loop-continue"
                           )))
       (font-lock-add-keywords 'emacs-lisp-mode `((,(concat "(\\s-*" (regexp-opt new-keywords 'paren) "\\>")
                                                   1 font-lock-keyword-face)) 'append))
     (--each (buffer-list)
       (with-current-buffer it
         (when (and (eq major-mode 'emacs-lisp-mode)
                    (boundp 'font-lock-mode)
                    font-lock-mode)
           (font-lock-refresh-defaults))))))

(provide 'vl)
;;; vl.el ends here
