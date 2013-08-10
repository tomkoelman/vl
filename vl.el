(require 'loop)

(defun -list-first-index-and-item-pair-for-pred ( list pred )
  (let ((i -1)
        return-item-and-index-pair)
    (loop-for-each item list
      (incf i)
      (when (funcall pred item)
        (setq return-item-and-index-pair (cons i item))
        (loop-break)))
    return-item-and-index-pair))

(defun -list-first-index-for-pred ( list pred )
  (car (-list-first-index-and-item-pair-for-pred list pred)))

(defun -list-item-in-list-where-pred-in-other-list ( item-list pred-list pred )
  "Returns the nth item from ITEM-LIST, where n is the index of the first item from PRED-LIST for which PRED holds.
Assumes ITEM-LIST and PRED-LIST are of identical length. Returns nil when not found."
  (-when-let (index (-list-first-index-for-pred 
                     pred-list
                     pred)))
  (nth index item-list))

(defun -list-item-in-list-where-item-in-other-list ( item-list compare-list compare-item )
  "Returns the nth item from ITEM-LIST, where n is the index of the first item from COMPARE-LIST which is equal
to COMPARE-ITEM. Assumes ITEM-LIST and PRED-LIST are of identical length. Returns nil when not found."
  (-list-item-in-list-where-pred-in-other-list 
   item-list 
   compare-list
   (lambda (it) (equal it compare-item))))

(defun -list-guaranteed ( possible-list )
  (if (atom possible-list) (list possible-list) possible-list))

(defun -vector-first-index-and-item-pair-for-pred ( vector pred )
  (-list-first-index-and-item-pair-for-pred (mapcar 'identity vector) pred))

(defun -vector-first-item-for-pred ( vector pred )
  (cdr (-vector-first-index-and-item-pair-for-pred vector pred)))

(defun -vector-first-item-index-for-pred ( vector pred )
  (car (-vector-first-index-and-item-pair-for-pred vector pred)))

(eval-after-load "lisp-mode"
  '(progn
     (let ((new-keywords '(
                           "-list-first-index-and-item-pair-for-pred"
                           "-list-first-index-for-pred"
                           "-list-guaranteed"
                           "-list-item-in-list-where-pred-in-other-list"
                           "-list-item-in-list-where-item-in-other-list"
                           "-vector-first-index-and-item-pair-for-pred"
                           "-vector-first-item-for-pred"
                           "-vector-first-item-index-for-pred"
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
