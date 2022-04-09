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

(defmacro cached-form ( cache-var form )
  "Return CACHE-VAR when it is not nil. Otherwise set CACHE-VAR to FORM and return it."
  `(or ,cache-var
       (setq ,cache-var ,form)))

(provide 'vl)
;;; vl.el ends here
