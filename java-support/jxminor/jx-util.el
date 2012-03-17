;;; jx-util.el --- elisp utility functions for jxminor

;; Copyright (C) 2001, 2002 Alex Moffat
;; Keywords: java
;; Version: 1.0
;; CVS: $Id: jx-util.el,v 1.4 2002/03/25 03:28:06 alexmoffat Exp $

;;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Purpose
;;
;; Utility functions that are not specific to any particular set of
;; jxminor functions.
;;
;; Installation:
;;
;; Make the file accessible somewhere on your load path.
;;
;; Customization:
;;
;; None
;;
;; Change log:
;;
;; 20020308 ajm Added jx-find-first
;;
;; 20020324 ajm Moved provide to bottom of file
;;

(defun jx-foldr1 (f l)
  "foldr1 in elisp"
  (if (null (cdr l))
      (car l)
    (funcall f (car l) (jx-foldr1 f (cdr l)))))

(defun jx-find-member (f e l)
  "Find the first member of the list L that the function F returns non
nil for when called with that member and the item E"
  (let ((found nil))
    (while (not (or found
                    (null l)))
      (if (funcall f (car l) e)
          (setq found (car l)))
      (setq l (cdr l)))
    found))

(defun jx-filtered-map (f l)
  "Return a list of the non null values returned by the function F
when applied to the members of the list L."
  (let ((nl '()))
    (while (not (null l))
      (let ((v (funcall f (car l))))
        (if (not (null v))
            (setq nl (cons v nl)))
        (setq l (cdr l))))
    (nreverse nl)))

(defun jx-filter-unique (f l)
  "Return a list of the unique members of the list L where the
function F, called with two members of the list, returns non null if
it considers them equal."
  (let ((nl '()))
    (while (not (null l))
      (if (not (jx-find-member f (car l) nl))
          (setq nl (cons (car l) nl)))
      (setq l (cdr l)))
    (nreverse nl)))

(defun jx-separate (l s)
  "Take a list L and a separator S and return a list with the elements
of the original list separated by S. The original list must have at
least one element."
  (if (null (cdr l))
      l
    (cons (car l) (cons s (jx-separate (cdr l) s)))))

(defun jx-find-first (f l)
  "Apply function F to each element in list L in turn returning the
first non nil result"
  (let ((found nil))
    (while (and (not found)
                (not (null l)))
      (setq found (funcall f (car l)))
      (setq l (cdr l)))
    found))

(defmacro jx-bsnp (i)
  "buffer-substring-no-properties is long"
  `(buffer-substring-no-properties (match-beginning ,i)
				   (match-end ,i)))

(provide 'jx-util)