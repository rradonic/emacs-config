;;; jx-jump-to.el --- Tries to jump to the source code for a highlighted class

;; Copyright (C) 2001, 2002 Alex Moffat
;; Keywords: java
;; Version: 1.0
;; CVS: $Id: jx-jump-to.el,v 1.3 2002/11/02 01:41:59 alexmoffat Exp $

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
;; Assumes the text in the region is the name of a class. Tries to jump
;; to the source code for the class.
;;
;; Installation:
;; 
;; See the file jx-mode.el
;;
;; Customization:
;; 
;; None yet.
;;
;; Change log:
;;
;; 20021021 ajm Initial version.
;;
;; 20021101 ajm Corrected bug where leading / on file names causing
;;              find not to work.
;;              Corrected reference to free variable.

(require 'jx-imports)

(require 'jx-util)

(defun jx-jump (s e)
  "Try to jump to source file for the class named in the region."
  (interactive "r")
  (let ((text (buffer-substring-no-properties s e)))
    (save-excursion
      (let ((import-region (jx-get-import-region)))
        (goto-char (car import-region))
        (if (re-search-forward (concat "^"
                                       jx-import-regexp-prefix
                                       text
                                       "\\);")
                               (cdr import-region) t)
            (let ((f (jx-find-file-for-import (jx-bsnp 1))))
              (if f
                  (jx-try-to-find-file f)))
          (let ((f (jx-find-file-from-package-import text import-region)))
            (if f
                (jx-try-to-find-file f)
              (if buffer-file-name
                  (jx-try-to-find-file (concat (file-name-directory buffer-file-name)
                                               "/"
                                               text
                                               ".java"))
                (message "This buffer has no file")))))))))

(defun jx-find-file-for-import (import)
  "Try to find the file containing the source code for IMPORT"
  (let* ((imported-class (split-string import "\\."))
         (this-class (split-string (buffer-file-name) "/"))
         (match-position (jx-find-longest-match this-class imported-class)))
    (if (> match-position 0)
        (let ((f (concat
		  (if (equal (substring (buffer-file-name) 0 1) "/")
		      "/")
                  (mapconcat (lambda (x) x)
                             (append
                              (reverse (nthcdr (- (length this-class) match-position)
                                               (reverse this-class)))
                              imported-class)
                             "/")
                  ".java")))
          (if (file-exists-p f)
              f
            nil))
      nil)))

(defun jx-list-possible-packages-for-class (class import-region)
  "Return a list of all of the packages ending in .* with the .*
replaced by the name of the class being searched for."
  (let ((package-import-regexp (concat "^"
                                       jx-import-regexp-prefix
                                       "\\*"
                                       "\\);"))
        (package-list '()))
    (goto-char (car import-region))
    (while (re-search-forward package-import-regexp (cdr import-region) t)
      (setq package-list (cons (concat (substring (jx-bsnp 1) 0 -2) "." class) package-list)))
    package-list))

(defun jx-find-file-from-package-import (class import-region)
  ""
  (let ((l (jx-list-possible-packages-for-class class import-region))
        (f nil))
    (while (and (not f)
                (car l))
      (setq f (jx-find-file-for-import (car l)))
      (setq l (cdr l)))
    f))

(defun jx-try-to-find-file (f)
  ""
  (if (file-exists-p f)
      (find-file-other-window f)
    (message "Guessed file %s doesn't exist" f)))
  
(defun jx-find-longest-match (list-to-search list-to-look-for)
  "Return the starting position of the sublist in LIST-TO-SEARCH that
matches most of LIST-TO-LOOK-FOR starting from the beginning of
LIST-TO-LOOK-FOR."
  (let* ((match-length 0)
         (i 0)
         (start-i 0)
         (max-k 0)
         (look-for-limit (length list-to-look-for))
         (search-limit (length list-to-search)))
    (while (< i search-limit)
      (let ((j i)
            (k 0)
            (limit (if (> look-for-limit (- search-limit i))
                       (- search-limit i)
                     look-for-limit)))
        (while (and (< k limit)
                    (equal (nth j list-to-search) (nth k list-to-look-for)))
          (setq j (+ j 1))
          (setq k (+ k 1)))
        (if (> k max-k)
            (progn
              (setq max-k k)
              (setq start-i i))))
      (setq i (+ i 1)))
    start-i))

(defun jx-jump-to-submenu ()
  "The menu for jumping"
  '(menu-item "JumpTo" jx-jump
	      :key-sequence nil
	      :help "Try to jump to the source code for the class selected"))

(provide 'jx-jump-to)