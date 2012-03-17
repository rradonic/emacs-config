;;; jx-javadoc.el --- elisp functions for java methods

;; Copyright (C) 2001, 2002 Alex Moffat
;; Keywords: java
;; Version: 1.0
;; CVS: $Id: jx-javadoc.el,v 1.1 2002/03/25 03:20:48 alexmoffat Exp $

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
;; Functions that help manage javadoc comments.
;;
;; Installation:
;; 
;; See jx-mode.el
;;
;; Customization:
;; 
;; None
;;
;; Change log:
;;
;; 20020324 ajm Created by moving some functions from jx-methods.el
;;

;; We need the methods code
(require 'jx-methods)

;; We need the utilities code
(require 'jx-util)

(defun jx-get-current-jdoc (start-of-method)
  "Read the jdoc for the method that starts in position
START-OF-METHOD. Normally START-OF-METHOD is found as the first
character in the signature returned by jx-get-method-signature."
  (save-excursion
    (goto-char start-of-method)
    (forward-line -1)
    (if (looking-at "^\\s-*\\*/")
        (progn
          (forward-line -1)
          (while (looking-at "^\\s-*\\*")
            (forward-line -1))
          (if (looking-at "^\\s-*/\\*\\*")
              (let ((data ())
                    (tag 'description)
                    (current-string nil))
                (while (not (looking-at "^\\s-*\\*/"))
                  (cond ((looking-at "^\\s-*\\*\\s-+@\\(\\S-+\\)\\(\\s-+\\(.+\\)\\)$")
                         (setq data (jx-append-assoc data tag current-string))
                         (setq tag (intern (jx-bsnp 1)))
                         (if (match-beginning 3)
                             (setq current-string (jx-bsnp 3))
                           (setq current-string nil)))
                        ((looking-at "^\\s-*\\*\\s-+\\(.+\\)$")
                         (setq current-string (concat current-string
						      (if (not (null current-string))
							  " ")
						      (jx-bsnp 1)))))
                  (forward-line 1))
                (if (not (null current-string))
                    (setq data (jx-append-assoc data tag current-string)))
                data))))))

(defun jx-append-assoc (al k s)
  ""
  (if (assoc k al)
      (let ((l (assoc k al)))
        (setcdr l (cons s (cdr l))))
    (setq al (cons (cons k (list s)) al)))
  al)

(defun jx-insert-jdoc ()
  "Insert a javadoc skeleton in front of the next method after point,
or in front of the current method if point is inside a method header."
  (interactive)
  ;; Supply endOfMatchIncluded flag so that if point is right after
  ;; the { after a method it counts as being in the method and point
  ;; doesn't move. This means you can use jx-forward-method to
  ;; locate a method and then use this to insert the doc for that
  ;; method.
  (if (jx-forward-method t)
      (let* ((sig (jx-get-method-signature))
             (jdoc (jx-get-current-jdoc (caar sig)))
	     (description "<<Description>>")
	     ;; Make this nill so that indent-according-to-mode
	     ;; is run on each line
	     (indent-region-function nil)
	     ;; Set a marker so that we can get back to the end of
	     ;; the method
	     (method-end (set-marker (make-marker) (cdar sig))))
	;; Go to start of the line the method def starts on
	(goto-char (caar sig))
	;; skip any completely blank lines that got included
	;; in the match
	(while (looking-at "^\\s-*$")
	  (forward-line 1))
	;; Add a newline
	(newline)
	;; Go to the start of the line just added
	(forward-line -1)
	(let ((pos (point)))
	  ;; Start of the comment block
	  (insert "/**")
	  (newline)
	  ;; End of comment block
	  (insert "*/")
	  (newline)
	  (forward-line -1)
	  ;; The description goes here
	  (jx-insert-jdoc-line
	   (jx-existing-doc jdoc 'description description))
	  ;; A blank line after the description
	  (insert "*")
	  (newline)
	  ;; Insert any params
	  (let ((params (jx-sig-params sig)))
	    (while (not (null params))
	      (jx-insert-jdoc-line
	       (concat "@param "
		       (jx-existing-doc jdoc 'param
					description (cdar params))))
	      (setq params (cdr params))))
	  ;; A return statement if there is a non void one
	  (let ((ret (jx-sig-return-type sig)))
	    (if (and (not (null ret))
		     (not (equal ret "void")))
		(jx-insert-jdoc-line
		 (concat "@return "
			 (jx-existing-doc jdoc 'return description)))))
	  ;; Any exceptions
	  (let ((ex (jx-sig-exceptions sig)))
	    (while (not (null ex))
	      (jx-insert-jdoc-line
	       (concat "@throws "
		       (jx-existing-doc jdoc 'throws
					description (car ex))))
	      (setq ex (cdr ex))))
	  ;; Add any @see, @since, @serial, or @deprecated tags
	  ;; that were in the original jdoc.
	  (if (not (null jdoc))
	      (progn
		(jx-insert-jdoc-list 'see jdoc)
		(jx-insert-jdoc-list 'since jdoc)
		(jx-insert-jdoc-list 'serial jdoc)
		(jx-insert-jdoc-list 'deprecated jdoc)))
	  ;; Indent and remove tabs
	  (end-of-line)
	  (delete-blank-lines)
	  (goto-char method-end)
	  (indent-region pos (point) nil)
	  (untabify pos (point))
	  (let ((eor method-end))
	    ;; Position point at start of new description for easy
	    ;; typing, if no description then position at start of
	    ;; method def.
	    (goto-char pos)
	    (if (search-forward description eor 1)
		(backward-char (length description))
	      (jx-backward-method)))))))

(defun jx-insert-jdoc-line (line)
  "Insert a line of jdoc, fill it, and put a newline after it."
  (insert "* " line)
  (c-fill-paragraph)
  (newline))

(defun jx-insert-jdoc-list (type jdoc)
  "Insert entries for all of the items of type TYPE. TYPE must be a
symbol whose name is one of the possible javadoc tags."
  (let ((l (and (assoc type jdoc)
		(cdr (assoc type jdoc)))))
    (while (not (null l))
      (jx-insert-jdoc-line
       (concat "@" (symbol-name type) " " (car l)))
      (setq l (cdr l)))))
  
(defun jx-existing-doc (jdoc type default &optional key)
  "Find the existing doc of TYPE with KEY, if none found return
KEY DEFAULT."
  (if (and (not (null jdoc))
	    (assoc type jdoc))
       (if (not (null key))
	   (let ((desc (jx-find-member
			(function (lambda (x e)
				    (string-match (concat "^" e "\\s-") x)))
			key
			(cdr (assoc type jdoc)))))
	     (if (not (null desc))
		 desc
	       (concat key " " default)))
	 (cadr (assoc type jdoc)))
     (if (not (null key))
	 (concat key " " default)
       default)))

(defun jx-javadoc-submenu ()
  "The menu for javadoc"
  '(menu-item "Javadoc" jx-insert-jdoc
	      :key-sequence nil
	      :help "Insert javadoc skeleton for next method in the file."))

(provide 'jx-javadoc)



