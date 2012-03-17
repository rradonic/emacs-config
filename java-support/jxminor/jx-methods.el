;;; jx-methods.el --- elisp functions for java methods

;; Copyright (C) 2001, 2002 Alex Moffat
;; Keywords: java
;; Version: 1.0
;; CVS: $Id: jx-methods.el,v 1.10 2002/05/21 00:49:07 alexmoffat Exp $

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
;; Functions that help work with java methods.
;; Including -
;;
;; 1. Navigating forwards and backwards method by method.
;;
;; 2. Incremental search for method definitions.
;;
;; 3. Extract the signature for a message.
;;
;; Installation:
;; 
;; See jx-mode.el
;;
;; Customization:
;; 
;; None
;;
;; Acknowledgements
;;
;; I borrowed the initial version of regular expression framework and
;; the structure returned by java-get-method-signature from the
;; qflib.el package at http://www.qfs.de/en/projects/qflib/index.html
;;
;; Change log:
;;
;; 20020218 ajm Renamed java-find-next-method and
;;              java-find-prior-method to java-forward-method and
;;              java-backward-method as the find names would be better
;;              used for finding.
;;
;; 20020219 ajm Renamed to use jx- prefix instead of java- and checked
;;              into CVS. endOfMatchIncluded not the correct style so
;;              changed to end-of-match-included.
;;
;; 20020220 ajm Changed look function in jx-find-next-method to
;;              iterate rather than recurse.
;;
;; 20020224 ajm Moved utility function to jx-util.
;;
;; 20020227 ajm Extra newline in jx-insert-method-log makes output look
;;              nicer.
;;
;; 20020228 ajm Major changes to the search code. Removed the find
;;              methods, added search methods in their place and
;;              arranged for them to be called from incremental
;;              search. This provides incremental forward and backward
;;              search by method name.
;;
;; 20020301 ajm search-backward was not being correctly restored.
;;
;; 20020319 ajm Moved jx-insert-method-log to jx-log4j.
;;

;; We need the utility code
(require 'jx-util)

(defun jx-token-string-to-list (s r f)
  "Take a string S, repeatedly match using regular expression R, for
each match call function F passing it the string, and return a list of
the results returned by F."
  (let ((pos 0)
        (l '()))
    (while (string-match r s pos)
      (setq l (cons (funcall f s) l))
      (setq pos (match-end 0)))
    (nreverse l)))

(defun jx-buffer-match-to-list (i r f)
  "If there is a Ith match then call jx-token-string-to-list with
the buffer-substring for that match and return the results, otherwise
return '()"
  (if (match-beginning i)
      (save-match-data
        (jx-token-string-to-list (jx-bsnp i) r f))
    '()))

(defconst jx-method-modifiers
  '("public" "protected" "private" 
    "static" "final" "abstract" "synchronized" "native")
  "List of possible java qualifiers that can be applied to methods.")

(defconst jx-method-regexp
  (let* ((mod (concat
               "\\("
               (jx-foldr1 (function (lambda (a b) 
				      (concat a "\\|" b)))
			  jx-method-modifiers)
               "\\)"))
         (word "\\(\\w\\|_\\)+")
         (type "\\(\\w\\|\\.\\)+[][ \t]*")
         (ws "[ \t\r\n]")
         (ws* (concat ws "*"))
         (ws+ (concat ws "+"))
         (final (concat "\\(final" ws+ "\\)?")))
    (concat
     "^[ \t]*"          ; possible whitespace at the start of the line
     "\\(\\(" mod "[ \t]+\\)*\\)"       ; method modifiers (1)
     "\\(\\(" type "\\)" ws+ "\\)?"     ; return type, optional to
					; match constructors (5)
     "\\(" word "\\)" ws* "("           ; method name (7)
     "\\("                              ; optional arguments (9)
     ws* final type ws+ word            ; first argument
     "\\(" ws* "," ws* final type ws+ word "\\)*" ; rest of arguments
     "\\)?"                             ; end of optional arguments
     ws* ")"                            ; closing paren after args
     "\\(" ws* "throws" ws+             ; optional throws (18)
     "\\(" type                         ; first exception
     "\\(" ws* "," ws* type "\\)*\\)"   ; other exceptions
     "\\)?" ws* "{"))
  "Regular expression that matches the beginning of a java method.
It has the following limitations:
- no comments inside the method declaration
- only supports `Obj[] args', not `Obj args[]'")

(defconst jx-reindex-modifiers 1)
(defconst jx-reindex-return 5)
(defconst jx-reindex-name 7)
(defconst jx-reindex-args 9)
(defconst jx-reindex-throws 18)

(defun jx-forward-method (&optional end-of-match-included)
  "Find the next method following point, or the current method if
point is currently inside a method header. If a method is found then
point is moved to after the opening brace. If no method is found then
point is not moved. The endOfMatchIncluded controls whether or not
the very last character in the header, the {, is included or not."
  (interactive)
  ;; Need to exclude case where we're just after the { for which
  ;; jx-in-method-header will return t. If we don't do this then
  ;; repeated jx-forward-method calls don't actually move once the
  ;; first method is found.
  (if (and (jx-in-method-header)
           (or end-of-match-included
               (< (point) (match-end 0))))
      (goto-char (match-end 0))
    (let ((found))
      (while (and (setq found (re-search-forward jx-method-regexp nil t))
		  (equal (jx-get-method-name) "catch")))
      found)))

(defun jx-backward-method ()
  "Find the previous method before point, or the current method if
point is currently inside a method header. If a method is found then
point is moved to the start of the first line of the header. If no
method is found then point is not moved."
  (interactive)
  ;; Need to exclude case where we're right at the start of first line
  ;; of the header. In this position jx-in-method-header will return
  ;; t. If we don't do this then repeated jx-backward-method calls
  ;; don't actually move once the first method is found.
  (if (and (jx-in-method-header)
           (> (point) (match-beginning 0)))
      (goto-char (match-beginning 0))
    (let ((found))
      (while (and (setq found (re-search-backward jx-method-regexp nil t))
		  (equal (jx-get-method-name) "catch")))
      found)))

(defadvice isearch-search (around jx-isearch-method disable)
  "This advice makes isearch use jx-search-forward instead of
search-forward and jx-search-backward instead of search-backward."
  ;; Store the functions currently used for forward and backward
  ;; search.
  (let ((current-search-forward (symbol-function 'search-forward))
	(current-search-backward (symbol-function 'search-backward)))
    (fset 'search-forward 'jx-search-forward)
    (fset 'search-backward 'jx-search-backward)
    ;; Use unwind-protect to make sure the original search functions
    ;; get restored whatever happens.
    (unwind-protect 
	ad-do-it
      (fset 'search-forward current-search-forward)
      (fset 'search-backward current-search-backward))))

(defun jx-isearch-forward ()
  "Incremental forward search by method name."
  (interactive)
  (jx-isearch 'forward))

(defun jx-isearch-backward ()
  "Incremental backward search by method name."
  (interactive)
  (jx-isearch 'backward))

(defun jx-isearch (direction)
  "Incremental search in DIRECTION which must be forward of backward. This
function activates the advice jx-isearch-method and then calls either
isearch-forward or isearch-backward depending on DIRECTION."
  ;; Enable and activate the advice
  (ad-enable-advice 'isearch-search 'around 'jx-isearch-method)
  (ad-activate 'isearch-search)
  ;; Use unwind-protect to make sure the advice will get disabled
  ;; whatever happens.
  (unwind-protect
      (cond
       ((eq direction 'forward)
	(isearch-forward))
       ((eq direction 'backward)
	(isearch-backward))
       (t
	(error "Direction must be forward or backward")))
    (ad-disable-advice 'isearch-search 'around 'jx-isearch-method)
    (ad-activate 'isearch-search)))

(defun jx-search-forward (name &optional bound noerror)
  "Search forward for the name provided. This is also called by
isearch-search because of the advice attached to that function. It is
always passed bound of nil and noerror of t by isearch-search."
  (interactive "sMethod: ")
  (jx-search 'forward name bound noerror))

(defun jx-search-backward (name &optional bound noerror)
  "Search backward for the name provided. This is also called by
isearch-search because of the advice attached to that function. It is
always passed bound of nil and noerror of t by isearch-search."
  (interactive "sMethod: ")
  (jx-search 'backward name bound noerror))

(defun jx-search (direction name &optional bound noerror)
  "Search in DIRECTION for NAME, ignore BOUND but respect
noerror. This is really for use from isearch-search via
jx-search-forward or jx-search-backward and the advice attached to
isearch-search."
  (let ((look (function (lambda (name)
			  ;; Ignore the possibility of being inside a
			  ;; header as we start at the top of the
			  ;; file. Could be a problem if someone
			  ;; narrows the buffer but deal with that
			  ;; later.
			  (let ((found nil))
			    (while (and (not found)
					(cond
					 ((eq direction 'forward)
					  (re-search-forward jx-method-regexp nil t))
					 ((eq direction 'backward)
					  (re-search-backward jx-method-regexp nil t))
					 (t
					  (error "Direction not forward or backward"))))
			      (save-match-data
				(if (string-match name (jx-get-method-name))
				    (setq found (point)))))
			    found)))))
    (let ((pos nil))
      ;; Don't let the search process move point
      (save-excursion
	(setq pos (funcall look name)))
      ;; If method found then go there otherwise a message is nice
      (if pos
          (progn
	    (goto-char pos)
	    pos)
	(if (not noerror)
	    (error "Method %s not found" name))
        nil))))

(defun jx-in-method-header ()
  "Return t if in a method header, nil if not. If in a method header
the match data will be set. Being on the very first character of the
first line of a method definition or after the { at the end of a
method is counted as being in the method."
  (let ( ;; Where we are now
        (pos (point))
        ;; position of the next {
        (open-brace (and (search-forward "{" nil 1)
                         (+ (point) 1))))
    ;; Go back to where we started
    (goto-char pos)
    ;; Look for the previous } and position ourselves there or if not
    ;; found then at the start of the buffer.
    (search-backward "}" nil 1)
    ;; Is there a method header between the last } and the next { and
    ;; if there is did point start out inside it?
    (let ((found (and (re-search-forward jx-method-regexp open-brace t)
                      (>= pos (match-beginning 0))
                      (<= pos (match-end 0)))))
      ;; Go back to where we started before returning
      (goto-char pos)
      found)))

(defmacro jx-sig-return-type (sig)
  "Return the return type from the signature SIG"
  (list 'nth 2 sig))

(defmacro jx-sig-method-name (sig)
  "Return the method name from the signature SIG"
  (list 'nth 3 sig))

(defmacro jx-sig-params (sig)
  "Return the parameters from the signature SIG"
  (list 'nth 4 sig))

(defmacro jx-sig-exceptions (sig)
  "Return the exceptions from the signature SIG"
  (list 'nth 5 sig))

(defun jx-get-method-signature ()
  "Return the signature of the method that was just matched by
jx-in-method-header, jx-forward-method, or jx-backward-method."
  ;; Result is a list
  (list (cons (match-beginning 0) (match-end 0))
        ;; list of method modifiers as a list of strings or nil if no
        ;; modifiers (nth 1
        (jx-buffer-match-to-list
         jx-reindex-modifiers
         "\\w+"
         (function (lambda (s)
                     (substring s (match-beginning 0) (match-end 0)))))
        ;; return type or nil if a constructor (nth 2
        (if (match-beginning jx-reindex-return)
	    (jx-bsnp jx-reindex-return)
          nil)
        ;; method name (nth 3
        (jx-get-method-name)
        ;; the method arguments as a list of cons cells, each with a
        ;; car of the argument type and a cdr of the argument name (nth 4
        (jx-buffer-match-to-list
         jx-reindex-args
         (concat "\\(final[ \t\r\n]+\\)?"
                 "\\(\\(\\w\\|\\s \\|\\.\\)+\\([][ \t]*]\\)?\\)" ;type
                 "[ \t\r\n]+"		; whitespace
                 "\\(\\(\\w\\|\\s \\)+\\)" ;name
                 )
         (function (lambda (s)
                     (cons (substring s (match-beginning 2) (match-end 2))
                           (substring s (match-beginning 5) (match-end 5))))))
        ;; the exceptions as a list of strings (nth 5
        (sort (jx-buffer-match-to-list
	       jx-reindex-throws
	       "\\(\\w\\|\\.\\)+"
	       (function (lambda (s)
			   (substring s (match-beginning 0) (match-end 0)))))
	      'string<)))
  
(defun jx-get-method-name ()
  "Return the name of the most recently matched method header."
  (jx-bsnp jx-reindex-name))

(provide 'jx-methods)