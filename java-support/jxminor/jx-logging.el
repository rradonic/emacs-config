;;; jx-logging.el --- java logging related functions

;; Copyright (C) 2001, 2002 Alex Moffat
;; Keywords: java
;; Version: 1.0
;; CVS: $Id: jx-logging.el,v 1.5 2002/05/21 01:25:59 alexmoffat Exp $

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
;; Functions that insert log4j debug logging statements into the
;; current buffer. It is used by other of the java-....el files.
;;
;; Installation:
;; 
;; Make the file accessible somewhere on your load path.
;;
;; Customization:
;; 
;; You can change the value of the variable java-log4j-statement to
;; the name of the java variable that contains the
;; org.apache.log4j.Category instance to use for logging. It can also
;; be set to a method call, for example getLogCategory(), that returns
;; the Category instance to use.
;;
;; Change log:
;;
;; 20020218 ajm Corrected problem with not completely renaming
;;              java-log4j-insert-debug everywhere it was used.
;; 
;; 20020219 ajm Renamed to use jx- prefix instead of java- and checked
;;              into CVS.
;;
;; 20020305 ajm Provide menu through a function.
;;
;; 20020319 ajm Moved jx-insert-method-log from jx-methods. Added initial
;;              value to jx-log4j-insert-debug initial argument.
;;
;; 20020407 ajm Renamed from jx-log4j and added customization and support
;;              for logging using methods other than log4j.
;;
;; 20020513 ajm Changed way in which jx-log-should-log-statement and
;;              jx-log-logging-method are calculated. Actually works
;;              this way.

(require 'jx-methods)

(defcustom jx-log-log4j-category-field
  "logCat"
  "Name of the java variable holding the instance of
org.apache.log4j.Category that should be used for logging. Can also be
a method call that returns a Category. Must support isDebugEnabled and
debug methods. Used to generate the logging statements when \"Log4j\" is
chosen as the logging package."
  :group 'jxminor
  :tag "Log4j category field"
  :type 'string
  :require 'jx-logging)

(defcustom jx-log-sysout-should-log
  nil
  "Statement to evaluate to see whether we should log to System.out. May be
as simple as a boolean variable or some other condition or method call. Used
when \"System.out\" is chosen as the logging package."
  :group 'jxminor
  :tag "System.out check statement"
  :type '(choice string
		 (const :tag "None" nil))
  :require 'jx-logging)

(defcustom jx-log-method
  'log4j
  "The logging package to use."
  :group 'jxminor
  :tag "Logging package to use."
  :type '(choice (const :tag "Log4j" log4j)
		 (const :tag "System.out" sysout)
		 (const :tag "Other" other))
  :require 'jx-logging)

(defun jx-log-should-log-statement ()
  "A java statement that return a boolean to indicate whether logging
should take place."
  (cond ((eq jx-log-method 'log4j)
	 (concat jx-log-log4j-category-field ".isDebugEnabled()"))
	((eq jx-log-method 'sysout)
	 jx-log-sysout-should-log)
	((eq jx-log-method 'other)
	 "Unsupported")))

(defun jx-log-logging-method ()
  "A java statement to log a message"
  (cond ((eq jx-log-method 'log4j)
	 (concat jx-log-log4j-category-field ".debug"))
	((eq jx-log-method 'sysout)
	 "System.out.println")
	((eq jx-log-method 'other)
	 "Unsupported")))

(defun jx-mouse-insert-debug-log (mouse-event)
  "Position point where the mouse was clicked and call jx-insert-debug-log"
  (interactive "e")
  (goto-char (posn-point (event-start mouse-event)))
  (jx-insert-debug-log))

(defun jx-insert-debug-log ()
  "Insert statements to write a debugging message using some sort of
logging system. The name of the method the debugging message is being
issued from is provided as the initial value for the first part of the
message."
  (interactive)
  ;; If at the end of the line then the logging statement goes after
  ;; this line, otherwise it goes before this line.
  (if (eolp)
      (newline-and-indent)
    (if (not (bolp))
	(forward-line 0))
    (newline)
    (forward-line -1)
    (indent-according-to-mode))
  (if (jx-log-should-log-statement)
      (progn
	(insert "if (" (jx-log-should-log-statement) ")")
	(newline-and-indent)))
  (insert (jx-log-logging-method) "(")
  (let ((sig (save-excursion
               (jx-backward-method)
               (jx-get-method-signature))))
    (jx-log-insert-more-debug (read-string "Enter msg: "
                                             (concat "\"" (nth 3 sig) "(...)"))
                                t))
  (insert ");"))

(defun jx-log-insert-more-debug (msg &optional firstTime)
  "Insert an additional debugging message with a plus between it and
any previous messages."
  (if (or (null msg)
          (eq (length msg) 0))
      ()
    (if (not firstTime)
        (progn
          (insert " + ")
          (newline-and-indent)))
    (insert msg)
    (jx-log-insert-more-debug (read-string "More msg or enter to end: " ""))))

(defun jx-mouse-insert-method-log (mouse-event)
  "Position point where the mouse was clicked and call jx-insert-method-log"
  (interactive "e")
  (goto-char (posn-point (event-start mouse-event)))
  (jx-insert-method-log))

(defun jx-insert-method-log ()
  "Insert logging statements to log the name of the preceeding
method and all of its parameters."
  (interactive)
  (if (jx-backward-method)
      (progn
        (goto-char (match-end 0))
        (newline)
        (let ((sig (jx-get-method-signature)))
          (jx-log-insert-debug-list
	   (append
	    (list (concat "\"" (nth 3 sig) "(\""))
	    (if (not (null (nth 4 sig)))
            (jx-separate (mapcar (function (lambda (x)
                                             (cdr x))) (nth 4 sig))
                      "\", \""))
	    (list "\")\"" )))))))

(defun jx-log-insert-debug-list (l)
  "Insert a list of items as a debug call. Point should be positioned
at the start of an empty line when this function is called."
  (let ((pos (point))
        (indent-region-function nil))
    (if (jx-log-should-log-statement)
	(progn
	  (insert "if (" (jx-log-should-log-statement) ")")
	  (newline)))
    (insert (jx-log-logging-method) "(")
    (while (not (null l))
      (insert (car l))
      (if (not (null (cdr l)))
          (progn
            (insert " +")
            (newline)))
      (setq l (cdr l)))
    (insert ");")
    (indent-region pos (point) nil)
    (untabify pos (point))))

(defun jx-log-submenu ()
  "Create and return a keymap to use as a menu to access the functions
jx-loggging provides."
  (let ((jx-log-submenu (make-sparse-keymap "Logging")))

    (define-key jx-log-submenu [method-debug-statement]
      '(menu-item "Method logging" jx-mouse-insert-method-log
		  :key-sequence nil
		  :help "Insert a logging debug level statement for the prior method in the file."))
    
    (define-key jx-log-submenu [prompt-debug-statement]
      '(menu-item "Prompt logging" jx-mouse-insert-debug-log
		  :key-sequence nil
		  :help "Insert a logging debug statement prompting for the things to log."))

    (cons "Logging" jx-log-submenu)))
 
(provide 'jx-logging)

