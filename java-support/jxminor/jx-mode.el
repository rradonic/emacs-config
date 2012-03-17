;;; jx-mode.el --- Provides jxminor ("JX") minor mode.

;; Copyright (C) 2001, 2002 Alex Moffat
;; Keywords: java
;; Version: 1.6
;; CVS: $Id: jx-mode.el,v 1.14 2002/10/13 01:06:00 alexmoffat Exp $

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
;; Provides a minor mode called jxminor, indentified in the
;; mode line by "JX", that has a number of useful functions for
;; helping with editing java code.
;;
;; Installation:
;; 
;; Make the files accessible somewhere on your load path.
;; In your .emacs file put
;; (autoload 'jxminor-mode-on "jx-mode" "" t)
;; (add-hook 'java-mode-hook 'jxminor-mode-on)
;;
;; Customization:
;; 
;; See each of the individual files for customization options.
;;
;; Change log:
;;
;; 20020102 ajm Changed hoist-forward to use delete-region instead of
;;              kill-region so that the kill ring is not modified when
;;              java-sort-imports is run.
;;
;; 20020102 ajm Added java-delete-commented-imports command to make it
;;              easier to get rid of the import statements that
;;              java-check-imports comments out.
;;
;; 20020102 ajm Added java-add-extras-menu to make the commands
;;              available as menu options when in java mode.
;;
;; 20020104 ajm Changed to use forward-line instead of
;;              beginning-of-line so that we ignore field boundaries
;;              if they are enabled.
;;
;; 20020104 ajm If deleting an import and the preceeding and following
;;              lines are blank then also delete the following
;;              line. This avoids introducing unwanted blank lines.
;;
;; 20020111 ajm Added functions to insert logging messages into java
;;              programs. Wrote more documentation.
;;
;; 20020212 ajm Improved java-sort-imports so that it comments out
;;              duplicate import statements and import statements that
;;              import classes that are referred to in commented out
;;              code.
;;
;; 20020215 ajm Moved the log4j code to its own file.
;;              Moved the import related code to its own file.
;;              Introduced new file of functions for java method related
;;              operations.
;;              Renamed to java-extras from java-stuff
;;              Defined java-extras-mode
;;
;; 20020217 ajm Added more documentation for java-extras-mode
;;              Rearranged menu structure
;;              Added COPYING and README files to distribution
;;
;; 20020218 ajm Renamed java-find-next-method and
;;              java-find-prior-method to java-forward-method and
;;              java-backward-method as the find names would be better
;;              used for finding.
;;
;; 20020219 ajm Renamed to use jx- prefix instead of java- and checked
;;              into CVS.
;;
;; 20020224 ajm Added template functions to menus.
;;
;; 20020303 ajm Added code to make it possible to read info for this
;;              mode.  Started to create customization group. Added
;;              Help entry to menu.
;;
;; 20020304 ajm Improved way that jxminor-mode-info looked for info
;;              files and the sort of error messages it provides.
;;
;; 20020305 ajm Restructured way menu is constructed to increase
;;              encapsulation of information in other files. jx-mode
;;              doesn't need to know what's in each menu, just that
;;              there is a menu. This could be changed to handle
;;              packages that don't provide a menu, and could be
;;              extended to deal with keybindings.
;;
;; 20020324 ajm Added javadoc managing code in a separate file.
;;
;; 20020407 ajm Use new jx-logging package instead of jx-log4j
;;

;; The utilities stuff
(require 'jx-util)

;; We need the logging code
(require 'jx-logging)

;; We need the imports code
(require 'jx-imports)

;; We need the methods code
(require 'jx-methods)

;; We need the template code
(require 'jx-template)

;; We need the compile code
(require 'jx-compile)

;; We need the javadoc code
(require 'jx-javadoc)

;; We need the jump-to code
(require 'jx-jump-to)

(defgroup jxminor nil
  "Minor mode to help with editing Java code"
  :tag "Java extras minor mode"
  :group 'languages
  :version "21.1.1")

(defcustom jxminor-mode-info
  "jxminor"
  "Where to find the Info file for `jxminor-mode'. 
The default value assumes that the info file \"jxminor.info\" is on
Emacs' info directory path. If it's not either put the file there or
set the value of this variable to the exact location of the file. This
can be done in your .emacs file or through this customization interface.
(setq jxminor-mode-info \"~/elisp/jxminor/jxminfo.info\")"
:group 'jxminor
:tag "Info file location"
:type '(choice (string :tag "Name of info file")
               (file :tag "Path to info file" :must-match t))
:require 'jx-mode)

(defun jxminor-mode-info ()
  "Command to access the info documentation for `jxminor-mode'.
See doc for the variable `jxminor-mode-info'."
  (interactive)
  (require 'info)
  ;; Want to provide a friendly sort of error if we can't find the
  ;; file so we'll check first.
  (let ((file-name jxminor-mode-info))
    ;; Look for the value of jxminor-mode-info
    (if (not (jx-can-find-info-file file-name))
	;; That didn't work so look for jxminor.info in the directory
	;; this file was loaded from.
	(progn
	  (setq file-name
		(expand-file-name
		 "jxminor.info"
		 (file-name-directory (locate-library "jx-mode"))))
	  (if (not (jx-can-find-info-file file-name))
	      ;; That didn't work either so give up and tell the user
	      ;; about jxminor-mode variable.
	      (progn
		(describe-variable 'jxminor-mode-info)
		(error "Jxminor can't find info file \"%s\", try setting this variable then try the help command again"
		       jxminor-mode-info))
	    ;; Couldn't find the file in jxminor-mode-info but found
	    ;; file jxminor.info in directory this file was loaded
	    ;; from. Use it but report issue to user.
	    (message "jxminor couldn't find info file \"%s\" but found \"%s\". Try setting jxminor-mode-info variable."
		     jxminor-mode-info file-name))))
    ;; Now we can go to info mode on the file. Still might fail so
    ;; trap errors.
    (condition-case nil
	(progn
	  (info file-name))
      (error (progn
	       (describe-variable 'jxminor-mode-info)
	       (message "Info can't find \"%s\", try setting this variable then try the help command again"
			file-name))))))
    
(defun jx-can-find-info-file (filename)
  "Look for an info file in a similar way to the info command. This
function doesn't have to be completely accurate as info will be
invoked anyway if this returns non nil, so we don't check for
particular extensions."
  (require 'info)
  ;; Check first if filename is a file we can find
  (if (file-exists-p (expand-file-name filename))
      t
    ;; Couldn't find the file so look in the various directories
    (let (;; The list of directories to check.
	  (l (if Info-additional-directory-list
		 (append Info-directory-list Info-directory-list)
	       Info-directory-list))
	  ;; Have we found it yet?
	  (found nil)
	  ;; The regular expression to look for.
	  (regexp (concat "^"
			  (file-name-nondirectory
			   (file-name-sans-extension filename))
			  "\\.")))
      ;; Loop checking each directory until we find something or run
      ;; out of directories.
      (while (and (not found) l)
	(if (directory-files (car l) nil regexp nil)
	    (setq found t)
	  (setq l (cdr l))))
      found)))

(defun jxminor-mode-on ()
  "Turn on jxminor mode. This is for calling from java-mode-hook"
  (jxminor-mode t))

(define-minor-mode jxminor-mode
  ;; Documentation for the mode
  "Java extras minor mode

The following two commands are for navigation. If point is inside a
method header when either is called then they consider that method the
next or previous method.  
\\[jx-forward-method] move to the opening { following the next
method in the file
\\[jx-backward-method] move to the start of the previous method in
the file
\\[jx-isearch-forward] incremental search forward for method
\\[jx-isearch-backward] incremental search backward for method

The following commands are also available through the JXMinor submenu
under the Java menu.

Commands for managing import statements
\\[jx-sort-imports] sort the import statements at the start of the
file
\\[jx-check-imports] check that each import statement is needed and
comment out any that aren't
\\[jx-sort-and-check-imports] first sort and then check the import
statements
\\[jx-delete-commented-imports] delete any import statements that
are commented out

Commands for inserting logging statements
\\[jx-insert-debug-log] insert a debug statement, prompting
for the various items to log
\\[jx-insert-method-log] insert a debug statement that logs
the parameters of the previous method

Command for javadoc
\\[jx-insert-jdoc] insert a skeleton javadoc for the next method in the file

Getting help
\\[jxminor-mode-info] read the help information about this mode"
  ;; Initial value so mode is off initially
  nil
  ;; Mode line indicator
  " JX"
  ;; Key bindings
  '(("\M-n" . jx-forward-method)
    ("\M-p" . jx-backward-method)
    ("\M-s" . jx-isearch-forward)
    ("\M-r" . jx-isearch-backward)
    ("\C-c\C-fi" . jx-minor-mode-info)
    )
  ;; Additional code executed on switching modes
  (if (not (null jxminor-mode))
      (jx-add-jxminor-menu)
    (jx-remove-jxminor-menu)))

(defun jx-compile-submenu ()
  "The menu for compilation"
  '(menu-item "JxCompile" jx-compile
	      :key-sequence nil
	      :help "Compile using Ant or Make"))

;; The keymap used for the jxminor menu
(defvar jx-jxminor-menu
  (let ((menu (make-sparse-keymap "Extras")))
    
    (define-key menu [help]
      '(menu-item "Help" jxminor-mode-info
		  :key-sequence nil
		  :help "Read the info for java extras mode"))
    
    (define-key menu [help-separator]
      '(menu-item "--shadow-etched-in"))

    (define-key menu [templates] (jx-templates-submenu))
    
    (define-key menu [imports] (jx-imports-submenu))

    (define-key menu [logging] (jx-log-submenu))

    (define-key menu [javadoc] (jx-javadoc-submenu))

    (define-key menu [jump] (jx-jump-to-submenu))
    
    (define-key menu [compile] (jx-compile-submenu))    

    menu)
"The menu that presents some extra java mode functionality")

(defun jx-add-jxminor-menu ()
  "Add the jxminor menu if it is not already present."
  (let ((java-menu (lookup-key java-mode-map [menu-bar Java])))
    (if (null (lookup-key java-menu [jxminor]))
	(progn
	  (define-key-after java-menu
	    [jxminor] (cons "Extras" jx-jxminor-menu) t)
	  (local-set-key [M-down-mouse-1] jx-jxminor-menu)))))

(defun jx-remove-jxminor-menu ()
  "Remove the jxminor menu if it is present."
  (let ((java-menu (lookup-key java-mode-map [menu-bar Java])))
    (if (not (null (lookup-key java-menu [jxminor])))
	(progn
	  (define-key java-menu [jxminor] nil)
	  (local-set-key [M-down-mouse-1] nil)))))

(provide 'jx-mode)

