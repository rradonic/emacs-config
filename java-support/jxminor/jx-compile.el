;;; jx-compile.el --- Provides compile functions for jxminor

;; Copyright (C) 2001, 2002 Alex Moffat
;; Keywords: java
;; Version: 1.6
;; CVS: $Id: jx-compile.el,v 1.6 2002/10/13 01:05:26 alexmoffat Exp $

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
;; Provides support for compiling java programs that understands
;; something about how ant and make are used in these situations.
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
;; 20020308 ajm Initial version. Sort of supports ant, no real support
;;              for make.
;;
;; 20020310 ajm Additional support for make. Cleanup of ant support.
;; 

;; Some utility functions used
(require 'jx-util)

;; Needed to access compile-history
(require 'compile)

;; Needed to parse build.xml for ant
(require 'xml)

(defun jx-compile ()
  "Compile command that knows more about how to compile java than the
standard compile does. See info documentation for more information."
  (interactive)
  ;; Find the build/make file.
  (let ((build-file (jx-find-build-file default-directory)))
    (if (not (null build-file))
        ;; Found a build/make file.
        (let ( ;; Work out a possible command for compiling.
              (possible-compile-command (jx-compile-command build-file
                                                            buffer-file-name))
              ;; Set the default directory to the location of the
              ;; build/make file.
              (default-directory (file-name-directory build-file)))
          ;; Did we find a compile command?
          (if possible-compile-command
              ;; Yes, so set compile-command so that compile uses it.
              (let ((compile-command possible-compile-command))
                (call-interactively 'compile))
            ;; No, so just call compile
            (call-interactively 'compile)))
      ;; No build file found so just default to compile.
      (call-interactively 'compile))))

(defvar jx-build-file-names
  '("build.xml" "makefile" "Makefile")
  "A list of the names of the build files to look for in the order in
which they are preferred.")

(defvar jx-ant-target-history
  '()
  "A list of the previous ant targets selected. Used as a history by
completing read.")

(defcustom jx-ant-command
  "ant -emacs -buildfile %b %t"
  "The command used by jx-compile to invoke ant. The token %b is
replaced by the name of the build file, without its directory, the
token %B is replaced by the full path name of the build file, and the
token %t is replaced by the target chosen."
  :group 'jxminor
  :tag "Command to invoke ant"
  :type 'string
  :require 'jx-compile)

(defvar jx-make-target-history
  '()
  "A list of the previous make targets selected. Used as a history by
completing read.")

(defcustom jx-make-command
  "make -k %t"
  "The command used by jx-compile to invoke make. The token %b is
replaced by the name of the build file, without its directory, the
token %B is replaced by the full path name of the build file, and the
token %t is replaced by the target chosen."
  :group 'jxminor
  :tag "Command to invoke make"
  :type 'string
  :require 'jx-compile)

(defun jx-compile-command (build-file current-file)
  "Return the compile command to suggest to the user, or nil if no
good suggestion can be made. BUILD-FILE is the full path to the
build file found. CURRENT-FILE is the file the user was viewing
when they invoked jx-compile, may be null."
  (let ((build-file-name (file-name-nondirectory build-file)))
    (cond
     ;; If we found ant
     ((equal build-file-name "build.xml")
      (jx-ant-compile-command build-file current-file))
     ;; If we found a makefile
     ((or (equal build-file-name "makefile")
          (equal build-file-name "Makefile"))
      (jx-make-compile-command build-file current-file))
     ;; No idea how we found this. 
     (t
      (error "jx-compile-command does not understand %s as a build file"
             build-file)))))

(defun jx-ant-compile-command (build-file current-file)
  "Return the compile command that should be used for the ant
BUILD-FILE passed in or nil if one can't be determined. The list of
targets defined in the build.xml file is used in a completing-read to
make it easier for the user to choose one."
  ;; Parse the build.xml file
  (let ((xml (xml-parse-file build-file)))
    (if xml
        ;; Parse worked
        (let* (	;; Number used in completion list
               (n 0)
               ;; Build completion list from the values of the name
               ;; attributes of the target elements.
               (targets (jx-filtered-map
                         (function (lambda (x)
                                     (if (equal (xml-node-name x) 'target)
                                         (progn
                                           (setq n (+ n 1))
                                           (cons (xml-get-attribute x 'name) n))
                                       nil)))
                         (xml-node-children (car xml)))))
	  ;; Let the user choose a target. Suggest the one they previously chose.
          (let ((target (completing-read "Choose target: " targets
                                         nil nil (if (not (null jx-ant-target-history))
                                                     (car jx-ant-target-history)
                                                   nil) 'jx-ant-target-history)))
            (if (null target)
                ;; No target chosen
                nil
              ;; See if there was a previous compile for this
              ;; target. If there was suggest the complete command the
              ;; user entered last time so that any params they added
              ;; after the target name don't have to be reentered.
              (jx-find-previous-compile
               (jx-substitute-in-command jx-ant-command
                                         build-file
                                         target)))))
      ;; Parse failed
      nil)))

(defun jx-make-compile-command (make-file current-file)
  "Return the compile command for the MAKE-FILE and CURRENT-FILE
passed in. If CURRENT-FILE is not null it is suggested as a target,
otherwise no suggestion is made."
  (let ((target (read-string "Choose target: "
                             (or current-file "")
                             jx-make-target-history)))
    (jx-substitute-in-command jx-make-command
                              make-file
                              target)))

(defun jx-find-build-file (dir)
  "Return a build file if one can be found in directory DIR or its
parents. If none can be found return nil."
  (jx-find-first
   (function (lambda (x)
               (jx-look-for-build-file dir x)))
   jx-build-file-names))

(defun jx-look-for-build-file (dir build-file)
  "Look in DIR and its parent directories for BUILD-FILE."
  (let ((found nil))
    (while (and (not found)
                (not (null dir)))
      (let ((file (expand-file-name build-file dir)))
        (if (file-exists-p file)
            ;; Found a file.
            (setq found file)
          ;; No file found.
          (let ((lastdir dir))
            ;; The parent directory.
            (setq dir (file-name-directory (directory-file-name dir)))
            ;; No change in directory so must be at root.
            (if (equal lastdir dir)
                (setq dir nil))))))
    ;; Return what we found or nil
    found))

(defun jx-find-previous-compile (command-prefix)
  "Look through the compile-history for the first element that starts
with command-prefix and return it. If none can be found return
command-prefix."
  (let ((l compile-history)
        (n (length command-prefix))
        (found nil))
    (while (and (not found)
                (not (null l)))
      (let ((s (car l)))
        (if (and (>= (length s) n)
                 (eq (compare-strings s 0 n command-prefix 0 n) t))
            (setq found s)
          (setq l (cdr l)))))
    (or	found
        command-prefix)))

(defun jx-substitute-in-command (command-string-template build-file target)
  "Substitute in COMMAND-STRING-TEMPLATE for the tokens B, b, and
t. B is replaced by BUILD-FILE, b is replaced by
(file-name-nondirectory BUILD-FILE) and t is replaced by TARGET."
(let ((s command-string-template)
      (case-fold-search nil))
  (if (string-match "%B" s)
      (setq s (concat (substring s 0 (match-beginning 0))
		      build-file
		      (substring s (match-end 0)))))
  (if (string-match "%b" s)
      (setq s (concat (substring s 0 (match-beginning 0))
		      (file-name-nondirectory build-file)
		      (substring s (match-end 0)))))
  (if (string-match "%t" s)
      (setq s (concat (substring s 0 (match-beginning 0))
		      target
		      (substring s (match-end 0)))))
  s))

(provide 'jx-compile)