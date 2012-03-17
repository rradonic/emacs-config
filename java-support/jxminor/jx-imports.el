;;; jx-imports.el --- elisp functions to manipulate import statements

;; Copyright (C) 2001, 2002 Alex Moffat
;; Keywords: java
;; Version: 1.0
;; CVS: $Id: jx-imports.el,v 1.5 2002/03/25 03:22:22 alexmoffat Exp $

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
;; Functions that manipulate the import statements in a java program.
;; Functions are provided to -
;;
;; 1. Sort the imports alphabetically by package name putting blank
;;    lines between imports from different packages.
;;
;; 2. Comment out import statements for classes that are not used in
;;    the program.
;;
;; 3. Deleting commented out import statements.
;;
;; 4. Checking whether an import statement needs to be added for a
;;    word in the program (when interpreted as a class name).
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
;; 20020219 ajm Renamed to use jx- prefix instead of java- and checked
;;              into CVS.
;;
;; 20020220 ajm Use a marker to record the position of the end of the
;;              region in jx-check-imports so that when comment
;;              characters are inserted as lines are commented out we
;;              still know where the end of the region is.
;;
;; 20020305 ajm Provide menu through a function.
;;
;; 20020324 ajm Move provide to end of file

;; Define some regular expressions that are used later

(defconst jx-import-regexp-prefix
  "\\s-*import\\s-+\\([^ ]+\\."
  "The first part of a regular expression that recognizes import
statements.")

(defconst jx-import-regexp
  (concat jx-import-regexp-prefix
	  "\\([^;]+\\)\\);")
  "A regular expression that recognizes import statements.")

(defconst jx-uncommented-import-regexp
  (concat "^" jx-import-regexp)
  "Regular expression to recognize an import statement that has
not been commented out.")

(defconst jx-commented-import-regexp
  (concat "^" "\\s-*//" jx-import-regexp)
  "Regular expression to recognize an import statement that has been
commented out by this package.")

(defun jx-get-import-region (&optional import-regexp)
  "Find the region containing the import statements and return a
cons of its start and end positions."
  (let ((import-regexp (if import-regexp
                           (if (equal (aref import-regexp 0) ?^)
                               import-regexp
                             (concat "^" import-regexp))
			 jx-uncommented-import-regexp)))
    ;; Start at the end of the buffer
    (goto-char (point-max))
    ;; If no imports return empty list
    (if (not (re-search-backward import-regexp nil t))
        ()
      ;; Find the end of the region containing all the imports and
      ;; record it
      (end-of-line)
      (let ((end-of-imports (+ (point) 1)))
        ;; Now find the start of the "import region" and return the
        ;; start and end markers.
        (goto-char (point-min))
        (re-search-forward import-regexp)
	(cons (match-beginning 0) end-of-imports)))))

(defun jx-check-if-needs-import ()
  "Check if the word point is in or just after names a class that
needs to be imported."
  (interactive)
  (save-excursion
    (backward-word 1)
    (let ((word-start (point)))
      (forward-word 1)
      (let ((the-word (buffer-substring-no-properties word-start (point)))
            (import-region (jx-get-import-region)))
        (goto-char (car import-region))
        (if (re-search-forward (concat "^"
				       jx-import-regexp-prefix
                                       the-word
                                       "\\);")
                               (cdr import-region) t)
            (message "Found import \"%s\"." (jx-bsnp 1))
          (let ((source-file (concat the-word ".java")))
            (if (file-exists-p source-file)
                (message "Found source \"%s\" in current dir." source-file)
              (message "Did not find %s." the-word))))))))

(defun jx-sort-imports (&optional no-message)
  "Sort the import statements at the top of a java program.  They are
grouped into three sets in the following order, those starting with
java, those starting with org, and those starting with com. Within
each set they are sorted alphabetically and blank lines are inserted
between packages."
  (interactive)
  (let* ( ;; Regexp to recognize import statements, which may be commented out.
        ;; hoist-forwards appends a package name to the end of this to
         ;; form a new regular expression so be careful changing it.
         (import-regexp "\\s-*\\(//\\)?\\s-*import\\s-+")
         ;; Function to move a block of import statements that all
         ;; start with hoist-import from their current position to
         ;; hoist-point. Must be no blank lines between import
         ;; statements when this is called.
         (hoist-forward
          (lambda (hoist-import hoist-point)
            (goto-char (point-min))
            (if (re-search-forward
                 (concat "^" import-regexp hoist-import)
                 nil
                 t)
                (progn
                  (forward-line 0)
                  (let ((start-of-region (point)))
                    (next-line 1)
                    (while (looking-at
                            (concat import-regexp hoist-import))
                      (next-line 1))
                    (let ((copied-text (buffer-substring start-of-region
                                                         (point))))
                      (delete-region start-of-region
                                     (point))
                      (goto-char hoist-point)
                      (insert copied-text)))))))
         ;; Function to sort the import statements based on the names
         ;; being imported. All blank lines or lines that are not
         ;; import statements sort to the front.
         (sort-imports
          (lambda (start-of-sort-region end-of-sort-region)
            (save-restriction
              (narrow-to-region start-of-sort-region
                                end-of-sort-region)
              (goto-char (point-min))
              (sort-subr
               nil
               ;; start of next record
               (function (lambda ()
                           (next-line 1)
                           (forward-line 0)))
               ;; end of current record
               (function (lambda ()
                           (end-of-line)))
              ;; key of record, this is the name imported if an import
               ;; statement is found, otherwise it is blank
               (function (lambda ()
                           (let ((here (point)))
                             (end-of-line)
                             (let ((limit (point)))
                               (goto-char here)
                               (if (re-search-forward (concat "^" import-regexp) limit t)
                                   (buffer-substring (match-end 0) limit)
                                 "")))))))))
         ;; Reformat a region of the current buffer that contains
         ;; import statements. This sorts the statements and inserts
         ;; blank lines between packages.
         (reformat-region
          (lambda (start-of-sort-region end-of-sort-region)
            ;; sort all the import statements based on the package
            ;; names
            (funcall sort-imports start-of-sort-region end-of-sort-region)
            ;; remove the non import lines that sorted to the top
            (goto-char start-of-sort-region)
            (re-search-forward (concat "^" import-regexp))
            (forward-line 0)
            (delete-region start-of-sort-region (point))
            ;; move the org stuff to the front
            (funcall hoist-forward "org"
                     start-of-sort-region)
            ;; move the java stuff to the front (so now it
            ;; has java in front of org in front of com)
            (funcall hoist-forward "java"
                     start-of-sort-region)
            ;; back to the beginning
            (goto-char start-of-sort-region)
            ;; put blank lines between the packages
            (let ((last-package "")
                  (this-package "")
                  (start-of-package 0))
              (while (looking-at import-regexp)
                (setq start-of-package (+ (match-end 0) 1))
                (end-of-line)
                (search-backward "." start-of-package)
                (setq this-package
                      (buffer-substring
                       start-of-package (point)))
                (if (not (equal last-package this-package))
                    (progn
                      (if (not (equal last-package ""))
                          (progn
                            (forward-line 0)
                            (newline)))
                      (setq last-package this-package)))
                (next-line 1)
                (forward-line 0))))))
    (save-excursion
      (let ((import-region (jx-get-import-region import-regexp)))
        (if (null import-region)
            ()
          (let ( ;; beginning of region containing imports
                (start-of-sort-region (car import-region))
                ;; end of region containing imports
                (end-of-sort-region (cdr import-region))
                ;; current buffer 
                (old-buf (current-buffer))
                ;; temp buffer for sorting in, name starts with space
                ;; so it won't be listed to user
                (temp-buf (generate-new-buffer " sort-imports"))
                ;; was the buffer modified
                (modified nil))
            ;; go to the temp buffer, insert the import statements and
            ;; reformat/sort them. we rely on save-excursion to always
            ;; get us back to the correct buffer
            (set-buffer temp-buf)
            (insert-buffer-substring old-buf start-of-sort-region
                                     end-of-sort-region)
            (funcall reformat-region (point-min) (point-max))
            ;; are the newly sorted statements the same as before
            ;; sorting?
            (if (equal
                 (compare-buffer-substrings
                  (current-buffer) (point-min) (point-max)
                  old-buf start-of-sort-region end-of-sort-region)
                 0)
                ;; yes, nothing changed
                (or no-message
                    (message "Already sorted. No changes made to buffer."))
              ;; no, so remove the unsorted ones and replace with
              ;; the sorted ones
              (set-buffer old-buf)
              (delete-region start-of-sort-region end-of-sort-region)
              (goto-char start-of-sort-region)
              (insert-buffer temp-buf)
              (or no-message
                  (message "Sorting completed. Buffer has been modified"))
              (setq modified t))
            ;; get rid of the temp buffer
            (kill-buffer temp-buf)
            modified))))))

(defun jx-check-imports (&optional no-message)
  "Check the import statements in a program and comment out any where
the class name can not be found in the program. Can not deal with
imports that end in * so they are ignored. Returns t if the buffer was
modified, nil otherwise."
  (interactive)
  ;; Don't loose our place.
  (save-excursion
    (let* ( ;; Regexp to recognize import statements that are not commented out
           (import-regexp jx-uncommented-import-regexp)
           ;; The region containing the imports
           (import-region (jx-get-import-region import-regexp)))
      ;; if no imports then do nothing and return nil 
      (if (null import-region)
          nil
        (let* (	;; was the buffer modified
               (modified nil)
               ;; Name of the last package seen
               (last-package-name "")
               ;; Don't ignore case when searching
               (case-fold-search nil)
               ;; Record start of import region
               (start-of-region (car import-region))
               ;; Set marker at end of import region
               (end-of-region (copy-marker (cdr import-region)))
               ;; Function to comment out a line if its not already
               ;; commented out.
               (comment-line (lambda (n)
                               (let ((pos (point)))
                                 (forward-line n)
                                 (if (not (looking-at "^\\s-*//"))
                                     (progn
                                       (insert "//")
                                       (setq modified t)))
                                 (goto-char pos))))
               ;; Function to find non commented out class name
               (find-non-commented
                (lambda (class-name)
                  (if (re-search-forward
                       (concat "\\Sw\\(" class-name "\\)\\Sw")
                       (point-max) t)
                      (if (equal (get-text-property (match-beginning 1) 'face)
                                 'font-lock-comment-face)
                          (funcall find-non-commented class-name)
                        t)
                    nil))))
          ;; Start at the beginning.
          (goto-char start-of-region)
          ;; While there are still import statements.
          (while (re-search-forward import-regexp (marker-position end-of-region) t)
            ;; Grab the package and class names from the buffer.
            (let ((package-name (match-string-no-properties 1))
                  (class-name (match-string-no-properties 2)))
              ;; If same package as last seen comment out previous
              ;; one. This means check of rest of program done more
              ;; than once but easier to program.
              (if (equal package-name last-package-name)
                  (funcall comment-line -1))
              ;; Don't bother with *.
              (if (not (equal class-name "*"))
                  ;; Remember where we are.
                  (let ((pos (point)))
                    ;; go to the end of the import region
                    (goto-char end-of-region)
                    ;; See if the class is referenced anywhere.
                    (let ((found (funcall find-non-commented class-name)))
                      ;; point moved and we need to move back.
                      (goto-char pos)
                      ;; Didn't find one so comment out the current
                      ;; line.
                      (if (not found)
                          (funcall comment-line 0)))))
              ;; Remember this package
              (setq last-package-name package-name))
            ;; Start searching again from the next line
            (forward-line 1))
          (or no-message
              (if modified
                  (message "Check completed. Buffer has been modified")
                (message "Check completed. No changes made to buffer")))
	  ;; null out the marker as it's no longer needed
	  (set-marker end-of-region nil)
          modified)))))

(defun jx-delete-commented-imports (&optional no-message)
  "Delete any import statements that have been commented out by
jx-check-imports."
  (interactive)
  (let ( ;; Regexp to recognize import statements commented out by
        ;; jx-check-imports
        (import-regexp jx-commented-import-regexp)
        ;; was the buffer modified
        (modified nil)
        (limit (point-max)))
    ;; Don't loose our place
    (save-excursion
      ;; Start at the beginning
      (goto-char (point-min))
      ;; While there are still commented out import statements
      (while (re-search-forward import-regexp limit t)
        ;; After the regexp matches point is at the end of the line
        ;; Move to start of previous line
        (forward-line -1)
        ;; Is this line blank
        (let ((blank-before (looking-at "^\\s-*$")))
          ;; Go to start of line to be deleted
          (forward-line 1)
          ;; Record position
          (let ((pos (point)))
            ;; Go to start of next line
            (forward-line 1)
            ;; If preceeded by a blank line and this line
            ;; is blank then go forward another line.
            (if (and blank-before
                     (looking-at "^\\s-*$"))
                (forward-line 1))
            ;; Delete the commented out line and
            ;; possibly the following blank line
            (delete-region (point) pos))
          (setq modified t))))
    (or no-message
        (if modified
            (message "Delete completed. Buffer has been modified")
          (message "Delete completed. No changes made to buffer")))
    modified))
  
(defun jx-sort-and-check-imports ()
  "Sort the import statements in the program and then check them to
comment out any that are not used."
  (interactive)
  (let ((modified (jx-sort-imports t)))
    (if (or (jx-check-imports t)
            modified)
        (message "Buffer was modified")
      (message "Buffer not modified"))))

(defun jx-imports-submenu ()
  "Create and return a keymap to use as a menu of the functions
available from jx-imports."
  (let ((jx-imports-submenu (make-sparse-keymap "Imports")))
    
    (define-key jx-imports-submenu [delete-commented-imports]
      '(menu-item "Delete Commented" jx-delete-commented-imports
		  :key-sequence nil
		  :help "Delete import statements that are commented out"))

    (define-key jx-imports-submenu [sort-and-check-imports]
      '(menu-item "Sort and Check" jx-sort-and-check-imports
		  :key-sequence nil
		  :help "Sort the import statements and check for unused ones"))
    
    (define-key jx-imports-submenu [check-imports]
      '(menu-item "Check" jx-check-imports
		  :key-sequence nil
		  :help "Check for unused import statements"))
    
    (define-key jx-imports-submenu [sort-imports]
      '(menu-item "Sort" jx-sort-imports
		  :key-sequence nil
		  :help "Sort the import statements"))

    (cons "Imports" jx-imports-submenu)))

(provide 'jx-imports)

