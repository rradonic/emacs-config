;;; jx-template.el --- elisp functions creating and inserting "templates"

;; Copyright (C) 2001, 2002 Alex Moffat
;; Keywords: java
;; Version: 1.0
;; CVS: $Id: jx-template.el,v 1.3 2002/03/25 03:26:49 alexmoffat Exp $

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
;; Allow you to create and insert "templates" of commonly used code.
;;
;; Installation:
;;
;; Make the file accessible somewhere on you load path.
;;
;; Customization:
;;
;; None
;;
;; Change log:
;;
;; 20020305 ajm Provide menu through a function.
;;
;; 20020324 ajm Moved provide to bottom of file
;;

;; Need the utility functions
(require 'jx-util)

(defvar jx-template-variable-id
  1
  "The id used to keep variable defs unique")

(defvar jx-template-dir
  "~/emacs/templates"
  "The location for templates")

(defun looks-like-expression (s)
  ""
  (string-match "^\\s-*(.*)\\s-*$" s))

(defun jx-next-variable-id ()
  "Return next id to use"
  (let ((i jx-template-variable-id))
    (setq jx-template-variable-id (+ jx-template-variable-id 1))
    i))

(defun jx-file-name-for-template (name)
  "Return the file name to use for template NAME"
  (expand-file-name (concat name ".tpl") jx-template-dir))
  
(defun jx-templatize (beg end)
  "Create a template from the current region."
  (interactive "r")
  (let ((code-to-template (buffer-substring-no-properties beg end))
        (template-buffer (generate-new-buffer "*jx-template*")))
    (switch-to-buffer template-buffer)
    (insert code-to-template)
    (jxtemplate-mode)))

(defun jx-mark-text (beg end)
  "Mark the current region as something that will be replaced by a
variable in the template. The region can not include any text that is
currently showing a variable. If it includes any text that is already
marked as a variable then the information from the first such section
of text is applied to the whole of the new region."
  (interactive "r")
  ;; If the first character in the region is marked with as showing a
  ;; variable definition or if a variable definition is visible
  ;; anywhere in the region don't proceed.
  (if (or (get-text-property beg 'jx-template-text)
	  (not (eq (next-single-property-change beg 'jx-template-text
						(current-buffer) end)
		   end)))
      (error "The region overlaps a currently displayed variable")
    ;; See if there is any text marked as a variable and if there is
    ;; record the variable definition, otherwise just use the next
    ;; number.
    (let ((variable (or (get-text-property beg 'jx-template-variable)
			(let ((pos (next-single-property-change beg
								'jx-template-variable
								(current-buffer) end)))
			  (if (not (eq pos end))
			      (get-text-property pos 'jx-template-variable)
			    (jx-next-variable-id))))))
      (add-text-properties beg end (list
				    'jx-template-variable variable
				    'face (if (numberp variable)
					      '(:foreground "red")
					    '(:foreground "green"))
				    'help-echo 'jx-echo-function)))))

(defun jx-unmark-text ()
  "Clear the properties that identify the region that point is in or
the mouse is over as an area that has a variable definition."
  (interactive)
  (let* ((where (jx-mouse-pos-or-point))
         (range (jx-template-range where 'jx-template-variable)))
    (if (not (null range))
        (remove-text-properties (car range)
                                (cdr range)
                                (list
                                 'jx-template-variable nil
                                 'face nil
                                 'help-echo nil)))))

(defun jx-show-variable ()
  "Show the variable def"
  (interactive)
  (let* ((where (jx-mouse-pos-or-point))
         (range (jx-template-range where 'jx-template-variable)))
    (if (not (null range))
        (let* ((beg (car range))
               (end (cdr range))
               (current-text (buffer-substring-no-properties beg end))
               (variable (get-text-property beg 'jx-template-variable)))
          (goto-char beg)
          (delete-region beg end)
          (insert ">")
          (add-text-properties (- (point) 1) (point)
                               (list
                                'read-only t
                                'rear-nonsticky (list 'read-only)))
          (insert (if (numberp variable)
                      ""
                    variable))
          (insert "<")
          (add-text-properties (- (point) 1) (point)
                               (list
                                'read-only t
                                'rear-nonsticky t))
          (let ((inhibit-read-only t))
            (add-text-properties beg
                                 (point)
                                 (list
                                  'jx-template-text current-text
                                  'face '(:foreground "blue")
                                  'help-echo 'jx-echo-function)))))))

(defun jx-show-text ()
  "Show the template text"
  (interactive)
  (let* ((where (jx-mouse-pos-or-point))
         (range (jx-template-range where 'jx-template-text)))
    (if (not (null range))
        (let* ((beg (car range))
               (end (cdr range))
               (variable (substring (buffer-substring-no-properties beg end)
                                    1 (- end beg 1)))
               (text (get-text-property beg 'jx-template-text)))
          (goto-char beg)
          (let ((inhibit-read-only t))
            (delete-region beg end)
            (insert text)
            (add-text-properties beg
				 (point)
				 (list
				  'jx-template-variable (if (eq (length variable) 0)
							    (jx-next-variable-id)
							  variable)
				  'face (if (eq (length variable) 0)
					    '(:foreground "red")
					  '(:foreground "green"))
				  'help-echo 'jx-echo-function)))))))

(defun jx-mouse-pos-or-point ()
  "Return where last mouse-1 was or point"
  (if (equal (event-basic-type last-nonmenu-event) 'mouse-1)
      (posn-point (event-end last-nonmenu-event))
    (point)))

(defun jx-template-range (where property)
  "Return cons of start and end of text with same property around
where. Start is first character with property, end is one after the
last character with the property."
  (let ((val (get-text-property where property)))
    (if val
        (let ((beg (if (eq where (point-min))
                       where
                     (if (not (equal (get-text-property (- where 1) property) val))
                         where
                       (previous-single-property-change where property
							(current-buffer) (point-min)))))
              (end (if (eq where (point-max))
                       where
                     (if (not (equal (get-text-property (+ where 1) property) val))
                         (+ where 1)
                       (next-single-property-change where property
						    (current-buffer) (point-max))))))
          (cons beg end)))))

(defun jx-read-template-from-file (name)
  "Read the template NAME from the file where it is stored and return
it as a list. Throws an error if the file containing the template
can't be found."
  (let* ((template-pieces nil)
         (template-file (jx-file-name-for-template name))
         (template-buffer (get-file-buffer template-file))
         (template-buffer-new (not template-buffer)))
    (if template-buffer-new
        (if (and (file-exists-p template-file)
                 (file-readable-p template-file))
            (setq template-buffer (find-file-noselect template-file nil t))
          (error "Can not find file %s for template %s" template-file name)))
    (save-excursion
      (set-buffer template-buffer)
      (goto-char (point-min))
      (setq template-pieces (read (current-buffer))))
    (if template-buffer-new
        (kill-buffer template-buffer))
    template-pieces))
   
(defun jx-split-template-text ()
  "Splits the template text in the current buffer into a list based on
changes in the value of the jx-template-variable property."
  (goto-char (point-min))
  (let* ((pieces '())
         (beg (point))
         (end (next-single-property-change beg 'jx-template-variable)))
    (while end
      (setq pieces (cons (buffer-substring beg end) pieces))
      (setq beg end)
      (setq end (next-single-property-change beg 'jx-template-variable)))
    (setq pieces (cons (buffer-substring beg (point-max)) pieces))
    (nreverse pieces)))

(defun jx-save-template (name)
  "Save the current buffer as a template called NAME."
  (interactive "sTemplate name: ")
  (let ((file-name (jx-file-name-for-template name)))
    (if (or (not (file-exists-p file-name))
	    (y-or-n-p "Template exists, replace it? "))
	(let ((template-pieces (jx-split-template-text)))
	  (with-temp-file file-name
	    (prin1 (mapcar (function
			    (lambda (x)
			      (let ((var (get-text-property 0 'jx-template-variable x))
				    (prompt nil))
				(if (and (not (null var))
					 (not (looks-like-expression var))
					 (string-match "^\\s-*\\(\\S-+\\)\\s-+\\(.+\\)\\s-*$" var))
				    (progn
				      (setq prompt (substring var (match-beginning 2)
							      (match-end 2)))
				      (setq var (substring var (match-beginning 1)
							   (match-end 1)))))
				(set-text-properties 0 (length x) nil x)
				(list 'text x
				      'var var
				      'prompt prompt))))
			   template-pieces)
		   (current-buffer)))))))

(defun jx-load-template (name)
  "Load the template NAME from the templates directory."
  (interactive "sTemplate name: ")
  (let ((new-buffer (generate-new-buffer (concat "*jx-template " name "*")))
	(old-buffer (current-buffer)))
    (condition-case err
	(progn
	  (switch-to-buffer new-buffer)
	  (mapc (function (lambda (x)
			    (let* ((pos (point))
				   (prompt (plist-get x 'prompt))
				   (var (if prompt
					    (concat (plist-get x 'var) " " prompt)
					  (plist-get x 'var))))
			    (insert (plist-get x 'text))
			    (if var
				(add-text-properties pos (point)
						     (list
						      'jx-template-variable var
						      'face (if (numberp var)
								'(:foreground "red")
							      '(:foreground "green"))
						      'help-echo 'jx-echo-function))))))
	      (jx-read-template-from-file name))
	  (jxtemplate-mode))
      (error
       (progn
	 (switch-to-buffer old-buffer)
	 (kill-buffer new-buffer)
	 (error (error-message-string err)))))))

(defun jx-insert-template (name)
  "Save a template to the templates directory."
  (interactive "sTemplate name: ")
  (let* ((template-pieces
	  (jx-read-template-from-file name))
         (vars-and-prompts
	  (jx-filter-unique (function
			     (lambda (a b)
			       (equal (car a) (car b))))
			    (jx-filtered-map (function
					      (lambda (x)
						(let ((var (plist-get x 'var)))
						  (if (and var
							   (not (looks-like-expression var)))
						      (cons var
							    (if (plist-get x 'prompt)
								(plist-get x 'prompt)
							      var))))))
					     template-pieces)))
         (vars
	  (mapcar (function
		   (lambda (x)
		     ;; intern so that (read var) below finds the
		     ;; correct symbol. the value is only bound within
		     ;; the generated lambda so no conflict with other code
		     (intern (car x))))
		  vars-and-prompts))
         (interactive
	  (mapconcat (function
		      (lambda (x)
			(concat "s" (cdr x) ": ")))
		     vars-and-prompts "\n"))
         (body
	  (mapcar (function (lambda (x)
			      (let ((var (plist-get x 'var)))
				(if (null var)
				    `(insert ,(plist-get x 'text))
				  `(insert ,(if (looks-like-expression var)
						(read var)
					      (jx-find-member (function
							       (lambda (symbol name)
								 (equal (symbol-name symbol)
									name)))
							      var vars)))))))
		  template-pieces)))
    (call-interactively (eval `(lambda (,@vars)
                                 (interactive ,interactive)
                                 ,@body)))))
    
(defun jx-echo-function (win obj pos)
  "Display the template variable definition or text depending on the
properties on the text the mouse is over."
  (let ((id (get-text-property pos 'jx-template-variable obj)))
    (if (not (null id))
        (if (numberp id)
            (format "%d" id)
          (format "%s" id))
      (setq id (get-text-property pos 'jx-template-text obj))
      (if (not (null id))
          (format "%s" id)
        nil))))

(defvar jx-template-popup-menu
  (make-sparse-keymap "Templates")
  "")

(define-key jx-template-popup-menu [jx-save-template]
  '(menu-item "Save template" jx-save-template
	      :keys "s"))

(define-key jx-template-popup-menu [jx-show-text]
  '(menu-item "Show text" jx-show-text
              :keys "t"))

(define-key jx-template-popup-menu [jx-show-variable]
  '(menu-item "Show variable" jx-show-variable
              :keys "v"))

(define-key jx-template-popup-menu [jx-unmark-text]
  '(menu-item "Unmark" jx-unmark-text
              :keys "u"))

(define-key jx-template-popup-menu [jx-mark-text]
  '(menu-item "Mark" jx-mark-text
              :keys "m"))
  
(define-derived-mode jxtemplate-mode
  fundamental-mode "jxtemplate"
  "Major mode for editing templates"

  (define-key jxtemplate-mode-map [M-down-mouse-1]
    jx-template-popup-menu)
  
  (define-key jxtemplate-mode-map "\C-c\C-fm"
    jx-template-popup-menu))

(defun jx-templates-submenu ()
  "Create and return a keymap to use as a menu to provide access to
the functions in jx-template.el"
  (let ((jx-templates-submenu (make-sparse-keymap "Templates")))

    (define-key jx-templates-submenu [make-template]
      '(menu-item "Create" jx-templatize
		  :key-sequence nil
		  :help "Create a new template from the current region"))
    
    (define-key jx-templates-submenu [edit-template]
      '(menu-item "Edit" jx-load-template
		  :key-sequence nil
		  :help "Load an existing template for editing into a new buffer"))
    
    (define-key jx-templates-submenu [insert-template]
      '(menu-item "Insert" jx-insert-template
		  :key-sequence nil
		  :help "Insert a template at the current position of point"))

    (cons "Templates" jx-templates-submenu)))

(provide 'jx-template)

