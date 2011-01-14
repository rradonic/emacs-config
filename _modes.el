(setq rng-nxml-auto-validate-flag nil)
(setq nxml-child-indent 4)
(setq ruby-insert-encoding-magic-comment nil)

(defun set-comment-region-keys ()
  (local-set-key (kbd "C-c c") 'comment-region)
  (local-set-key (kbd "C-c u") 'uncomment-region))

(dolist (mode-hook '(nxhtml-mode-hook
                     ruby-mode-hook
                     c-mode-hook
                     nxml-mode-hook
                     yaml-mode-hook
                     sh-mode-hook
                     lisp-mode-hook
                     emacs-lisp-mode-hook
                     lisp-interaction-mode-hook))
  (add-hook mode-hook 'set-comment-region-keys)
  (add-hook mode-hook (lambda nil (subword-mode 1))))

(dolist (mode-hook '(c-mode-hook))
  (add-hook mode-hook (lambda nil (c-set-style "stroustrup"))))

;; disable ruby's specific C-j binding, make it do just newline-and-indent instead
;; of indent,newline,indent
(add-hook 'ruby-mode-hook (lambda nil (local-set-key (kbd "C-j") 'newline-and-indent)))

(setq grep-find-ignored-directories
      (quote ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "dependencies" "log")))

(setq ibuffer-formats
      (quote ((mark modified read-only "  " (name 30 30 :left :elide) "  " filename-and-process)
              (mark "  " (name 30 30) "  " filename))))
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("*star*" (name . "\\*.*\\*")))
              ("cpp"
               ("source files" (filename . ".*\\.cpp"))
               ("header files" (filename . ".*\\.hpp"))
               ("resources" (filename . ".*/resources/.*"))
               ("*star*" (name . "\\*.*\\*")))
              ("rails"
               ("*star*" (name . "\\*.*\\*"))
               ("controllers" (filename . ".*controllers/.*"))
               ("views" (filename . ".*views/.*"))
               ("models" (filename . ".*models/.*"))
               ("lib" (filename . ".*/lib/.*"))
               ("migrations" (filename . ".*migrate/.*"))
               ("configuration" (filename . ".*config/.*"))
               ("translations" (filename . ".*locales/.*"))
               ("fixtures" (filename . ".*fixtures/.*"))
               ("tests" (filename . ".*test/.*"))
               ("logs" (filename . ".*log/.*")))
              ("kohana"
               ("*star*" (name . "\\*.*\\*"))
               ("controllers" (filename . ".*controller/.*"))
               ("views" (filename . ".*views/.*"))
               ("models" (filename . ".*model/.*"))
               ("lib" (filename . ".*/lib/.*"))
               ("configuration" (filename . ".*config/.*"))
               ("i18n" (filename . ".*i18n/.*"))
               ("logs" (filename . ".*logs/.*"))))))

(provide '_modes)
