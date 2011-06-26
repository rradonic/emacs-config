(setq rng-nxml-auto-validate-flag nil)
(setq c-basic-offset 4)
(setq nxml-child-indent 4)
(setq sgml-basic-offset 4)
(setq ruby-insert-encoding-magic-comment nil)

(defun set-comment-region-keys ()
  (local-set-key (kbd "C-c c") 'comment-region)
  (local-set-key (kbd "C-c u") 'uncomment-region))

(dolist (mode-hook '(nxhtml-mode-hook
                     ruby-mode-hook
                     c-mode-hook
                     c++-mode-hook
                     cmake-mode-hook
                     nxml-mode-hook
                     yaml-mode-hook
                     sh-mode-hook
                     python-mode-hook
                     lisp-mode-hook
                     emacs-lisp-mode-hook
                     lisp-interaction-mode-hook))
  (add-hook mode-hook 'set-comment-region-keys)
  (add-hook mode-hook (lambda nil (subword-mode 1))))

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (c++-mode . "stroustrup")
        (other . "gnu")))

;; disable ruby's specific C-j binding, make it do just newline-and-indent instead
;; of indent,newline,indent
(add-hook 'ruby-mode-hook (lambda nil (local-set-key (kbd "C-j") 'newline-and-indent)))

;; php-mode overrides the settings in c-default-style, so we need to enforce them
(add-hook 'php-mode-hook (lambda nil (c-set-style "stroustrup")))

(setq ibuffer-formats
      (quote ((mark modified read-only "  " (name 30 30 :left :elide) "  " filename-and-process)
              (mark "  " (name 30 30) "  " filename))))
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("*star*" (name . "\\*.*\\*")))
              ("cpp"
               ("*star*" (name . "\\*.*\\*"))
               ("source files" (filename . ".*\\.cpp"))
               ("header files" (filename . ".*\\.hpp"))
               ("resources" (filename . ".*/resources/.*")))
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
               ("controllers" (filename . ".*/controller/.*"))
               ("views" (filename . ".*/views/.*"))
               ("models" (filename . ".*/model/.*"))
               ("fixtures" (filename . ".*/fixtures/.*"))
               ("tests" (filename . ".*/tests/.*"))
               ("configuration" (filename . ".*/config/.*"))
               ("i18n" (filename . ".*/i18n/.*"))
               ("logs" (filename . ".*/logs/.*"))
               ("lib" (filename . ".*/lib/.*"))))))

(provide '_modes)
