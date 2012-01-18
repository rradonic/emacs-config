(setq rng-nxml-auto-validate-flag nil)
(setq c-basic-offset 4)
(setq nxml-child-indent 4)
(setq sgml-basic-offset 4)
(setq ruby-insert-encoding-magic-comment nil)

;; comment region keys that work regardless of whether or not the region is active
(defun set-comment-region-keys ()
  (local-set-key (kbd "C-c c") 'comment-region)
  (local-set-key (kbd "C-c u") 'uncomment-region))

(dolist (mode-hook '(nxhtml-mode-hook
                     ruby-mode-hook
                     c-mode-hook
                     c++-mode-hook
                     clojure-mode-hook
                     cmake-mode-hook
                     html-mode-hook
                     nxml-mode-hook
                     yaml-mode-hook
                     sh-mode-hook
                     python-mode-hook
                     lisp-mode-hook
                     emacs-lisp-mode-hook
                     lisp-interaction-mode-hook))
  (add-hook mode-hook 'set-comment-region-keys))

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (c++-mode . "linux")
        (c++-mode . "linux")
        (other . "gnu")))

;; php-mode overrides the settings in c-default-style, so we need to enforce them
(add-hook 'php-mode-hook (lambda nil (c-set-style "linux")))

(setq inferior-lisp-program "/opt/leiningen-1.6.1/lein repl")
(add-hook 'emacs-lisp-mode-hook (lambda nil (local-set-key (kbd "C-c C-e") 'eval-last-sexp)))

(setq ibuffer-formats
      (quote ((mark modified read-only "  " (name 20 20 :left :elide) "  " filename-and-process)
              (mark "  " (name 30 30) "  " filename))))
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("*emacs*" (or (mode . dired-mode)
                              (name . "\\*.*\\*"))))
              ("cpp"
               ("*emacs*" (or (mode . dired-mode)
                              (name . "\\*.*\\*")))
               ("headers" (filename . ".*\.hpp"))
               ("source" (filename . ".*\.cpp")))
              ("rails"
               ("*emacs*" (or (mode . dired-mode)
                              (name . "\\*.*\\*")))
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
               ("*emacs*" (or (mode . dired-mode)
                              (name . "\\*.*\\*")))
               ("controllers" (filename . ".*/controller/.*"))
               ("views" (filename . ".*/views/.*"))
               ("models" (filename . ".*/model/.*"))
               ("migrations" (filename . ".*/migrations/.*"))
               ("fixtures" (filename . ".*/fixtures/.*"))
               ("tests" (filename . ".*/tests/.*"))
               ("configuration" (filename . ".*/config/.*"))
               ("i18n" (filename . ".*/i18n/.*"))
               ("logs" (filename . ".*/logs/.*"))
               ("lib" (filename . ".*/lib/.*"))))))

(add-hook 'ibuffer-mode-hook (lambda nil (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-show-empty-filter-groups nil)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-merge-split-window-function 'split-window-horizontally)
(setq-default ediff-auto-refine 'off)
(setq-default ediff-keep-variants nil)

(provide '_modes)
