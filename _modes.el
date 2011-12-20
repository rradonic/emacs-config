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

;; syntax stuf for c++

(font-lock-add-keywords 'c++-mode
                        '(("\\<and\\>" . font-lock-keyword-face)
                          ("\\<and_eq\\>" . font-lock-keyword-face)
                          ("\\<alignas\\>" . font-lock-keyword-face)
                          ("\\<alignof\\>" . font-lock-keyword-face)
                          ("\\<asm\\>" . font-lock-keyword-face)
                          ("\\<auto\\>" . font-lock-keyword-face)
                          ("\\<bitand\\>" . font-lock-keyword-face)
                          ("\\<bitor\\>" . font-lock-keyword-face)
                          ("\\<bool\\>" . font-lock-type-face)
                          ("\\<break\\>" . font-lock-keyword-face)
                          ("\\<case\\>" . font-lock-keyword-face)
                          ("\\<catch\\>" . font-lock-keyword-face)
                          ("\\<char\\>" . font-lock-type-face)
                          ("\\<char16_t\\>" . font-lock-type-face)
                          ("\\<char32_t\\>" . font-lock-type-face)
                          ("\\<class\\>" . font-lock-keyword-face)
                          ("\\<compl\\>" . font-lock-keyword-face)
                          ("\\<const\\>" . font-lock-keyword-face)
                          ("\\<constexpr\\>" . font-lock-keyword-face)
                          ("\\<const_cast\\>" . font-lock-keyword-face)
                          ("\\<continue\\>" . font-lock-keyword-face)
                          ("\\<do\\>" . font-lock-keyword-face)
                          ("\\<decltype\\>" . font-lock-keyword-face)
                          ("\\<default\\>" . font-lock-keyword-face)
                          ("\\<delete\\>" . font-lock-keyword-face)
                          ("\\<double\\>" . font-lock-type-face)
                          ("\\<dynamic_cast\\>" . font-lock-keyword-face)
                          ("\\<else\\>" . font-lock-keyword-face)
                          ("\\<enum\\>" . font-lock-keyword-face)
                          ("\\<explicit\\>" . font-lock-keyword-face)
                          ("\\<export\\>" . font-lock-keyword-face)
                          ("\\<extern\\>" . font-lock-keyword-face)
                          ("\\<false\\>" . font-lock-constant-face)
                          ("\\<float\\>" . font-lock-type-face)
                          ("\\<for\\>" . font-lock-keyword-face)
                          ("\\<friend\\>" . font-lock-keyword-face)
                          ("\\<goto\\>" . font-lock-keyword-face)
                          ("\\<if\\>" . font-lock-keyword-face)
                          ("\\<inline\\>" . font-lock-keyword-face)
                          ("\\<int\\>" . font-lock-type-face)
                          ("\\<long\\>" . font-lock-type-face)
                          ("\\<mutable\\>" . font-lock-keyword-face)
                          ("\\<namespace\\>" . font-lock-keyword-face)
                          ("\\<new\\>" . font-lock-keyword-face)
                          ("\\<noexcept\\>" . font-lock-keyword-face)
                          ("\\<not\\>" . font-lock-keyword-face)
                          ("\\<not_eq\\>" . font-lock-keyword-face)
                          ("\\<nullptr\\>" . font-lock-constant-face)
                          ("\\<operator\\>" . font-lock-keyword-face)
                          ("\\<or\\>" . font-lock-keyword-face)
                          ("\\<or_eq\\>" . font-lock-keyword-face)
                          ("\\<private\\>" . font-lock-keyword-face)
                          ("\\<protected\\>" . font-lock-keyword-face)
                          ("\\<public\\>" . font-lock-keyword-face)
                          ("\\<register\\>" . font-lock-keyword-face)
                          ("\\<reinterpret_cast\\>" . font-lock-keyword-face)
                          ("\\<return\\>" . font-lock-keyword-face)
                          ("\\<short\\>" . font-lock-type-face)
                          ("\\<signed\\>" . font-lock-keyword-face)
                          ("\\<sizeof\\>" . font-lock-keyword-face)
                          ("\\<static\\>" . font-lock-keyword-face)
                          ("\\<static_assert\\>" . font-lock-keyword-face)
                          ("\\<static_cast\\>" . font-lock-keyword-face)
                          ("\\<struct\\>" . font-lock-keyword-face)
                          ("\\<switch\\>" . font-lock-keyword-face)
                          ("\\<template\\>" . font-lock-keyword-face)
                          ("\\<this\\>" . font-lock-keyword-face)
                          ("\\<thread_local\\>" . font-lock-keyword-face)
                          ("\\<throw\\>" . font-lock-keyword-face)
                          ("\\<true\\>" . font-lock-constant-face)
                          ("\\<try\\>" . font-lock-keyword-face)
                          ("\\<typedef\\>" . font-lock-keyword-face)
                          ("\\<typeid\\>" . font-lock-keyword-face)
                          ("\\<typename\\>" . font-lock-keyword-face)
                          ("\\<union\\>" . font-lock-keyword-face)
                          ("\\<unsigned\\>" . font-lock-keyword-face)
                          ("\\<using\\>" . font-lock-keyword-face)
                          ("\\<virtual\\>" . font-lock-keyword-face)
                          ("\\<void\\>" . font-lock-type-face)
                          ("\\<volatile\\>" . font-lock-keyword-face)
                          ("\\<wchar_t\\>" . font-lock-type-face)
                          ("\\<while\\>" . font-lock-keyword-face)
                          ("\\<xor\\>" . font-lock-keyword-face)
                          ("\\<xor_eq\\>" . font-lock-keyword-face)))

(setq c++-font-lock-extra-types nil)

(setq font-lock-maximum-decoration
      '((c++-mode . 1)
        (t . t)))

(provide '_modes)
