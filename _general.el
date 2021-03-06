(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 100))

(add-to-list 'default-frame-alist '(font . "Liberation Mono-11"))

(setq inhibit-splash-screen t)

(setq frame-title-format '(buffer-file-name "%f" ("%b")))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(blink-cursor-mode -1)

(delete-selection-mode 1)
(transient-mark-mode -1)

(line-number-mode 1)
(column-number-mode 1)

(global-subword-mode 1)

(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount `(3))

(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default
 tab-stop-list
 '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100))

(setq mouse-drag-copy-region nil)
(setq select-active-regions t)
(setq x-select-enable-primary nil)
(setq x-select-enable-clipboard t)
(global-set-key [mouse-2] 'mouse-yank-primary)

(setq tramp-default-method "ssh")
(setq auto-save-default nil)

(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(global-set-key (kbd "C-x <") '(lambda nil (interactive) (scroll-right 60)))
(global-set-key (kbd "C-x >") '(lambda nil (interactive) (scroll-left 60)))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-i") 'indent-relative)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default require-final-newline t)

(setq bookmark-save-flag 1)

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)

(setq grep-find-ignored-directories
      (quote ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "log" "tmp")))

(setq tooltip-use-echo-area t)

(winner-mode 1)

(provide '_general)
