(setq inhibit-splash-screen t)

(setq frame-title-format '(buffer-file-name "%f" ("%b")))

(add-to-list 'default-frame-alist '(height . 65))
(add-to-list 'default-frame-alist '(width . 120))

;; (add-to-list 'default-frame-alist '(alpha 95 95))

(setq tooltip-use-echo-area t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; (set-scroll-bar-mode `right)

(blink-cursor-mode -1)
;; (setq default-cursor-type '(bar . 1))
;; (setq default-cursor-in-non-selected-windows 'nil)

;; (cua-mode 1)
;; (setq cua-enable-cua-keys nil)
;; (setq cua-virtual-rectangle-edges nil)
(delete-selection-mode 1)
(transient-mark-mode -1)

(line-number-mode 1)
(column-number-mode 1)
;; (size-indication-mode 1)

(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount `(3))

(setq-default truncate-lines t)
(setq-default fill-column 100)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

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

(global-set-key (kbd "C-x <") 'scroll-right)
(global-set-key (kbd "C-x >") 'scroll-left)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-i") 'indent-relative)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-auto-refine 'off)
(setq-default ediff-autostore-merges nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default require-final-newline t)

(setq bookmark-save-flag 1)

(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)
;; (remove-hook 'kill-emacs-query-functions 'server-kill-emacs-query-function)
;; (remove-hook 'delete-frame-functions 'server-handle-delete-frame)

(defalias 'yes-or-no-p 'y-or-n-p)

(provide '_general)
