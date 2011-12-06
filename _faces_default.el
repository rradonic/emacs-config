(add-to-list 'default-frame-alist '(font . "Monospace-10"))

(custom-set-faces
 ;; '(default ((t (:foreground "#333333"))))
 ;; '(cursor ((t (:background "#333333"))))
 '(region ((t (:background "#8eaac3" :foreground "#ffffff"))))

 '(mode-line ((t (:background "#333333" :foreground "#d9d9d9"))))
 '(mode-line-inactive ((t (:background "#d9d9d9" :foreground "Gray45"))))
 '(mode-line-highlight ((t (:box nil))))

 ;; ;; '(isearch ((t (:background "#b39a6b" :foreground "#ffffff"))))
 ;; ;; '(lazy-highlight ((t (:background "#f2eada" :foreground "#333333"))))

 ;; '(dired-directory ((t (:foreground "#436eee"))))
 ;; '(dired-symlink ((t (:inherit default))))

 ;; '(link ((t (:foreground "#436eee" :underline t))))
 ;; '(custom-group-tag ((t (:foreground "#436eee" :weight bold))))
 ;; '(custom-variable-tag ((t (:foreground "#436eee" :weight bold))))

 ;; '(font-lock-keyword-face ((t (:foreground "#436eee"))))
 ;; '(font-lock-string-face ((t (:foreground "#cd8c95"))))
 '(font-lock-function-name-face ((t (:inherit default))))
 '(font-lock-variable-name-face ((t (:inherit default))))
 ;; '(font-lock-comment-face ((t (:foreground "Gray60"))))

 ;; ;; '(font-lock-type-face ((t (:inherit font-lock-keyword-face))))
 ;; ;; '(font-lock-constant-face ((t (:inherit default))))

 ;; '(diff-header ((t (:background "#ffe4c4"))))
 ;; '(diff-file-header ((t (:inherit diff-header :weight bold))))
 ;; '(diff-hunk-header ((t (:inherit diff-header))))
 ;; '(diff-context ((t (:inherit font-lock-comment-face))))
 ;; '(diff-removed ((t (:background "#ffe4e1"))))
 ;; '(diff-added ((t (:background "#d9eed9"))))
 ;; '(diff-refine-change ((t (:background "#bbffff" :foreground "#333333"))))

 ;; `(ediff-current-diff-A ((t (:background "#ffe4e1" :foreground "#333333"))))
 ;; `(ediff-current-diff-Ancestor ((t (:background "#f2f2f2" :foreground "#333333"))))
 ;; `(ediff-current-diff-B ((t (:background "#d9eed9" :foreground "#333333"))))
 ;; `(ediff-current-diff-C ((t (:background "#f2f2f2" :foreground "#333333"))))

 ;; `(ediff-even-diff-A ((t (:background "#f2f2f2" :foreground "#333333"))))
 ;; `(ediff-even-diff-Ancestor ((t (:background "#f2f2f2" :foreground "#333333"))))
 ;; `(ediff-even-diff-B ((t (:background "#f2f2f2" :foreground "#333333"))))
 ;; `(ediff-even-diff-C ((t (:background "#f2f2f2" :foreground "#333333"))))

 ;; `(ediff-odd-diff-A ((t (:background "#f2f2f2" :foreground "#333333"))))
 ;; `(ediff-odd-diff-Ancestor ((t (:background "#f2f2f2" :foreground "#333333"))))
 ;; `(ediff-odd-diff-B ((t (:background "#f2f2f2" :foreground "#333333"))))
 ;; `(ediff-odd-diff-C ((t (:background "#f2f2f2" :foreground "#333333"))))

 ;; `(ediff-fine-diff-A ((t (:background "#bbffff" :foreground "#333333"))))
 ;; `(ediff-fine-diff-Ancestor ((t (:background "#f2f2f2" :foreground "#333333"))))
 ;; `(ediff-fine-diff-B ((t (:background "#bbffff" :foreground "#333333"))))
 ;; `(ediff-fine-diff-C ((t (:background "#f2f2f2" :foreground "#333333"))))
 )

(setq custom-raised-buttons nil)

(provide '_faces_default)
