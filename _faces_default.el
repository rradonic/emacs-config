(custom-set-faces
 '(region ((t (:background "#8eaac3" :foreground "#ffffff"))))
 '(isearch ((t (:background "Magenta3" :foreground "#ffffff"))))
 '(lazy-highlight ((t (:background "PaleTurquoise" :foreground "Black"))))

 '(font-lock-keyword-face ((t (:foreground "DarkCyan"))))
 '(font-lock-comment-face ((t (:foreground "Gray60"))))
 '(font-lock-constant-face ((t (:foreground "VioletRed4"))))
 '(font-lock-function-name-face ((t (:inherit default))))
 '(dired-directory ((t (:foreground "Blue1"))))
 '(dired-header ((t (:inherit default))))

 '(diff-header ((t (:background "#e5e5e5"))))
 '(diff-file-header ((t (:inherit diff-header :weight bold))))
 '(diff-hunk-header ((t (:inherit diff-header))))
 '(diff-context ((t (:inherit font-lock-comment-face))))
 '(diff-removed ((t (:background "#ffe4e1"))))
 '(diff-added ((t (:background "#d9eed9"))))
 '(diff-refine-change ((t (:background "#beeeee"))))

 `(ediff-current-diff-A ((t (:background "#ffe4e1"))))
 `(ediff-current-diff-Ancestor ((t (:background "#f2f2f2"))))
 `(ediff-current-diff-B ((t (:background "#d9eed9"))))
 `(ediff-current-diff-C ((t (:background "#f2f2f2"))))

 `(ediff-even-diff-A ((t (:background "#f2f2f2"))))
 `(ediff-even-diff-Ancestor ((t (:background "#f2f2f2"))))
 `(ediff-even-diff-B ((t (:background "#f2f2f2"))))
 `(ediff-even-diff-C ((t (:background "#f2f2f2"))))

 `(ediff-odd-diff-A ((t (:background "#f2f2f2"))))
 `(ediff-odd-diff-Ancestor ((t (:background "#f2f2f2"))))
 `(ediff-odd-diff-B ((t (:background "#f2f2f2"))))
 `(ediff-odd-diff-C ((t (:background "#f2f2f2"))))

 `(ediff-fine-diff-A ((t (:background "#beeeee"))))
 `(ediff-fine-diff-Ancestor ((t (:background "#f2f2f2"))))
 `(ediff-fine-diff-B ((t (:background "#beeeee"))))
 `(ediff-fine-diff-C ((t (:background "#f2f2f2"))))
 )

(setq ibuffer-title-face nil)

(provide '_faces_default)
