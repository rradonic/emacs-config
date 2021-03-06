(custom-set-faces
 '(mode-line ((t (:background "#000000" :foreground "#ffffff"))))
 '(mode-line-inactive ((t (:background "#e5e5e5" :foreground "#737373"))))
 '(mode-line-highlight ((t (:box nil))))

 '(fringe ((t (:background "#e5e5e5"))))

 '(region ((t (:background "#999999" :foreground "#ffffff"))))
 '(isearch ((t (:background "#cd00cd" :foreground "#ffffff"))))
 '(lazy-highlight ((t (:background "#afeeee" :foreground "Black"))))

 '(font-lock-comment-face ((t (:foreground "#a5a5a5"))))
 '(font-lock-doc-face ((t (:foreground "#a5a5a5"))))

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
 `(ediff-fine-diff-C ((t (:background "#f2f2f2")))))

(provide '_faces_default)
