(require 'color-theme-zenburn)
(color-theme-zenburn)

(custom-set-faces
 `(mode-line ((t (:background "gray10" :foreground "#dcdccc"))))
 `(mode-line-inactive ((t (:background "gray20" :foreground "#8c8c82"))))
 `(mode-line-highlight ((t nil)))

 `(isearch ((t (:background "#668b8b" :foreground ,zenburn-fg))))
 `(lazy-highlight ((t (:background "#495766" :foreground ,zenburn-fg))))

 `(font-lock-keyword-face ((t (:foreground ,zenburn-yellow))))
 `(font-lock-type-face ((t (:foreground ,zenburn-orange))))
 `(font-lock-variable-name-face ((t (:inherit default))))

 `(diff-file-header ((t (:background ,zenburn-bg+1 :bold t))))

 `(ediff-current-diff-A ((t (:background "#495766" :foreground ,zenburn-fg))))
 `(ediff-current-diff-Ancestor ((t (:background "#495766" :foreground ,zenburn-fg))))
 `(ediff-current-diff-B ((t (:background "#495766" :foreground ,zenburn-fg))))
 `(ediff-current-diff-C ((t (:background "#495766" :foreground ,zenburn-fg))))

 `(ediff-even-diff-A ((t (:background ,zenburn-bg+1))))
 `(ediff-even-diff-Ancestor ((t (:background ,zenburn-bg+1))))
 `(ediff-even-diff-B ((t (:background ,zenburn-bg+1))))
 `(ediff-even-diff-C ((t (:background ,zenburn-bg+1))))

 `(ediff-odd-diff-A ((t (:background ,zenburn-bg+1))))
 `(ediff-odd-diff-Ancestor ((t (:background ,zenburn-bg+1))))
 `(ediff-odd-diff-B ((t (:background ,zenburn-bg+1))))
 `(ediff-odd-diff-C ((t (:background ,zenburn-bg+1))))

 `(ediff-fine-diff-A ((t (:background "#668b8b"))))
 `(ediff-fine-diff-Ancestor ((t (:background "#668b8b" :foreground ,zenburn-fg))))
 `(ediff-fine-diff-B ((t (:background "#668b8b" :foreground ,zenburn-fg))))
 `(ediff-fine-diff-C ((t (:background "#668b8b" :foreground ,zenburn-fg)))))

(provide '_faces_zenburn)
