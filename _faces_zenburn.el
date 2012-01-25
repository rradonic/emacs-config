(require 'color-theme-zenburn)
(color-theme-zenburn)

(custom-set-faces
 `(mode-line ((t (:background "gray10" :foreground "#dcdccc"))))
 `(mode-line-inactive ((t (:background "gray20" :foreground "#8c8c82"))))
 `(mode-line-highlight ((t nil)))
 `(font-lock-type-face ((t (:foreground ,zenburn-orange))))
 `(font-lock-variable-name-face ((t (:inherit default)))))

(provide '_faces_zenburn)
