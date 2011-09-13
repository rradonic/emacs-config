(add-to-list 'default-frame-alist '(font . "Monospace-10"))

(require 'zenburn)
(color-theme-zenburn)

(custom-set-faces
 `(mode-line ((t (:background "gray10" :foreground "#dcdccc"))))
 `(mode-line-inactive ((t (:background "gray20" :foreground "#8c8c82"))))
 `(mode-line-highlight ((t nil)))
 `(font-lock-type ((t (:foreground ,zenburn-orange :weight bold))))
;; `(font-lock-variable-name-face ((t (:inherit default-face))))
 `(region ((t (:background "#2f2f2f"))))
 `(link ((t (:inherit custom-link)))))

(provide '_faces_zenburn)
