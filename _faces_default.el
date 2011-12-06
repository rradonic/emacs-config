(add-to-list 'default-frame-alist '(font . "Monospace-10"))

(custom-set-faces
 '(region ((t (:background "#8eaac3" :foreground "#ffffff"))))

 '(mode-line ((t (:background "#333333" :foreground "#d9d9d9"))))
 '(mode-line-inactive ((t (:background "#d9d9d9" :foreground "Gray45"))))
 '(mode-line-highlight ((t (:box nil))))

 '(font-lock-function-name-face ((t (:inherit default))))
 '(font-lock-variable-name-face ((t (:inherit default)))))

(setq custom-raised-buttons nil)

(provide '_faces_default)
