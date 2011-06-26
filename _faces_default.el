(add-to-list 'default-frame-alist '(font . "Monospace-10"))

(custom-set-faces
 '(mode-line ((t (:background "gray20" :foreground "gray90"))))
 '(mode-line-inactive ((t (:background "gray85" :foreground "gray40"))))
 '(mode-line-highlight ((t (:box nil))))

 '(sh-quoted-exec ((t (:inherit font-lock-string-face)))))

(provide '_faces_default)
