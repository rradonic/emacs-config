(add-to-list 'default-frame-alist '(font . "Monospace-10"))

(require 'zenburn)
(color-theme-zenburn)

(custom-set-faces
 `(mode-line ((t (:background "#1e2320" :foreground "#acbc90"))))
 `(mode-line-inactive ((t (:background "#2e3330" :foreground "#88b090"))))
 `(mode-line-highlight ((t nil)))
 `(font-lock-type ((t (:foreground ,zenburn-orange :weight bold))))
 ;; `(font-lock-function-name ((t (:weight bold))))
 ;; `(nxml-processing-instruction-content ((t (:foreground ,zenburn-blue))))
 ;; `(nxml-processing-instruction-delimiter ((t (:foreground ,zenburn-blue))))
 ;; `(nxml-processing-instruction-target ((t (:foreground ,zenburn-blue))))
 ;; `(nxml-entity-ref-delimiter ((t nil)))
 ;; `(nxml-entity-ref-name ((t nil)))

 ;; `(mumamo-border-face-in ((t (:foreground ,zenburn-fg :underline nil))))
 ;; `(mumamo-border-face-out ((t (:foreground ,zenburn-fg :underline nil))))

 ;;'(tex-verbatim ((t nil)))
 )
;;(setq tex-fontify-script nil)

(provide '_faces_zenburn)
