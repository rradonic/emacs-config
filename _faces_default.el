(add-to-list 'default-frame-alist '(font . "Monospace-10"))

(require 'zenburn)
(color-theme-zenburn)

(custom-set-faces
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; these are for the default color scheme (under construction)
 ;; '(mode-line ((t (:background "gray70"))))
 ;; '(mode-line-inactive ((t (:background "gray85" :foreground "gray50"))))
 ;; '(mode-line-highlight ((t (:box nil))))
 ;; '(region ((t (:background "#97bf60" :foreground "#ffffff"))))
 ;; '(cursor ((t (:background "gray20"))))

 ;; `(ediff-current-diff-A ((t (:background "#495766" :foreground ,zenburn-fg))))
 ;; `(ediff-current-diff-Ancestor ((t (:background "#495766" :foreground ,zenburn-fg))))
 ;; `(ediff-current-diff-B ((t (:background "#495766" :foreground ,zenburn-fg))))
 ;; `(ediff-current-diff-C ((t (:background "#495766" :foreground ,zenburn-fg))))

 ;; `(ediff-even-diff-A ((t (:foreground "white"))))
 ;; `(ediff-even-diff-Ancestor ((t (:foreground "white"))))
 ;; `(ediff-even-diff-B ((t (:foreground "white"))))
 ;; `(ediff-even-diff-C ((t (:foreground "white"))))

 ;; `(ediff-odd-diff-A ((t (:foreground "white"))))
 ;; `(ediff-odd-diff-Ancestor ((t (:foreground "white"))))
 ;; `(ediff-odd-diff-B ((t (:foreground "white"))))
 ;; `(ediff-odd-diff-C ((t (:foreground "white"))))

 ;; `(ediff-fine-diff-A ((t (:background "#668b8b"))))
 ;; `(ediff-fine-diff-Ancestor ((t (:background "#668b8b" :foreground ,zenburn-fg))))
 ;; `(ediff-fine-diff-B ((t (:background "#668b8b" :foreground ,zenburn-fg))))
 ;; `(ediff-fine-diff-C ((t (:background "#668b8b" :foreground ,zenburn-fg))))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; these are for zenburn
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

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; these are always in use
 ;;'(tex-verbatim ((t nil)))
 )
;;(setq tex-fontify-script nil)

(setq mumamo-chunk-coloring 5)

(provide '_faces)
