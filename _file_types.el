(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rxml$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

;; use html mode instead of nxhtml mode for html and similar files
;; (add-to-list 'auto-mode-alist '("\\.html$" . html-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . html-mumamo-mode))
(add-to-list 'auto-mode-alist '("views/.*\\.php$" . html-mumamo-mode))

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(provide '_file_types)
