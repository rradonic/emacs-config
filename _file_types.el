(autoload 'clojure-mode "clojure-mode")
(autoload 'php-mode "php-mode")
(autoload 'yaml-mode "yaml-mode")

(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rxml$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . html-mode))
(add-to-list 'auto-mode-alist '("views/.*\\.php$" . html-mode))

(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(provide '_file_types)
