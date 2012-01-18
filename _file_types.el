(autoload 'clojure-mode "clojure-mode")
(autoload 'php-mode "php-mode")
(autoload 'yaml-mode "yaml-mode")

(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(provide '_file_types)
