(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/clojure-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/color-theme"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/php-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/yaml-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/zenburn-emacs"))

(require '_general)
(require '_faces_default)
;; (require '_faces_zenburn)
(require '_modes)
(require '_file_types)
