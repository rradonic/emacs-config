(custom-set-faces
 '(mode-line ((t (:background "#444444" :foreground "#d9d9d9"))))
 '(mode-line-inactive ((t (:background "#e5e5e5" :foreground "#737373"))))
 '(mode-line-highlight ((t (:box nil))))

 '(cursor ((t (:background "#444444"))))

 '(region ((t (:background "#909090" :foreground "#ffffff"))))
 '(isearch ((t (:background "#cd00cd" :foreground "#ffffff"))))
 '(lazy-highlight ((t (:background "#afeeee" :foreground "Black"))))

 '(diff-header ((t (:background "#e5e5e5"))))
 '(diff-file-header ((t (:inherit diff-header :weight bold))))
 '(diff-hunk-header ((t (:inherit diff-header))))
 '(diff-context ((t (:inherit font-lock-comment-face))))
 '(diff-removed ((t (:background "#ffe4e1"))))
 '(diff-added ((t (:background "#d9eed9"))))
 '(diff-refine-change ((t (:background "#beeeee"))))

 `(ediff-current-diff-A ((t (:background "#ffe4e1"))))
 `(ediff-current-diff-Ancestor ((t (:background "#f2f2f2"))))
 `(ediff-current-diff-B ((t (:background "#d9eed9"))))
 `(ediff-current-diff-C ((t (:background "#f2f2f2"))))

 `(ediff-even-diff-A ((t (:background "#f2f2f2"))))
 `(ediff-even-diff-Ancestor ((t (:background "#f2f2f2"))))
 `(ediff-even-diff-B ((t (:background "#f2f2f2"))))
 `(ediff-even-diff-C ((t (:background "#f2f2f2"))))

 `(ediff-odd-diff-A ((t (:background "#f2f2f2"))))
 `(ediff-odd-diff-Ancestor ((t (:background "#f2f2f2"))))
 `(ediff-odd-diff-B ((t (:background "#f2f2f2"))))
 `(ediff-odd-diff-C ((t (:background "#f2f2f2"))))

 `(ediff-fine-diff-A ((t (:background "#beeeee"))))
 `(ediff-fine-diff-Ancestor ((t (:background "#f2f2f2"))))
 `(ediff-fine-diff-B ((t (:background "#beeeee"))))
 `(ediff-fine-diff-C ((t (:background "#f2f2f2"))))
 )

(font-lock-add-keywords 'c++-mode
                        '(("\\<and\\>" . font-lock-keyword-face)
                          ("\\<and_eq\\>" . font-lock-keyword-face)
                          ("\\<alignas\\>" . font-lock-keyword-face)
                          ("\\<alignof\\>" . font-lock-keyword-face)
                          ("\\<asm\\>" . font-lock-keyword-face)
                          ("\\<auto\\>" . font-lock-keyword-face)
                          ("\\<bitand\\>" . font-lock-keyword-face)
                          ("\\<bitor\\>" . font-lock-keyword-face)
                          ("\\<bool\\>" . font-lock-keyword-face)
                          ("\\<break\\>" . font-lock-keyword-face)
                          ("\\<case\\>" . font-lock-keyword-face)
                          ("\\<catch\\>" . font-lock-keyword-face)
                          ("\\<char\\>" . font-lock-keyword-face)
                          ("\\<char16_t\\>" . font-lock-keyword-face)
                          ("\\<char32_t\\>" . font-lock-keyword-face)
                          ("\\<class\\>" . font-lock-keyword-face)
                          ("\\<compl\\>" . font-lock-keyword-face)
                          ("\\<const\\>" . font-lock-keyword-face)
                          ("\\<constexpr\\>" . font-lock-keyword-face)
                          ("\\<const_cast\\>" . font-lock-keyword-face)
                          ("\\<continue\\>" . font-lock-keyword-face)
                          ("\\<do\\>" . font-lock-keyword-face)
                          ("\\<decltype\\>" . font-lock-keyword-face)
                          ("\\<default\\>" . font-lock-keyword-face)
                          ("\\<delete\\>" . font-lock-keyword-face)
                          ("\\<double\\>" . font-lock-keyword-face)
                          ("\\<dynamic_cast\\>" . font-lock-keyword-face)
                          ("\\<else\\>" . font-lock-keyword-face)
                          ("\\<enum\\>" . font-lock-keyword-face)
                          ("\\<explicit\\>" . font-lock-keyword-face)
                          ("\\<export\\>" . font-lock-keyword-face)
                          ("\\<extern\\>" . font-lock-keyword-face)
                          ("\\<false\\>" . font-lock-constant-face)
                          ("\\<float\\>" . font-lock-keyword-face)
                          ("\\<for\\>" . font-lock-keyword-face)
                          ("\\<friend\\>" . font-lock-keyword-face)
                          ("\\<goto\\>" . font-lock-keyword-face)
                          ("\\<if\\>" . font-lock-keyword-face)
                          ("\\<inline\\>" . font-lock-keyword-face)
                          ("\\<int\\>" . font-lock-keyword-face)
                          ("\\<long\\>" . font-lock-keyword-face)
                          ("\\<mutable\\>" . font-lock-keyword-face)
                          ("\\<namespace\\>" . font-lock-keyword-face)
                          ("\\<new\\>" . font-lock-keyword-face)
                          ("\\<noexcept\\>" . font-lock-keyword-face)
                          ("\\<not\\>" . font-lock-keyword-face)
                          ("\\<not_eq\\>" . font-lock-keyword-face)
                          ("\\<nullptr\\>" . font-lock-keyword-face)
                          ("\\<operator\\>" . font-lock-keyword-face)
                          ("\\<or\\>" . font-lock-keyword-face)
                          ("\\<or_eq\\>" . font-lock-keyword-face)
                          ("\\<private\\>" . font-lock-keyword-face)
                          ("\\<protected\\>" . font-lock-keyword-face)
                          ("\\<public\\>" . font-lock-keyword-face)
                          ("\\<register\\>" . font-lock-keyword-face)
                          ("\\<reinterpret_cast\\>" . font-lock-keyword-face)
                          ("\\<return\\>" . font-lock-keyword-face)
                          ("\\<short\\>" . font-lock-keyword-face)
                          ("\\<signed\\>" . font-lock-keyword-face)
                          ("\\<sizeof\\>" . font-lock-keyword-face)
                          ("\\<static\\>" . font-lock-keyword-face)
                          ("\\<static_assert\\>" . font-lock-keyword-face)
                          ("\\<static_cast\\>" . font-lock-keyword-face)
                          ("\\<struct\\>" . font-lock-keyword-face)
                          ("\\<switch\\>" . font-lock-keyword-face)
                          ("\\<template\\>" . font-lock-keyword-face)
                          ("\\<this\\>" . font-lock-keyword-face)
                          ("\\<thread_local\\>" . font-lock-keyword-face)
                          ("\\<throw\\>" . font-lock-keyword-face)
                          ("\\<true\\>" . font-lock-keyword-face)
                          ("\\<try\\>" . font-lock-keyword-face)
                          ("\\<typedef\\>" . font-lock-keyword-face)
                          ("\\<typeid\\>" . font-lock-keyword-face)
                          ("\\<typename\\>" . font-lock-keyword-face)
                          ("\\<union\\>" . font-lock-keyword-face)
                          ("\\<unsigned\\>" . font-lock-keyword-face)
                          ("\\<using\\>" . font-lock-keyword-face)
                          ("\\<virtual\\>" . font-lock-keyword-face)
                          ("\\<void\\>" . font-lock-keyword-face)
                          ("\\<volatile\\>" . font-lock-keyword-face)
                          ("\\<wchar_t\\>" . font-lock-keyword-face)
                          ("\\<while\\>" . font-lock-keyword-face)
                          ("\\<xor\\>" . font-lock-keyword-face)
                          ("\\<xor_eq\\>" . font-lock-keyword-face)))

(font-lock-add-keywords 'java-mode
                        '(("\\<abstract\\>" . font-lock-keyword-face)
                          ("\\<assert\\>" . font-lock-keyword-face)
                          ("\\<boolean\\>" . font-lock-keyword-face)
                          ("\\<break\\>" . font-lock-keyword-face)
                          ("\\<byte\\>" . font-lock-keyword-face)
                          ("\\<case\\>" . font-lock-keyword-face)
                          ("\\<catch\\>" . font-lock-keyword-face)
                          ("\\<char\\>" . font-lock-keyword-face)
                          ("\\<class\\>" . font-lock-keyword-face)
                          ("\\<const\\>" . font-lock-keyword-face)
                          ("\\<continue\\>" . font-lock-keyword-face)
                          ("\\<default\\>" . font-lock-keyword-face)
                          ("\\<do\\>" . font-lock-keyword-face)
                          ("\\<double\\>" . font-lock-keyword-face)
                          ("\\<else\\>" . font-lock-keyword-face)
                          ("\\<enum\\>" . font-lock-keyword-face)
                          ("\\<extends\\>" . font-lock-keyword-face)
                          ("\\<final\\>" . font-lock-keyword-face)
                          ("\\<finally\\>" . font-lock-keyword-face)
                          ("\\<float\\>" . font-lock-keyword-face)
                          ("\\<for\\>" . font-lock-keyword-face)
                          ("\\<goto\\>" . font-lock-keyword-face)
                          ("\\<if\\>" . font-lock-keyword-face)
                          ("\\<implements\\>" . font-lock-keyword-face)
                          ("\\<import\\>" . font-lock-keyword-face)
                          ("\\<instanceof\\>" . font-lock-keyword-face)
                          ("\\<int\\>" . font-lock-keyword-face)
                          ("\\<interface\\>" . font-lock-keyword-face)
                          ("\\<long\\>" . font-lock-keyword-face)
                          ("\\<native\\>" . font-lock-keyword-face)
                          ("\\<new\\>" . font-lock-keyword-face)
                          ("\\<package\\>" . font-lock-keyword-face)
                          ("\\<private\\>" . font-lock-keyword-face)
                          ("\\<protected\\>" . font-lock-keyword-face)
                          ("\\<public\\>" . font-lock-keyword-face)
                          ("\\<return\\>" . font-lock-keyword-face)
                          ("\\<short\\>" . font-lock-keyword-face)
                          ("\\<static\\>" . font-lock-keyword-face)
                          ("\\<strictfp\\>" . font-lock-keyword-face)
                          ("\\<super\\>" . font-lock-keyword-face)
                          ("\\<switch\\>" . font-lock-keyword-face)
                          ("\\<synchronized\\>" . font-lock-keyword-face)
                          ("\\<this\\>" . font-lock-keyword-face)
                          ("\\<throw\\>" . font-lock-keyword-face)
                          ("\\<throws\\>" . font-lock-keyword-face)
                          ("\\<transient\\>" . font-lock-keyword-face)
                          ("\\<try\\>" . font-lock-keyword-face)
                          ("\\<void\\>" . font-lock-keyword-face)
                          ("\\<volatile\\>" . font-lock-keyword-face)
                          ("\\<while\\>" . font-lock-keyword-face)
                          ("@\\<[[:alnum:]]+?\\>" . font-lock-builtin-face)))

(setq c++-font-lock-extra-types nil)
(setq java-font-lock-extra-types nil)

(setq font-lock-maximum-decoration
      '((c++-mode . 1)
        (java-mode . 1)
        (t . t)))

(provide '_faces_default)
