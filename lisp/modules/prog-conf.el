
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(semantic-mode 1)

;; open .h in c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'text-mode-hook 'turn-on-auto-fill)
