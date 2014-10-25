
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; (semantic-mode 1)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq tab-stop-list (number-sequence 2 120 2))

(setq sgml-basic-offset 0)
