
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(semantic-mode 1)

(add-hook 'prog-mode-hook
          (lambda ()
            (hl-line-mode 1)
            (set-face-attribute hl-line-face nil
                                :background "#3B3D3A")))
(add-hook 'text-mode-hook
          (lambda ()
            (hl-line-mode 1)
            (set-face-attribute hl-line-face nil
                                :background "#3B3D3A")))

;; open .h in c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'text-mode-hook 'turn-on-auto-fill)
