;;; prog-conf.el --- General prog configuration
;;; Time-stamp: <2015-05-08 22:32:16 gongzhitaao>

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; (semantic-mode 1)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq tab-stop-list (number-sequence 2 120 2))

(add-hook 'prog-mode-hook 'which-function-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (hs-minor-mode)))

(setq sgml-basic-offset 1)

;;; prog-conf.el ends here
