;;; sp-conf.el --- Smartparens config
;;; Time-stamp: <2015-04-16 09:01:41 CDT gongzhitaao>

(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(setq sp-navigate-close-if-unbalanced t)

(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">")
  (sp-local-tag "i" "\"[" "\"]"))

(my/sp-keymap)

;;; sp-conf.el ends here
