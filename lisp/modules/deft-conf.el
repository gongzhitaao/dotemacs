;;; deft-conf.el --- Deft configuration
;;; Time-stamp: <2014-08-23 08:49:46 CDT gongzhitaao>
;;; Code:

(require 'deft)
(setq deft-extension "org")
(setq deft-directory (expand-file-name "notes" my-personal-dir))
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)
(setq deft-auto-save-interval 0)

(provide 'deft-conf)
;;; deft-conf.el ends here
