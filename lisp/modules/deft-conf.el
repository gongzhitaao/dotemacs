;;; deft-conf.el --- Deft configuration
;;; Time-stamp: <2014-04-09 16:01:41 CDT gongzhitaao>
;;; Code:

(require 'deft)
(setq deft-extension "org")
(setq deft-directory (expand-file-name "notes" my-personal-dir))
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)

(provide 'deft-conf)
;;; deft-conf.el ends here
