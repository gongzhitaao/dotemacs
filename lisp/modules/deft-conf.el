;;; deft-conf.el --- Deft configuration
;;; Time-stamp: <2015-04-12 08:57:11 CDT gongzhitaao>
;;; Code:

(require 'deft)

(setq deft-extension "org")
(setq deft-directory (expand-file-name "notes" my-personal-dir))
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title nil)
(setq deft-auto-save-interval 0)
(setq deft-strip-title-regexp
      (concat deft-strip-title-regexp
              "\\|\\(?:\\+TITLE:[[:space:]]+\\)"))

(provide 'deft-conf)
;;; deft-conf.el ends here
