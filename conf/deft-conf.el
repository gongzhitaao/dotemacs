;;; deft-conf.el
;;; Time-stamp: <2015-05-05 20:20:43 CDT gongzhitaao>

(require 'deft)

(setq deft-extension "org")
(setq deft-directory (expand-file-name "notes" my-personal-dir))
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title nil)
(setq deft-auto-save-interval 0)
(setq deft-strip-title-regexp
      (concat deft-strip-title-regexp
              "\\|\\(?:\\+TITLE:[[:space:]]+\\)"))

;;; deft-conf.el ends here
