;;; deft.conf.el
;;; Time-stamp: <2013-09-26 17:14:35 CDT gongzhitaao>

(require 'deft)

(when (require 'deft nil 'noerror)
  (setq deft-extension "org"
        deft-directory (concat gzt/emacs-dir "emacs.d/deft")
        deft-text-mode 'org-mode
        deft-incremental-search nil))

(provide 'deft.conf)
