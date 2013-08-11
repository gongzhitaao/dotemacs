;;; deft.conf.el
;;; Time-stamp: <2013-08-10 10:29:25 CDT gongzhitaao>

(require 'deft)

(when (require 'deft nil 'noerror)
  (setq deft-extension "org"
        deft-directory "~/Documents/dotemacs/deft/"
        deft-text-mode 'org-mode
        deft-incremental-search nil))

(provide 'deft.conf)
