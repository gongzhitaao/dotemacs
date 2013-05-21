;;; cc-mode.conf.el
;;; Time-stamp: <2013-05-19 22:52:20 CDT gongzhitaao>

(require 'cc-mode)

(setq c-basic-offset 4
      c-ident-level 4)

(setq-default indent-tabs-mode nil
              tab-width 4)

(add-hook 'c++-mode-hook
          '(lambda ()
             (local-set-key "\C-n" 'next-error)
             (local-set-key "\C-n" 'previous-error)))

(provide 'cc-mode.conf.el)
