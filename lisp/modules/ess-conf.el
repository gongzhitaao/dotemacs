;;; ess.conf.el
;;; Time-stamp: <2014-06-19 10:56:09 CDT gongzhitaao>

(require 'ess-site)

(add-hook 'ess-mode-hook
          (lambda ()
            (setq ess-help-own-frame 'one)
            (setq ess-indent-level 2)
            (setq ess-first-continued-statement-offset 2)
            (setq ess-continued-statement-offset 0)
            (setq ess-tab-complete-in-script t)
            (setq ess-first-tab-never-complete 'symbol-or-paren-or-punct)
            (yas-minor-mode -1)))

(provide 'ess-conf)
;;; ess.conf.el ends here
