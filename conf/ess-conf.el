;;; ess.conf.el
;;; Time-stamp: <2015-05-05 20:50:50 CDT gongzhitaao>

(require 'ess-site)

(add-hook 'ess-mode-hook
          (lambda ()
            (setq ess-help-own-frame 'one)
            (setq ess-indent-level 2)
            (setq ess-first-continued-statement-offset 2)
            (setq ess-continued-statement-offset 0)
            (setq ess-tab-complete-in-script t)
            (setq ess-first-tab-never-complete 'symbol-or-paren-or-punct)))

(add-hook 'inferior-ess-mode-hook
          (lambda ()
            (smartparens-mode 1)))

;;; ess.conf.el ends here
