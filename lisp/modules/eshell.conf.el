;;; eshell.conf.el --- Eshell config
;;; Time-stamp: <2014-08-23 08:11:25 CDT gongzhitaao>

(require 'eshell)

;; Truncate eshell buffer just in case you got megabytes of output
(add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

(add-hook 'eshell-mode-hook
          (lambda ()
            (linum-mode -1)))

(provide 'eshell.conf)
;;; eshell.conf.el ends here
