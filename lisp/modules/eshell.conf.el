;;; eshell.conf.el --- Eshell config
;;; Time-stamp: <2014-09-05 09:55:53 CDT gongzhitaao>

(require 'eshell)

;; Truncate eshell buffer just in case you got megabytes of output
(add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

(provide 'eshell.conf)
;;; eshell.conf.el ends here
