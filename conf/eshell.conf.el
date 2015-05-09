;;; eshell.conf.el --- Eshell config
;;; Time-stamp: <2015-05-05 20:50:33 CDT gongzhitaao>

(require 'eshell)

;; Truncate eshell buffer just in case you got megabytes of output
(add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

(setq eshell-directory-name (expand-file-name "eshell" my-tmp))

;;; eshell.conf.el ends here
