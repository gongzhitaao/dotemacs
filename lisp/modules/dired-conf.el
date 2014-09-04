;;; dired-conf.el --- Dired config
;;; Time-stamp: <2014-08-31 17:04:09 CDT gongzhitaao>

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

(setq dired-listing-switches "-alh")

(provide 'dired-conf)
;;; dired-conf.el ends here
