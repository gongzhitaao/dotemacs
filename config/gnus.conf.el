;;; gnus.conf.el
;;; Time-stamp: <2012-12-08 08:01:56 CST gongzhitaao>

(require 'gnus)

(setq gnus-init-file "~/Documents/dotemacs/config/gnus.conf.el")

(setq gnus-secondary-select-methods
      '((nnimap "localhost" (nnimap-stream network))))

(provide 'gnus.conf)
