;;; gnus.conf.el
;;; Time-stamp: <2012-12-19 20:45:24 CST gongzhitaao>

(require 'gnus)

(setq user-full-name "Zhitao Gong")
(setq user-mail-address "me@gongzhitaao.org")

(setq gnus-init-file "~/Documents/dotemacs/config/gnus.conf.el")

(setq gnus-secondary-select-methods
      '((nnimap "localhost" (nnimap-stream network))))

(setq gnus-summary-line-format
      "%U%R%z%I%(%[%&user-date;  %-15,15f%] %)%s\n"
      gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M")))
;;      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject

(setq gnus-thread-sort-functions '((not gnus-thread-sort-by-number)))

;; trn-style three three look
(setq gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├► "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-leaf "╰► "
      gnus-sum-thread-tree-vertical "│")

(provide 'gnus.conf)
