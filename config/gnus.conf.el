;;; gnus.conf.el
;;; Time-stamp: <2013-02-11 15:35:00 CST gongzhitaao>

(require 'gnus)

(setq user-full-name "Zhitao Gong")
(setq user-mail-address "me@gongzhitaao.org")

(setq gnus-init-file "~/Documents/dotemacs/config/gnus.conf.el")

(setq gnus-secondary-select-methods
      '((nnimap "localhost" (nnimap-stream network))))

(setq gnus-select-methods
      '(nnimap "Mail"
               (nnimap-address "localhost")
               (nnimap-stream network)
               (nnimap-authenticator login)))

(setq gnus-use-trees t)

(setq gnus-summary-line-format
      "%U%R%z%I%(%[%&user-date;  %-15,15f%] %)%s\n"
      gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M")))

(setq gnus-summary-make-false-root 'dummy
      gnus-summary-gather-subject-limit 'fuzzy)

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-date
        gnus-thread-sort-by-author
        gnus-thread-sort-by-number))

;; trn-style three three look
(setq gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├► "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-leaf "╰► "
      gnus-sum-thread-tree-vertical "│")

(provide 'gnus.conf)
