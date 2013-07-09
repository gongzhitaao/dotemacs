;;; gnus.conf.el
;;; Time-stamp: <2013-06-26 19:20:58 CDT gongzhitaao>

(require 'gnus)

(setq user-full-name "Zhitao Gong")
(setq user-mail-address "me@gongzhitaao.org")

(setq gnus-init-file "~/Documents/dotemacs/master/config/gnus.conf.el")

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-select-method
      '(nnimap "Mail"
               (nnimap-address "localhost")
               (nnimap-stream network)
               (nnimap-authenticator login)
               (nnimap-authinfo-file "~/.authinfo")))

(setq gnus-check-new-newsgroups nil)

(setq sendmail-program "msmtp")
(setq send-mail-function 'mailclient-send-it)

(setq gnus-summary-same-subject "")
(setq gnus-sum-thread-tree-indent " ")
(setq gnus-sum-thread-tree-single-indent "◎ ")
(setq gnus-sum-thread-tree-root "● ")
(setq gnus-sum-thread-tree-false-root "☆")
(setq gnus-sum-thread-tree-vertical "│")
(setq gnus-sum-thread-tree-leaf-with-other "├─► ")
(setq gnus-sum-thread-tree-single-leaf "╰─► ")

(add-hook 'gnus-article-prepare-hook 'gnus-article-date-local)
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq mm-text-html-renderer 'w3m)
(setq mm-inline-large-images t)
(auto-image-file-mode t)
(setq mm-inline-text-html-with-images t)
(setq mm-w3m-safe-url-regexp nil)

(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-message)
(add-hook 'message-setup-hook 'bbdb-define-all-aliases)

(provide 'gnus.conf)
