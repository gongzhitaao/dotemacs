;;; gnus.conf.el
;;; Time-stamp: <2014-03-16 10:13:04 CDT gongzhitaao>

(require 'gnus)

(setq user-full-name "Zhitao Gong")
(setq user-mail-address "zzg0009@auburn.edu")

(setq gnus-init-file (concat my-emacs-root "gnus.conf.el"))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; (setq gnus-select-method
;;       '(nnimap "outlook.office365.com"))

(setq gnus-select-method
      '(nnimap "Tigermail"
               (nnimap-address "localhost")
               (nnimap-stream network)
               (nnimap-server-port 143)))

(setq-default
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│")

(setq gnus-posting-styles
      '((".*"
         (signature-file "~/.sig")
         (name "Zhitao Gong")
         (organization "Department of Computer Science & Software Engineering, Auburn University"))))

(setq gnus-parameters
  '(("nnimap.*"
     (display . all))))

(setq send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "msmtp")

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

;; When I hit "forward as mail", Gnus will forward articles as inline
;; content (that is, part of the message), not as MIME.
(setq message-forward-as-mime nil)

(provide 'gnus.conf)
