;;; gnus.conf.el
;;; Time-stamp: <2013-12-18 16:33:42 CST gongzhitaao>

(require 'gnus)

(setq user-full-name "Zhitao Gong")
(setq user-mail-address "me@gongzhitaao.org")

(setq gnus-init-file (concat my-emacs-root "gnus.conf.el"))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; (setq gnus-select-method '(nntp "news.gmane.org"))
;; (setq gnus-secondary-select-method
;;       '((nntp "news.gnus.org")
;;         (nnimap "tigermail"
;;                 (nnimap-address "pod51004.outlook.com")
;;                 (nnimap-server-port 995)
;;                 (nnimap-stream ssl))))

;; (setq sendmail-program "msmtp")
;; (setq send-mail-function 'mailclient-send-it)

;; (add-hook 'gnus-article-prepare-hook 'gnus-article-date-local)
;; (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; (setq mm-text-html-renderer 'w3m)
;; (setq mm-inline-large-images t)
;; (auto-image-file-mode t)
;; (setq mm-inline-text-html-with-images t)
;; (setq mm-w3m-safe-url-regexp nil)

;; (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;; (add-hook 'gnus-startup-hook 'bbdb-insinuate-message)
;; (add-hook 'message-setup-hook 'bbdb-define-all-aliases)

(provide 'gnus.conf)
