;;; w3m.conf.el --- Emacs-w3m configuration
;;; Time-stamp: <2013-12-18 16:43:03 CST gongzhitaao>

(require 'w3m)

(setq w3m-init-file (concat my-emacs-root "w3m.conf.el"))
(setq w3m-bookmark-file (concat my-emacs-data "w3m-bookmark.html"))

(if (file-exists-p "/usr/bin/firefox")
    (setq browse-url-browser-function 'browse-url-generic
	  browse-url-generic-program "/usr/bin/firefox"
	  browse-url-default-browser "/usr/bin/firefox")
  (setq browse-url-browser-function 'browse-url-default-browser))

(setq browse-url-new-window-flag t)
