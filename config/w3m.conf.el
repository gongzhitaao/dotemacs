;;; w3m.conf.el
;;; Time-stamp: <2013-11-19 22:42:26 CST gongzhitaao>

(require 'w3m)

(setq w3m-init-file "~/Documents/dotemacs/config/w3m.conf.elc")

(setq w3m-bookmark-file "~/Documents/dotemacs/emacs.d/w3m-bookmark.html")

(hl-line-mode 1)

(if (file-exists-p "/usr/bin/google-chrome")
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "/usr/bin/google-chrome"
          browse-url-default-browser "/usr/bin/google-chrome")
  (setq
   browse-url-browser-function 'browse-url-default-browser))
(setq browse-url-new-window-flag t)

(provide 'w3m.conf)
