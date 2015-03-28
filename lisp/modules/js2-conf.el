;;; js2-conf.el --- js2-mode conf
;;; Time-stamp: <2015-03-08 17:45:11 CDT gongzhitaao>

(setq js-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq js2-basic-offset 2
      js2-bounce-indent-p nil)
(setq js2-mode-indent-ignore-first-tab t)

(setq-default js2-global-externs
              '("assert" "clearInterval" "clearTimeout" "console" "d3" "describe" "document" "google" "hljs" "Infinity" "jQuery" "JSON" "localStorage" "module" "require" "setInterval" "setTimeout" "window"))

(provide 'js2-conf)
;;; js2-conf.el ends here
