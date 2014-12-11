;;; js2-conf.el --- js2-mode conf
;;; Time-stamp: <2014-12-05 19:58:17 CST gongzhitaao>

(setq js-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq js2-basic-offset 2
      js2-bounce-indent-p nil)
(setq js2-mode-indent-ignore-first-tab t)

(setq-default js2-global-externs
              '("module" "require" "assert" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "console" "JSON" "Infinity" "d3" "window" "document" "describe" "jQuery" "hljs"))

(provide 'js2-conf)
;;; js2-conf.el ends here
