;;; js2-conf.el --- js2-mode conf
;;; Time-stamp: <2014-05-24 09:21:27 CDT gongzhitaao>

(setq js-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq js2-basic-offset 2
      js2-bounce-indent-p t)
(setq js2-mode-indent-ignore-first-tab t)

(setq-default js2-global-externs
              '("module" "require" "assert" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "console" "JSON" "Infinity" "d3" "window" "document"))

(provide 'js2-conf)
;;; js2-conf.el ends here
