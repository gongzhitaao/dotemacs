;;; js2-conf.el --- js2-mode conf
;;; Time-stamp: <2015-06-04 14:55:58 gongzhitaao>

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq js2-basic-offset 2
      js2-bounce-indent-p nil)
(setq js2-mode-indent-ignore-first-tab t)

(rename-modeline "js2-mode" js2-mode "JS2")

(setq js2-include-node-externs t
      js2-include-browser-externs t)

(setq-default js2-global-externs
              '("angular" "assert" "clearInterval" "clearTimeout" "console" "d3" "describe" "document" "google" "hljs" "Infinity" "io" "jQuery" "JSON" "localStorage" "module" "process" "require" "setInterval" "setTimeout" "window"))

;;; js2-conf.el ends here
