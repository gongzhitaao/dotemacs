;;; js2-conf.el --- js2-mode conf
;;; Time-stamp: <2014-05-06 12:04:58 CDT gongzhitaao>


(setq js-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq js2-basic-offset 2
      js2-bounce-indent-p t)

(provide 'js2-conf)
;;; js2-conf.el ends here
