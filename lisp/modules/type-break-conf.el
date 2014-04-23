;;; type-break-conf.el --- type-break config
;;; Time-stamp: <2014-04-23 08:27:05 CDT gongzhitaao>

(type-break-mode)
(setq type-break-file-name (expand-file-name "type-break" my-savefile-dir))
(type-break-mode-line-message-mode 1)

(type-break-query-mode 1)

(provide 'type-break-conf)
;;; type-break-conf.el ends here
