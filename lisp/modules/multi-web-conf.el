;;; multi-web-conf.el --- multi-web-mode conf
;;; Time-stamp: <2014-05-06 11:56:18 CDT gongzhitaao>

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags
      '((js-mode  "<script\\(\\s-+type=\"text/javascript\"\\|language=\"javascript\"[^>]*\\)?>" "</script>")
        (css-mode "<style\\(\\s-+type=\"text/css\"[^>]*\\)?>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

(provide 'multi-web-conf)
;;; multi-web-conf.el ends here
