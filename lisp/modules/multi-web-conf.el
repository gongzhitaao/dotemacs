;;; multi-web-conf.el --- multi-web-mode conf
;;; Time-stamp: <2014-05-21 21:27:59 CDT gongzhitaao>

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags
      '((js-mode  "<script\\(\\s-+type=\"text/javascript\"\\|language=\"javascript\"[^>]*\\)?>" "</script>")
        (glsl-mode "<script\\([^>]*\\)\\(\\s-+type=\"x-shader/[a-z-]+\"[^>]*\\)>" "</script>")
        (css-mode "<style\\(\\s-+type=\"text/css\"[^>]*\\)?>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

(setq mweb-submode-indent-offset 0)

(provide 'multi-web-conf)
;;; multi-web-conf.el ends here
