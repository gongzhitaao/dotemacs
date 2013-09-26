;;; prog.conf.el
;;; Time-stamp: <2013-09-26 17:16:37 CDT gongzhitaao>

;; -------------------------------------------------------------------
;; C/C++
;; -------------------------------------------------------------------
(require 'cc-mode)

(setq c-basic-offset 4)

(setq-default indent-tabs-mode nil
              tab-width 4)

(gzt/add-hooks '(lambda ()
                  (local-set-key "\C-n" 'next-error)
                  (local-set-key "\C-p" 'previous-error))
               '(c++-mode-hook c-mode-hook python-mode-hook emacs-lisp-mode-hook))

(add-hook 'python-mode-hook
          '(lambda ()
             (local-unset-key (kbd "<f7>"))
             (local-set-key (kbd "<f7>") 'pylint)))

;; -------------------------------------------------------------------
;; Compilation & Debug
;; -------------------------------------------------------------------
(require 'compile)

(add-hook 'compilation-mode-hook
          '(lambda ()
             (setq compilation-scroll-output 'first-error)))

(setq gdb-many-windows t)


;; -------------------------------------------------------------------
;; Multi web mode
;; -------------------------------------------------------------------
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js2-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; -------------------------------------------------------------------
;; Miscellaneous
;; -------------------------------------------------------------------
(defun doxymacs-conf ()
  (interactive)
  (font-lock-add-keywords nil doxymacs-doxygen-keywords)
  (font-lock-add-keywords nil '(("\\([@\\\\]\\(details\\)\\)\\>" (0 font-lock-keyword-face prepend))))
  (setq doxymacs-doxygen-style "Qt"))

(add-hook 'prog-mode-hook
          '(lambda ()
             (hs-minor-mode 1)
             (doxymacs-mode 1)
             (doxymacs-conf)
             (electric-pair-mode 1)
             (hl-line-mode 1)
             (if (display-graphic-p)
                 (set-face-attribute hl-line-face nil :background "#3B3D3A")
               (set-face-attribute hl-link-face nil :underline t :background nil))))

(provide 'prog.conf)