;;; prog.conf.el
;;; Time-stamp: <2013-11-20 22:51:19 CST gongzhitaao>

;; -------------------------------------------------------------------
;; C/C++
;; -------------------------------------------------------------------
(require 'cc-mode)

(setq c-basic-offset 2)

(setq-default indent-tabs-mode nil
              tab-width 4)

(gzt/add-hooks '(lambda ()
                  (local-set-key "\C-n" 'next-error)
                  (local-set-key "\C-p" 'previous-error))
               '(c++-mode-hook c-mode-hook python-mode-hook emacs-lisp-mode-hook))

(add-hook 'c-mode-hook
          'google-set-c-style
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

;; (defun my-c-lineup-arglist (langelem)
;;   (let ((ret (c-lineup-arglist langelem)))
;;     (if (< (elt ret 0) my-c-lineup-maximum-indent)
;;         ret
;;       (save-excursion
;;         (goto-char (cdr langelem))
;;         (vector (+ (current-column) 8))))))

;; (defun my-indent-setup ()
;;   (setcdr (assoc 'arglist-cont-nonempty c-offsets-alist)
;;           '(c-lineup-gcc-asm-reg my-c-lineup-arglist)))

(add-hook 'python-mode-hook
          '(lambda ()
             (local-unset-key (kbd "<f7>"))
             (local-set-key (kbd "<f7>") 'pylint)))


(require 'ac-math)

(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`
(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
        (append '(ac-source-math-unicode
                  ac-source-math-latex
                  ac-source-latex-commands)
                ac-sources)))

(add-hook 'latex-mode-hook 'ac-latex-mode-setup)


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
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")
                  (js2-mode "<script\\( +type=\"text/javascript\"\\|language=\"javascript\"\\)?[^>]*>" "</script>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq ac-js2-evaluate-calls t)

;; -------------------------------------------------------------------
;; Miscellaneous
;; -------------------------------------------------------------------
(defun doxymacs-conf ()
  (interactive)
  (font-lock-add-keywords nil doxymacs-doxygen-keywords)
  (font-lock-add-keywords nil '(("\\([@\\\\]\\(details\\)\\)\\>" (0 font-lock-keyword-face prepend))))
  (setq doxymacs-doxygen-style "Qt"))

(require 'fill-column-indicator)
(add-hook 'prog-mode-hook
          '(lambda ()
             ;; (hs-minor-mode 1)
             ;; (doxymacs-mode 1)
             ;; (doxymacs-conf)
             ;; (fci-mode 1)
             (electric-pair-mode 1)
             (hl-line-mode 1)
             (if (display-graphic-p)
                 (set-face-attribute hl-line-face nil :background "#3B3D3A")
               (set-face-attribute hl-link-face nil :underline t :background nil))))

(provide 'prog.conf)
