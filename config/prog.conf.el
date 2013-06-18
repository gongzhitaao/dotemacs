;;; prog.conf.el
;;; Time-stamp: <2013-06-16 14:26:47 CDT gongzhitaao>

;; -------------------------------------------------------------------
;; C/C++
;; -------------------------------------------------------------------
(require 'cc-mode)

(setq c-basic-offset 4)

(setq-default indent-tabs-mode nil
              tab-width 4)

(add-hook 'c++-mode-hook
          '(lambda ()
             (local-set-key "\C-n" 'next-error)
             (local-set-key "\C-p" 'previous-error)
             (electric-pair-mode 1)))

;; -------------------------------------------------------------------
;; Compilation & Debug
;; -------------------------------------------------------------------
(require 'compile)

(add-hook 'compilation-mode-hook
          '(lambda ()
             (setq compilation-scroll-output 'first-error)
             ))

(setq gdb-many-windows t)

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
             ))


(provide 'prog.conf)
