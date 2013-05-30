;;; prog.conf.el
;;; Time-stamp: <2013-05-29 20:53:32 CDT gongzhitaao>

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
             (local-set-key "\C-p" 'previous-error)))

;; -------------------------------------------------------------------
;; Compilation
;; -------------------------------------------------------------------
(require 'compile)

(add-hook 'compilation-mode-hook
          '(lambda ()
             (setq compilation-scroll-output 'first-error)
             ))

;; -------------------------------------------------------------------
;; Miscellaneous
;; -------------------------------------------------------------------
(add-hook 'prog-mode-hook
	  '(lambda ()
	     (hs-minor-mode 1)))

(provide 'prog.conf)
