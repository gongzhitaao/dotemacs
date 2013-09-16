;;; basic.el
;;; Time-stamp: <2013-08-24 09:58:28 CDT gongzhitaao>

;; -------------------------------------------------------------------
;; view
;; -------------------------------------------------------------------

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t))

(when (display-graphic-p)
  (load-theme 'naquadah t))
; (load-theme 'solarized-dark t)

(show-paren-mode t)
(setq show-paren-style 'mixed)
(mouse-avoidance-mode 'animate)
(global-font-lock-mode 1)
(blink-cursor-mode 0)

(setq scroll-margin 3
      scroll-conservatively 0)

(setq require-final-newline t)
(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %Z %u")

(add-hook 'write-file-functions
          (lambda ()
            (delete-trailing-whitespace)
            (time-stamp)))

;; -------------------------------------------------------------------
;; frame
;; -------------------------------------------------------------------
(tool-bar-mode 0)
(menu-bar-mode 1)
(scroll-bar-mode 0)
(global-linum-mode t)

(when (display-graphic-p)
  (progn
    (setq visible-bell t)))

(setq inhibit-startup-message t
      resize-mini-windows t
      frame-title-format "%b    %f")

(column-number-mode 1)
(setq size-indication-mode t
      display-time-24hr-format t
      display-time-day-and-date t)
(display-time)

;; -------------------------------------------------------------------
;; mode
;; -------------------------------------------------------------------
(require 'js2-mode)
(require 'cmake-mode)
(require 'undo-tree)

(setq auto-mode-alist
      (append '(("\\.\\(rake\\|gemspec\\)$\\|Rakefile$" . ruby-mode)
                ("rc$" . conf-mode)
                ("\\.md$" . markdown-mode)
                ("\\(Makefile.*\\)\\(\\.make$\\)?$" . makefile-mode)
                ("\\.js$" . js2-mode)
                ("\\.m$" . octave-mode)
                ("\\.h$" . c++-mode)
                ("CMake.*\\.txt$" . cmake-mode))
              auto-mode-alist))

(gzt/add-hooks
 '(lambda ()
    (auto-fill-mode 1))
 '(markdown-mode-hook LaTeX-mode-hook latex-mode-hook))

(recentf-mode 1)
(setq view-read-only t)

(when (not (display-graphic-p))
  (ido-mode 1))

;; -------------------------------------------------------------------
;; backup
;; -------------------------------------------------------------------
(unless (file-exists-p "~/.saves/") (make-directory "~/.saves/"))
(setq backup-directory-alist '(("." . "~/.saves/")))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(message "Auto cleaning week's old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files "~/.saves/" t))
    (when (and (backup-file-name-p file)
               (> (- current
                     (float-time (nth 5 (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;; -------------------------------------------------------------------
;; Miscellaneous
;; -------------------------------------------------------------------
(setq kill-ring-max 1000
      mouse-yank-at-point t
      case-fold-search nil
      x-select-enable-clipboard t)

(icomplete-mode t)

(setq abbrev-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(setq delete-by-moving-to-trash t)

(setq debug-on-error t)

(provide 'basic)
