
;;; basic.el
;;; Time-stamp: <2012-11-13 19:35:30 CST gongzhitaao>

(require 'color-theme)
(color-theme-initialize)
(require 'color-theme-solarized)
(color-theme-solarized-dark)

(require 'browse-kill-ring)
(require 'rainbow-mode)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(setq inhibit-startup-message t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq visible-bell t)
(add-to-list 'default-frame-alist '(width . 120))

(setq line-spacing 2)
(set-default-font "DejaVu Sans Mono:pixelsize=13")

(setq frame-title-format "%b    %f")
(column-number-mode 1)
(global-linum-mode t)

(setq size-indication-mode t
      display-time-24hr-format t
      display-time-day-and-date t)
(display-time)

(setq kill-ring-max 1000)
(setq mouse-yank-at-point t)
(setq scroll-margin 7
      scroll-conservatively 1000)

(show-paren-mode t)
(setq show-paren-style 'mixed)
(mouse-avoidance-mode 'animate)
(global-font-lock-mode t)

(setq case-fold-search nil)

(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %Z %u")
(add-hook 'write-file-hooks 'time-stamp)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(add-hook 'find-file-hook
          (lambda ()
            (turn-on-auto-fill)
            (hl-line-mode t)))

(setq require-final-newline t)
(setq x-select-enable-clipboard t)

(partial-completion-mode t)
(icomplete-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq resize-mini-windows t)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(setq c-basic-offset 4
      c-ident-level 4)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(blink-cursor-mode 0)
(setq global-auto-revert-non-file-buffers t)

(setq gnus-init-file "./config/mygnus.el")

;; ---------------------------------------------------------------------
;; mode
;; ---------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; ---------------------------------------------------------------------
;; backup
;; ---------------------------------------------------------------------
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
		     (float-time (fifth (file-attributes file))))
		  week))
      (message "%s" file)
      (delete-file file))))

(provide 'basic)
