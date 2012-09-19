
;;; basic.el

(require 'color-theme)
(color-theme-initialize)
(color-theme-gnome2)

(require 'epa-file)
(epa-file-enable)

(setq inhibit-startup-message t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq visible-bell t)
(add-to-list 'default-frame-alist '(width . 120))

(setq line-spacing 1)

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

(add-hook 'write-file-hooks 'time-stamp)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(setq require-final-newline t)
(setq x-select-enable-clipboard t)

(partial-completion-mode t)
(icomplete-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq resize-mini-windows nil)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(setq c-basic-offset 4
      c-ident-level 4)

(setq calendar-chinese-all-holidays-flag t
	  calendar-date-style (quote iso)
	  calendar-mark-holidays-flag t
	  calendar-view-holidays-initially-flag nil
	  column-number-mode t)

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

(set-default-font "DejaVu Sans Mono:pixelsize=13")

(provide 'basic.el)
