;;; basic.el --- Basic configuration for Emacs
;;; Time-stamp: <2013-12-18 17:05:36 CST gongzhitaao>

;; highlight parenthesis
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; mouse avoid cursor
(mouse-avoidance-mode 'animate)

;; highlight
(global-font-lock-mode 1)

;; no blinking cursor
(blink-cursor-mode 0)

(setq scroll-preserve-screen-position t)

(setq scroll-margin 3
      scroll-conservatively 0)

(setq require-final-newline t)
(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %Z %u")

(add-hook 'write-file-functions
          (lambda ()
            (delete-trailing-whitespace)
            (time-stamp)))

;; hide toolbar
(tool-bar-mode 0)

;; show menu bar because there are commands I'm never aware of.
(menu-bar-mode 1)

;; hide scrollbar
(scroll-bar-mode 0)

;; line number
(global-linum-mode t)

;; visible blink if I do something bad or wrong
(setq visible-bell t)

(setq inhibit-startup-message t
      resize-mini-windows t)

;; frame title: filename filepath user@machine
(setq-default
 frame-title-format
 '(:eval
   (format "%s%s< %s@%s >"
           (concat (file-name-nondirectory (or (buffer-file-name) (buffer-name))) (make-string 8 ? ))
           (concat (file-name-directory (or (buffer-file-name) default-directory)) (make-string 8 ? ))
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name))))

;; status bar
(column-number-mode 1)
(setq size-indication-mode t
      display-time-24hr-format t
      display-time-day-and-date t)
(display-time)

(file-name-shadow-mode t)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; set readonly file to view-mode
(setq view-read-only t)

(setq kill-ring-max 1000
      mouse-yank-at-point t
      case-fold-search nil
      x-select-enable-clipboard t)

(icomplete-mode t)
(setq icomplete-prospects-height 1)

(fset 'yes-or-no-p 'y-or-n-p)

;; So M-U will uppercase the word
(put 'upcase-region 'disabled nil)

(setq delete-by-moving-to-trash t)

(setq debug-on-error t)

;; CamelCase will behave like two words
(global-subword-mode 1)

;; so you won't accidentally close emacs.
(setq confirm-kill-emacs 'yes-or-no-p)

;; package config
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(load-theme 'naquadah t)

(server-start)

(provide 'basic)
;;; basic.el ends here
