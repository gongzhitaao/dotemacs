
;;; basic.el
;;; Time-stamp: <2013-03-04 17:32:52 CST gongzhitaao>

;; ---------------------------------------------------------------------
;; view
;; ---------------------------------------------------------------------

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(set-default-font "Monospace:pixelsize=14")

(show-paren-mode t)
(setq show-paren-style 'mixed)
(mouse-avoidance-mode 'animate)
(global-font-lock-mode 1)
(blink-cursor-mode 0)

(setq c-basic-offset 4
      c-ident-level 4)

(setq-default indent-tabs-mode nil
              tab-width 4)

(setq scroll-margin 3
      scroll-conservatively 0)

(setq require-final-newline t)
(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %Z %u")
(add-hook 'write-file-hooks
          (lambda ()
            (delete-trailing-whitespace)
            (time-stamp)))

;; ---------------------------------------------------------------------
;; frame
;; ---------------------------------------------------------------------
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-linum-mode t)

(setq inhibit-startup-message t
      visible-bell t
      resize-mini-windows t
      frame-title-format "%b    %f")

(column-number-mode 1)
(setq size-indication-mode t
      display-time-24hr-format t
      display-time-day-and-date t)
(display-time)

;; ---------------------------------------------------------------------
;; encoding
;; ---------------------------------------------------------------------
(let ((my-prefer-coding-system
       '(cp950 gb2312 cp936 gb18030 utf-16-unix utf-8-unix)))
  (dolist (c my-prefer-coding-system)
    (prefer-coding-system c)))

;; ---------------------------------------------------------------------
;; encrypt
;; ---------------------------------------------------------------------
(require 'epa-file)

(defadvice epg--start (around advice-epg-disable-agent disable)
  "Make epg--start not able to find a gpg-agent"
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (setenv "GPG_AGENT_INFO" agent)))

(defun epg-disable-agent ()
  "Make EasyPG bypass any gpg-agent"
  (interactive)
  (ad-enable-advice 'epg--start 'around 'advice-epg-disable-agent)
  (ad-activate 'epg--start)
  (message "EasyPG gpg-agent bypassed"))

(defun epg-enable-agent ()
  "Make EasyPG use a gpg-agent after having been disabled with epg-disable-agent"
  (interactive)
  (ad-disable-advice 'epg--start 'around 'advice-epg-disable-agent)
  (ad-activate 'epg--start)
  (message "EasyPG gpg-agent re-enabled"))

;; ---------------------------------------------------------------------
;; mode
;; ---------------------------------------------------------------------
(setq default-major-mode 'org-mode)

(setq auto-mode-alist
      (append '(("\\.\\(rake\\|gemspec\\)$\\|Rakefiles$" . ruby-mode)
                ("\\.txt" . org-mode)
                ("rc$" . conf-mode))
              auto-mode-alist))

(add-hook 'markdown-mode-hook
          (lambda ()
            (auto-fill-mode 1)))
(add-hook 'latex-mode-hook
          (lambda ()
            (auto-fill-mode 1)))

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

;; ---------------------------------------------------------------------
;; Miscellaneous
;; ---------------------------------------------------------------------
(require 'browse-kill-ring)
(require 'rainbow-mode)

(setq kill-ring-max 1000
      mouse-yank-at-point t
      case-fold-search nil
      x-select-enable-clipboard t)

; (partial-completion-mode t)
(icomplete-mode t)

(setq abbrev-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(provide 'basic)
