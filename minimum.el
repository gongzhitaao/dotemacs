;;; minimum.el
;;; Time-stamp: <2013-11-04 20:39:19 CST gongzhitaao>

(show-paren-mode t)
(setq show-paren-style 'mixed)
;; (mouse-avoidance-mode 'animate)

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

(tool-bar-mode 0)
(menu-bar-mode 1)
(scroll-bar-mode 0)

(setq inhibit-startup-message t
      resize-mini-windows t
      frame-title-format "%b    %f")

(column-number-mode 1)
(setq size-indication-mode t
      display-time-24hr-format t
      display-time-day-and-date t)
(display-time)

(setq view-read-only t)

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

(if (< (emacs-version) 23)
    (progn
      (defun region-active-p () (and transient-mark-mode mark-active)))
  (progn
    (global-subword-mode 1)
    (global-linum-mode t)))

;; ------------------------------------------------------------------
;; Custume functions
;; ------------------------------------------------------------------

(defun gzt/apply-region-or-line (func)
  "Apply FUNC to a region, or current line if mark is not
  active."
  (if (region-active-p)
      (funcall func (region-beginning) (region-end))
    (funcall func (line-beginning-position) (line-end-position))))

(defun gzt/toggle-comment-region-or-line ()
  "Toggle comment on active region or current line if no region
is active"
  (interactive)
  (gzt/apply-region-or-line 'comment-or-uncomment-region))

(defun gzt/kill-ring-save ()
  "Save the region, or line if mark is not active, as if killed,
  but don't kill it."
  (interactive)
  (gzt/apply-region-or-line 'kill-ring-save))

(defun gzt/kill-region ()
  "Kill the region, or current line if mark is not active and
  save it to the kill ring"
  (interactive)
  (gzt/apply-region-or-line 'kill-region))

(defun gzt/indent-buffer ()
  "Indent the whole buffer"
  (interactive)
  (gzt/apply-region-or-line 'indent-region))

;; ------------------------------------------------------------------
;; key bindings
;; ------------------------------------------------------------------
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'query-replace-regexp)

(global-set-key (kbd "C-x /") 'gzt/toggle-comment-region-or-line)

(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)

(global-unset-key (kbd "M-w"))
(global-set-key (kbd "M-w") 'gzt/kill-ring-save)
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'gzt/kill-region)

(global-set-key (kbd "C-c =") 'align-regexp)

(global-unset-key (kbd "C-M-\\"))
(global-set-key (kbd "C-M-\\") 'gzt/indent-buffer)

(require 'cc-mode)

(setq c-basic-offset 4)

(setq-default indent-tabs-mode nil
              tab-width 4)
