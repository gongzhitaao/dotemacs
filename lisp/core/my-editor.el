;;; my-editor.el --- My Editor configuration.

;; Copyright (C) 2014  Zhitao Gong

;; Author: Zhitao Gong <zzg0009@auburn.edu>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; don't use tables to indent
(setq-default indent-tabs-mode nil)

;; but maintain correct appearance just in case
(setq-default tab-width 8)

;; delete the selection with a keypress
(delete-selection-mode t)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(setq tab-always-indent 'complete)

(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; mouse avoid cursor
(mouse-avoidance-mode 'animate)

(blink-cursor-mode 0)

(setq scroll-preserve-screen-position t)

(setq require-final-newline t)
(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %Z %u")

(add-hook 'write-file-functions
          (lambda ()
            (delete-trailing-whitespace)
            (time-stamp)))

(setq blink-matching-paren nil)

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

;; status bar
(column-number-mode 1)
(setq size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(file-name-shadow-mode t)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; diminish keeps the modeline tidy
(require 'diminish)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing
                                         ; uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special
                                         ; buffers

;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" my-savefile-dir))
;; activate it for all buffers
(setq-default save-place t)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" my-savefile-dir))
(savehist-mode +1)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" my-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15)
(recentf-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; tramp, for sudo access
(require 'tramp)
(let ((my-tramp-file (concat my-savefile-dir "tramp")))
  (setq tramp-default-method "ssh"
        tramp-persistency-file-name my-tramp-file))

(set-default 'imenu-auto-rescan t)

;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(when (executable-find ispell-program-name)
  (flyspell-mode +1))

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(require 'expand-region)

;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks"
                                              my-savefile-dir)
      bookmark-save-flag 1)

;; anzu-mode enhances isearch by showing total matches and current
;; match position
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

;; shorter aliases for ack-and-a-half commands
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes
  '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-indent-blacklisted-modes
  '(python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not
  automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode yank-indent-blacklisted-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of `yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (when (and (not (ad-get-arg 0))
             (not (member major-mode yank-indent-blacklisted-modes))
             (or (derived-mode-p 'prog-mode)
                 (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
      (yank-advised-indent-function (region-beginning) (region-end)))))

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; sensible undo
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; backup editing file in one foler so that it doesn't cluster my
;; folder with tilded filenames
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-list-file-prefix
      (expand-file-name ".saves-" temporary-file-directory))

(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq scroll-margin 0
      scroll-preserve-screen-position 1)

(setq delete-by-moving-to-trash t)

;; CamelCase will behave like two words
(global-subword-mode 1)

;; so you won't accidentally close Emacs.
(setq confirm-kill-emacs 'yes-or-no-p)

(yas-global-mode 1)

;; frame title: filename filepath user@machine
(setq-default
 frame-title-format
 '(:eval
   (format "%s\t\t%s"
           (file-name-nondirectory
            (or (buffer-file-name) (buffer-name)))
           (file-name-directory
            (or (buffer-file-name) default-directory)))))

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

;; -------------------------------------------------------------------
;; font and encoding system
;; -------------------------------------------------------------------

(let ((my-prefer-coding-system
       '(cp950 gb2312 cp936 gb18030 utf-16 utf-8)))
  (dolist (c my-prefer-coding-system)
    (prefer-coding-system c)))

(require 'fill-column-indicator)
(setq fci-rule-color "white")

;; en
(set-face-attribute 'default nil
                    :font "Dejavu Sans Mono:pixelsize=14")

;; zh
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset (font-spec :family "WenQuanYi Zen Hei Mono"
                      :size 16)))

;; -------------------------------------------------------------------
;; Misc
;; -------------------------------------------------------------------
(load-theme 'naquadah t)

(setq initial-scratch-message nil)

(require 'chinese-wubi nil t)

(provide 'my-editor)
;;; my-editor.el ends here
