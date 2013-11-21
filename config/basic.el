;;; basic.el
;;; Time-stamp: <2013-11-20 22:55:39 CST gongzhitaao>

;; -------------------------------------------------------------------
;; view
;; -------------------------------------------------------------------

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t))

(load-theme 'naquadah t)
;; (load-theme 'solarized-dark t)

(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(mouse-avoidance-mode 'animate)
(global-font-lock-mode 1)
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

;; -------------------------------------------------------------------
;; frame
;; -------------------------------------------------------------------
(tool-bar-mode 0)
(menu-bar-mode 1)
(scroll-bar-mode 0)
(global-linum-mode t)

(setq visible-bell t)

(setq inhibit-startup-message t
      resize-mini-windows t)

(setq-default
 frame-title-format
 '(:eval
   (format "%s%s< %s@%s >"
           (concat (file-name-nondirectory (or (buffer-file-name) (buffer-name))) (make-string 8 ? ))
           (concat (file-name-directory (or (buffer-file-name) default-directory)) (make-string 8 ? ))
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name))))

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

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Killed line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

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
                ("\\.\\(h\\|cu\\)$" . c++-mode)
                ("CMake.*\\.txt$" . cmake-mode))
              auto-mode-alist))

(gzt/add-hooks
 '(lambda ()
    (auto-fill-mode 1))
 '(markdown-mode-hook LaTeX-mode-hook latex-mode-hook))

;; set readonly file to view-mode
(setq view-read-only t)

;; -------------------------------------------------------------------
;; backup
;; -------------------------------------------------------------------
(unless (file-exists-p "~/.emacs.d/cache/backups/") (make-directory "~/.emacs.d/cache/backups/"))
(setq backup-directory-alist '(("." . "~/.emacs.d/cache/backups/")))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq auto-save-list-file-prefix
      "~/.emacs.d/cache/auto-save-list/.saves-")

(message "Auto cleaning week's old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files "~/.emacs.d/cache/backups/" t))
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
(setq icomplete-prospects-height 1)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq delete-by-moving-to-trash t)

(setq debug-on-error t)

(require 'tramp)
(setq
 tramp-default-method "ssh"
 tramp-persistency-file-name "~/.emacs.d/cache/tramp")
(add-to-list 'tramp-default-proxies-alist
             '("mallard.auburn.edu" nil "/ssh:zzg0009@131.204.27.140:"))

;; CamelCase will behave like two words
(global-subword-mode 1)

;; so you won't accidentally close emacs.
(setq confirm-kill-emacs 'yes-or-no-p)

(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward-angle-brackets
 uniquify-ignore-buffers-re "^\\*")

(defun emacs-session-filename (SESSION-ID)
  (concat "~/.emacs.d/cache/session." SESSION-ID))

(setq save-place-file "~/.emacs.d/cache/saveplace")
(setq-default save-place t)
(require 'saveplace)

(setq savehist-additional-variables              ;; also save...
      '(search ring regexp-search-ring)          ;; ... my search entries
      savehist-file "~/.emacs.d/cache/savehist") ;; keep my home clean
(savehist-mode t)

(require 'recentf)    ;; save recently used files
(setq
 recentf-save-file "~/.emacs.d/cache/recentf"
 recentf-max-saved-items 100     ;; max save 100
 recentf-max-menu-items 15)      ;; max 15 in menu
(recentf-mode 1)

(setq abbrev-file-name               ;; tell emacs where to read abbrev
      "~/Documents/dotemacs/emacs.d/abbrev_defs") ;; definitions from...
(abbrev-mode t)                      ;; enable abbrevs (abbreviations) ...
(setq abbrev-mode t          ;; turn it on
      save-abbrevs t)                ;; don't ask
(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))        ;;  don't tell
(add-hook 'kill-emacs-hook           ;; write when ...
          'write-abbrev-file)        ;; ... exiting emacs

(eval-after-load "filecache"
  '(progn (message "Loading file cache...")
          (file-cache-add-directory "~/")
          (file-cache-add-directory-list '("~/Desktop" "~/Documents"))))

(require 'ido)
(ido-mode 'both) ;; for buffers and files
(setq
 ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
 ido-ignore-buffers ;; ignore these guys
 '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
   "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
 ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
 ido-case-fold  t                 ; be case-insensitive
 ido-enable-last-directory-history t ; remember last used dirs
 ido-max-work-directory-list 30   ; should be enough
 ido-max-work-file-list      50   ; remember many
 ido-use-filename-at-point nil    ; don't use filename at point (annoying)
 ido-use-url-at-point nil         ; don't use url at point (annoying)
 ido-enable-flex-matching nil     ; don't try to be too smart
 ido-max-prospects 8              ; don't spam my minibuffer
 ido-confirm-unique-completion t) ; wait for RET, even with unique completion

(setq confirm-nonexistent-file-or-buffer nil)

;; increase minibuffer size when ido completion is active
(add-hook 'ido-minibuffer-setup-hook
          (function
           (lambda ()
             (make-local-variable 'resize-minibuffer-window-max-height)
             (setq resize-minibuffer-window-max-height 1))))

(require 'yasnippet)
(auto-insert-mode 1)
(require 'auto-complete)
(global-auto-complete-mode 1)

(server-start)

(provide 'basic)
