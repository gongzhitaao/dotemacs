;;; init.el --- Yet another Emacs config
;; Time-stamp: <2018-09-30 15:20:11 gongzhitaao>

;;; Commentary:
;; me/xxx: mostly interactive functions, may be executed with M-x or keys
;; me//xxx: internal helper functions, not called directly by user
;; me-xxx: custom variables

;;; Code:

;; Added by Package.el.  This must come before configurations of installed
;; packages.  Don't delete this line.  If you don't want it, just comment it out
;; by adding a semicolon to the start of the line.  You may delete these
;; explanatory comments.
(package-initialize)

;; With Cask, the following package-archives is not necessary

;; (setq
;;  package-archives
;;  '(("melpa-stable" . "https://stable.melpa.org/packages/")
;;    ("melpa"        . "https://melpa.org/packages/")
;;    ("marmalade"    . "https://marmalade-repo.org/packages/")
;;    ("org"          . "http://orgmode.org/elpa/")
;;    ("gnu"          . "https://elpa.gnu.org/packages/")
;;    ("sc"           . "http://joseito.republika.pl/sunrise-commander/")))

;; (defun me//init-package ()
;;   "Load packages manually in just in case cask fails.

;; If cask fails mysteriously, use the following code to get things
;; going, at least for now.  Basically add every package path to
;; `load-path', and autoload the functions."
;;   (dolist (elem
;;            (directory-files-and-attributes
;;             "~/.emacs.d/.cask/24.5/elpa/" t))
;;     (let ((path (car elem))
;;           (dir (cadr elem))
;;           (auto nil))
;;       (if (and dir
;;                (not (string= path "."))
;;                (not (string= path "..")))
;;           (progn
;;             (add-to-list 'load-path path)
;;             (setq auto (directory-files path nil "-autoloads\.el$"))
;;             (if auto (autoload (car auto))))))))
;; (me//init-package)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode)

(eval-when-compile
  (require 'use-package))
(require 'delight)
(require 'bind-key)

;; I'm always skeptical about cask and use-package.  If they fail mysteriously,
;; I could not start my Emacs.  I guess a better way is to backup the packages
;; every day just in case.  However, use-package, bind-keys and cask are so much
;; more convenient, I surrender.

;; =============================================================================
;; Key binding
;; =============================================================================

(global-set-key (kbd "C-z") #'delete-other-windows)
(global-set-key (kbd "<backtab>") #'decrease-left-margin)

(global-set-key (kbd "C-|") #'fci-mode)
(global-set-key (kbd "C-+") #'me/join-next-line)
(global-set-key (kbd "C->") #'mc/mark-next-like-this-word)
(global-set-key (kbd "C-<") #'mc/mark-previous-like-this-word)

(global-set-key (kbd "s-;") #'comment-or-uncomment-region)

(global-set-key (kbd "C-s-<up>") #'windmove-up)
(global-set-key (kbd "C-s-<right>") #'windmove-right)
(global-set-key (kbd "C-s-<down>") #'windmove-down)
(global-set-key (kbd "C-s-<left>") #'windmove-left)

;; FN keys
;; -----------------------------------------------------------------------------

(global-set-key (kbd "<f5>") #'bookmark-bmenu-list)
(global-set-key (kbd "<f6>") #'calendar)
;; f8 -- deft
;; f10 -- menu
(global-set-key (kbd "<f11>") #'ispell)
(global-set-key (kbd "<f12>") #'mu4e)

;; Remaping
;; -----------------------------------------------------------------------------

(global-set-key [remap isearch-backward] #'isearch-backward-regexp)
(global-set-key [remap isearch-forward]  #'isearch-forward-regexp)
(global-set-key [remap list-buffers]     #'ibuffer)
(global-set-key [remap switch-to-buffer] #'helm-mini)
(global-set-key [remap yank-pop]         #'helm-show-kill-ring)

;; C-c user key
;; -----------------------------------------------------------------------------

;; C-c b -- helm-bibtex

(use-package drag-stuff
  :bind ("C-c d" . drag-stuff-mode)
  :delight (drag-stuff-mode " ")
  :config (drag-stuff-define-keys))

(bind-keys :prefix-map me-editing-command-map
           :prefix "C-c e"
           ("c" . set-buffer-file-coding-system)
           ("d" . delete-duplicate-lines)
           ("l" . magit-log-buffer-file)
           ("s l" . sort-lines)
           ("s s" . me/sort-symbols)
           ("s w" . me/sort-words)
           ("S" . me/sudo-edit)
           ("v" . add-file-local-variable)
           ("V" . add-file-local-variable-prop-line))

(use-package magit
  :bind ("C-c g" . magit-status)
  :config (setq git-commit-major-mode 'org-mode))

(use-package multiple-cursors
  :config
  (bind-keys :prefix-map me-multiple-cursors-command-map
             :prefix "C-x m"
             ("C-a" . mc/edit-beginnings-of-lines)
             ("C-e" . mc/edit-ends-of-lines)
             ("a" . mc/mark-all-like-this-dwim)
             ("l" . mc/edit-lines)
             ("i n" . mc/insert-numbers)
             ("i l" . mc/insert-letters)
             :map mc/keymap
             ("C-<right>" . mc/skip-to-next-like-this)
             ("C-<left>" . mc/skip-to-previous-like-this)
             ("M-<left>" . mc/unmark-next-like-this)
             ("M-<right>" . mc/unmark-previous-like-this))
  (setq mc/mode-line
        `(" mc:" (:eval (format ,(propertize "%d" 'face 'custom-rogue)
                                (mc/num-cursors))))))

(bind-keys :prefix-map me-org-command-map
           :prefix "C-c o"
           ("a" . org-agenda)
           ("c" . org-capture)
           ("e" . me/org-ref-open-entry)
           ("h" . me/org-custom-id-get-create)
           ("H" . me/org-custom-id-get-create-all)
           ("l b" . org-ref-extract-bibtex-entries)
           ("l f" . org-ref-list-of-figures)
           ("l t" . org-ref-list-of-tables)
           ("n" . me/org-ref-open-note)
           ("p" . me/org-ref-open-pdf)
           ("s" . me/org-sort-orgref-citation-list-by-year))

;; C-c s -- smartparens
;; C-c u -- undo-tree
(global-set-key (kbd "C-c =") #'align-regexp)
(global-set-key (kbd "C-c C-q") #'bury-buffer)

;; M- meta keys
;; -----------------------------------------------------------------------------

;; M-a backward-sentence
;; M-b backward-word
;; M-c capitalize-word
;; M-d kill-word
;; M-e forward-sentence
;; M-f forward-word
;; M-h mark-paragraph
;; M-i tab-to-tab-stop
;; M-j ace-jump
;; M-k kill-sentence
;; M-l downcase-word
;; M-m back-to-indentation
;; M-n
;; M-o ???
;; M-p ace-window
;; M-q fill-paragraph
;; M-r move-to-window-line-top-bottom
;; M-s search
;; M-t transpose-word
;; M-u upcase-word
;; M-v scroll-down
;; M-w kill-ring-save
;; M-x helm-M-x execute command
;; M-y helm-show-kill-ring
;; M-z zap-to-char

;;; M-s search

;; M-s a helm-do-ag
;; (global-set-key (kbd "M-s g") #'helm-do-grep-ag)
;; M-s h highlight-xxx
;; M-s q vr/query-replace
;; M-s s helm-swoop

;; =============================================================================
;; Variables
;; =============================================================================

(defconst me-home "~" "My home directory.")
(defconst me-data (expand-file-name "Dropbox" me-home))
(defconst me-emacs (expand-file-name "dotfiles/emacs.d" me-data))
(defconst me-emacs-data (expand-file-name "data" me-emacs))

(defconst me-emacs-tmp (expand-file-name "tmp" user-emacs-directory))
(unless (file-exists-p me-emacs-tmp)
  (mkdir me-emacs-tmp))

(defconst me-keylog (expand-file-name "keylog" user-emacs-directory))
(unless (file-exists-p me-keylog)
  (mkdir me-keylog))

(defconst me-local-conf (expand-file-name "local.el" user-emacs-directory)
  "Local configuration not shared around machines.")

(setq custom-file (expand-file-name "custom.el" me-emacs))

;; (if (file-exists-p custom-file)
;;     (load custom-file))

;; =============================================================================
;; General helpers
;; =============================================================================

(defun me--ad-with-region-or-line (args)
  "Operate on line or region.
Argument ARGS region if provided."
  (if (region-active-p)
      args
    (let ((bol (+ (line-beginning-position) (current-indentation)))
          (eol (line-end-position)))
      (push-mark bol)
      (goto-char eol)
      (list bol eol (nth 2 args)))))

(defun me-with-region-or-line (func &optional remove)
  "Call FUNC on region if region is active, otherwise line.
If not REMOVE, add advice to FUNC, i.e., when called with no
active region, call FUNC on current line.  Otherwise remove
advice."
  (if remove
      (advice-remove func #'me--ad-with-region-or-line)
    (advice-add func :filter-args #'me--ad-with-region-or-line)))

(me-with-region-or-line #'comment-or-uncomment-region)
(me-with-region-or-line #'kill-ring-save)

(defvar comint-buffer-maximum-size)
(defun me-clear-shell ()
  "Clear shell window."
   (interactive)
   (let ((old-max comint-buffer-maximum-size))
     (setq comint-buffer-maximum-size 0)
     (comint-truncate-buffer)
     (setq comint-buffer-maximum-size old-max)))

(defun me/sudo-edit (&optional arg)
  "Edit file as root.

With a prefix ARG prompt for a file to visit.  Will also prompt
for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (helm-read-file-name "Find file(root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:"
                                 buffer-file-name))))

(defun me/join-next-line (&optional arg)
  "Join this line with next line.  ARG passed to `delete-indentation'.

Convenient extension to `delete-indentation' which joins this
line with previous line."
  (interactive "P")
  (save-excursion
    (forward-line)
    (delete-indentation arg)))

(defun me/sort-words (reverse beg end)
  "Sort words (REVERSE if negative) in region (BEG, END) alphabetically.

Prefixed with negative \\[universal-argument], sorts in reverse.
The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.  See `sort-regexp-fields'.
<https://www.emacswiki.org/emacs/SortWords>"
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun me/sort-symbols (reverse beg end)
  "Sort symbols (REVERSE if negative) in region (BEG, END)
alphabetically.  See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

;; <https://oremacs.com/2015/04/28/blending-faces>

(defun me//colir-join (r g b)
  "Build a color from R G B.
Inverse of `color-values'."
  (format "#%02x%02x%02x"
          (ash r -8)
          (ash g -8)
          (ash b -8)))

(defun me//colir-blend (c1 c2 &optional alpha)
  "Blend the two colors C1 and C2 with ALPHA.
C1 and C2 are in the format of `color-values'.  ALPHA is a number
between 0.0 and 1.0 which corresponds to the influence of C1 on
the result."
  (let ((alpha (or alpha 0.5))
        (rgb1 (color-values c1))
        (rgb2 (color-values c2)))
    (apply #'me//colir-join
           (cl-mapcar
            (lambda (x y)
              (round (+ (* x alpha) (* y (- 1 alpha)))))
            rgb1 rgb2))))

;; =============================================================================
;; Appearance
;; =============================================================================

(tooltip-mode -1)
(tool-bar-mode 0)
(menu-bar-mode 1)

(blink-cursor-mode 0)
(mouse-avoidance-mode 'animate)

(scroll-bar-mode 0)
(setq scroll-margin 0
      scroll-preserve-screen-position 1)

(setq visible-bell t)
(setq inhibit-startup-message t
      resize-mini-windows t)

(defun me//set-title-bar()
  (let* ((name (or buffer-file-name dired-directory (buffer-name)))
         (hostname (or (file-remote-p name 'host) "localhost")))
    (concat "@" hostname "     " name)))
(setq frame-title-format '((:eval (me//set-title-bar))))

(add-to-list 'default-frame-alist '(background-color . "gray20"))
(add-to-list 'default-frame-alist '(foreground-color . "gray90"))

(global-hl-line-mode)
(set-face-background 'hl-line "black")

;; Show the search result count.
(use-package anzu
  :delight
  :config
  (global-anzu-mode +1)
  (setq anzu-search-threshold 1000)
  (setq anzu-cons-mode-line-p nil)
  (set-face-attribute 'anzu-mode-line nil :foreground "green4" :weight 'bold))

;; Show a dimmed delimiter at fill-column.
(use-package fill-column-indicator
  :config
  (setq fci-rule-color "gray50")
  (let ((hooks '(prog-mode-hook org-mode-hook markdown-mode-hook
                                tex-mode-hook message-mode-hook)))
    (mapc (lambda (x) (add-hook x #'turn-on-fci-mode)) hooks)))

;; Center the editing content.
(use-package writeroom-mode
  :config
  (setq writeroom-fullscreen-effect nil
        writeroom-maximize-window nil
        writeroom-mode-line t)
  (setq writeroom-width (+ fill-column 15))
  (setq writeroom-use-derived-modes t)
  (setq writeroom-major-modes
        '(prog-mode dired-mode Info-mode calendar-mode text-mode))
  (delete 'writeroom-set-menu-bar-lines writeroom-global-effects)
  (global-writeroom-mode))

;; Highlight the cursor when jumping around.
(use-package beacon
  :delight
  :config (beacon-mode))

;; Select interesting regions
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Working with parenthesis
(use-package smartparens-config
  :delight smartparens-mode
  :config
  (bind-keys ("M-<backspace>" . sp-backward-unwrap-sexp)
             ("M-<delete>" . sp-unwrap-sexp)
             :prefix-map smartparens-mode-map
             :prefix "C-c s"
             ("<backspace>" . sp-splice-sexp-killing-backward)
             ("<delete>" . sp-splice-sexp-killing-forward)
             ("<left>" . sp-backward-slurp-sexp)
             ("<right>" . sp-forward-slurp-sexp)
             ("a" . sp-beginning-of-sexp)
             ("b" . sp-backward-sexp)
             ("C-<delete>" . sp-splice-sexp-killing-around)
             ("C-<left>" . sp-forward-barf-sexp)
             ("C-<right>" . sp-backward-barf-sexp)
             ("D" . sp-backward-down-sexp)
             ("d" . sp-down-sexp)
             ("e" . sp-end-of-sexp)
             ("E" . sp-up-sexp)
             ("f" . sp-forward-sexp)
             ("k" . sp-kill-sexp)
             ("n" . sp-next-sexp)
             ("p" . sp-previous-sexp)
             ("r" . sp-rewrap-sexp)
             ("s" . sp-split-sexp)
             ("t" . sp-transpose-sexp)
             ("w" . sp-copy-sexp))

  (smartparens-global-mode)
  (show-smartparens-global-mode)

  (setq sp-highlight-pair-overlay nil)
  (setq sp-show-pair-from-inside t)

  (sp-with-modes
      '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
    (sp-local-tag "i" "\"<" "\">")
    (sp-local-tag "i" "\\|" "\\|")
    (sp-local-tag "i" "\\[" "\\]"))

  (sp-local-pair '(org-mode) "\\[" "\\]")
  (sp-local-pair '(emacs-lisp-mode lisp-mode lisp-interaction-mode) "`" "'")
  (sp-local-pair '(emacs-lisp-mode lisp-mode lisp-interaction-mode) "`"
                 nil :when '(sp-in-string-p))
  (sp-local-pair '(emacs-lisp-mode lisp-mode lisp-interaction-mode) "'"
                 nil :actions nil)
  (setq sp-cancel-autoskip-on-backward-movement nil))

;; fonts
;; -----------------------------------------------------------------------------

(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 130)

(set-fontset-font "fontset-default"
                  (cons (decode-char 'ucs #xF000)
                        (decode-char 'ucs #xF4E3))
                  (font-spec :family "Font Awesome 5 Free" :size 16))

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font) charset (font-spec
                                        :family "Sarasa Mono TC"
                                        :size 16)))

(set-face-attribute 'fixed-pitch nil :height 120)

;; modeline
;; -----------------------------------------------------------------------------

(defvar display-time-24hr-format)
(setq display-time-24hr-format t
      display-time-day-and-date nil)
(display-time)

(column-number-mode 1)
(setq size-indication-mode t)

(file-name-shadow-mode t)
(use-package uniquify
  :config
  (setq uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-ignore-buffers-re "^\\*"))

(use-package spaceline-config
  :config
  (spaceline-define-segment buffer-encoding-abbrev
    "The line ending convention used in the buffer."
    (let ((buf-coding (format "%s" buffer-file-coding-system)))
      (if (eq major-mode 'pdf-view-mode)
          "PDF"
        (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
            (match-string 1 buf-coding)
          buf-coding))))

  (set-face-background 'spaceline-flycheck-info "gray20")
  (set-face-background 'spaceline-flycheck-error "gray20")
  (set-face-background 'spaceline-flycheck-warning "gray20")
  (setq spaceline-minor-modes-separator " | ")

  (spaceline-spacemacs-theme)
  (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
  (spaceline-toggle-minor-modes-on)
  (spaceline-toggle-flycheck-error-on)
  (spaceline-toggle-selection-info-on))

;; =============================================================================
;; General editing
;; =============================================================================

(setq-default sort-fold-case t)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq-default fill-column 80)
(setq-default indent-tabs-mode nil
              tab-width 4
              standard-indent 2
              tab-stop-list (number-sequence 2 120 2))
(setq tab-always-indent 'complete)
(use-package aggressive-indent
  :config (global-aggressive-indent-mode 1)
  :delight)

(delete-selection-mode)
(add-hook 'before-save-hook 'time-stamp)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)

(setq delete-by-moving-to-trash t)
(setq select-enable-clipboard t)
(global-subword-mode 1)

(setq view-read-only t)

(use-package whitespace
  :delight global-whitespace-mode
  :config
  (setq whitespace-line-column fill-column)
  (setq whitespace-style '(empty face indentation lines-tail
                                 space-after-tab space-before-tab
                                 spaces tabs trailing))
  (setq whitespace-global-modes '(prog-mode org-mode))
  (global-whitespace-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup))

(use-package highlight-indent-guides
  :config (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
  :delight)

;; Whitespace-mode need to be called before highlight-indent-guides, otherwise
;; no guides are shown.
;; (add-hook 'prog-mode-hook #'whitespace-mode)

(use-package volatile-highlights
  :config (volatile-highlights-mode t)
  :delight)

(use-package highlight-numbers
  :config (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; two-color scheme for blanket.
(use-package rainbow-delimiters
  :requires cl-lib
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

  (let ((me//paren-dual-colors '("deep sky blue" "navajo white")))
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (set-face-foreground
      (intern (format "rainbow-delimiters-depth-%d-face" index))
      (elt me//paren-dual-colors
           (if (cl-evenp index) 0 1))))))

(use-package wgrep
  :config (setq wgrep-auto-save-buffer t))

;; jump around
(use-package avy
  :bind (("M-j" . avy-goto-word-1)
         ("s-j" . avy-goto-line))
  :config (setq avy-case-fold-search nil))

(use-package golden-ratio-scroll-screen
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

(use-package visual-regex
  :bind ("M-s q" . vr/query-replace))

;; Searching
;; -----------------------------------------------------------------------------

(use-package helm-ag
  :bind ("M-s a" . helm-do-ag))

(use-package helm-swoop
  :bind ("M-s s" . helm-swoop)
  :config
  (define-key isearch-mode-map (kbd "M-i") #'helm-swoop-from-isearch))

;; Completion
;; -----------------------------------------------------------------------------

(setq abbrev-file-name (expand-file-name "abbrev_defs" me-emacs))

(use-package yasnippet
  :delight yas-minor-mode
  :config
  (setq yas-snippet-dirs `(,(expand-file-name "snippets" me-emacs-data)))
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode))

;; Encoding
;; -----------------------------------------------------------------------------

(let ((my-prefer-coding-system
       '(cp950 gb2312 cp936 gb18030 utf-16 utf-8)))
  (dolist (c my-prefer-coding-system)
    (prefer-coding-system c)))

;; =============================================================================
;; Bookkeeping
;; =============================================================================

(defun me//force-backup-of-buffer ()
  "Force to backup buffer."
  (setq-local buffer-backed-up nil))

(defun me//cleanup-old-files (directory nday)
  "Cleanup DIRECTORY files older than NDAY."
  (let ((age (* 60 60 24 (or nday 7)))
        (current (float-time (current-time))))
    (message "Deleting old backup files...")
    (dolist (file (directory-files directory t))
      (when (and (backup-file-name-p file)
                 (> (- current (float-time (nth 5 (file-attributes file))))
                    age))
        (message "delete: %s" file)
        (delete-file file)))))
(me//cleanup-old-files me-emacs-tmp 7)

;; backup!!!
(use-package files
  :config
  (setq auto-save-list-file-prefix
        (expand-file-name ".saves-" me-emacs-tmp))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

  (setq backup-directory-alist `(("." . ,me-emacs-tmp)))
  (setq backup-by-copying    t
        delete-old-versions  t
        kept-new-versions    30
        kept-old-versions    30
        version-control      t)

  ;; Backup buffer before each save.
  (add-hook 'before-save-hook #'me//force-backup-of-buffer))

;; Save recent visited file list.
(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" me-emacs-tmp))
  (setq recentf-max-saved-items 50)
  (let ((ignores `(,(expand-file-name ".*" me-emacs-tmp)
                   ,(expand-file-name ".mail/.*" me-home)
                   ,(expand-file-name ".cask/.*" user-emacs-directory)
                   ,(expand-file-name "/usr/.*"))))
    (mapc (lambda (x) (add-to-list 'recentf-exclude x)) ignores))
  (recentf-mode 1))

;; Save minibuffer history
(use-package savehist
  :config
  (setq savehist-additional-variables '(search ring regexp-search-ring))
  (setq savehist-file (expand-file-name "savehist" me-emacs-tmp))
  (savehist-mode))

;; Save file editing positions across sessions.
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" me-emacs-tmp))
  (save-place-mode))

;; Save *scratch* buffer content to files.
(use-package persistent-scratch
  :config
  (setq persistent-scratch-backup-directory
        (expand-file-name "scratch" user-emacs-directory))
  ;; keep backups not older than a month
  (setq persistent-scratch-backup-filter
        (persistent-scratch-keep-backups-not-older-than
         (days-to-time 30)))
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode))

;; Syntax check
(use-package flycheck
  :delight
  :config
  (bind-keys ("M-\"" . flycheck-keymap-prefix))
  (setq flycheck-python-flake8-executable "flake8")
  (define-prefix-command 'flycheck-keymap-prefix)
  (fset 'flycheck-keymap-prefix flycheck-command-map)
  (setq flycheck-keymap-prefix flycheck-command-map)
  (setq flycheck-global-modes '(not org-mode))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode))

;; Edit remote files
(use-package tramp
  :config (setq tramp-default-method "ssh"))

(use-package tramp-cache
  :config (setq tramp-persistency-file-name
                (expand-file-name "tramp" me-emacs-tmp)))

(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" me-emacs-data)))

;; =============================================================================
;; General utilities
;; =============================================================================

;; Hint on keybindings.
(use-package which-key
  :config (which-key-mode)
  :delight which-key-mode)

;; EShell
(use-package eshell
  :config (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer))
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;; Dired
;; -----------------------------------------------------------------------------

(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-listing-switches "-alh")
  (define-key dired-mode-map (kbd "b") #'helm-mini))

(use-package dired-x)

(use-package async
  :delight dired-async-mode
  :config (dired-async-mode))

(use-package ls-lisp
  :config
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-insert-directory-program nil))

;; Calendar
;; -----------------------------------------------------------------------------

(use-package solar
  :config
  (setq calendar-latitude 32.6)
  (setq calendar-longitude -85.5))

(use-package calendar
  :config
  (setq calendar-week-start-day 1)
  (setq calendar-chinese-all-holidays-flag t)
  (setq diary-file (expand-file-name "diary" me-emacs-data))
  (calendar-set-date-style 'iso)

  (defface calendar-iso-week-face
    '((default :weight bold :foreground "pink"))
    "Face for calendar week number"
    :group 'calendar)

  (defface calendar-iso-week-header-face
    '((default :foreground "cyan1"))
    "Face for calendar week title"
    :group 'calendar)

  (setq calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car (calendar-iso-from-absolute
                        (calendar-absolute-from-gregorian
                         (list month day year)))))
          'font-lock-face 'calendar-iso-week-face))

  (setq calendar-intermonth-header
        (propertize "Wk" 'font-lock-face 'calendar-iso-week-header-face)))

(use-package cal-china-x
  :config
  (setq calendar-mark-holidays-flag t
        calendar-view-diary-initially-flag nil
        calendar-view-holidays-initially-flag nil
        calendar-mark-diary-entries-flag t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)

  (setq holiday-other-holidays
        '((holiday-fixed 3  8  "婦女節")
          (holiday-fixed 3  12 "植樹節")
          (holiday-fixed 5  4  "青年節")
          (holiday-fixed 6  1  "兒童節")
          (holiday-fixed 9  10 "教師節")
          ;; 阴历节日
          (holiday-lunar 1 15  "元宵節 (上元節)")
          (holiday-lunar 2 2   "中和節 (青龍節，龍抬頭)")
          (holiday-lunar 3 3   "上巳節 (黄帝诞辰)")
          (holiday-lunar 7 7   "七夕節 (乞巧節)")
          (holiday-lunar 7 15  "中元節 (鬼節)")
          (holiday-lunar 9 9   "重陽節")))

  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays
                holiday-other-holidays))

  (set-face-background 'cal-china-x-important-holiday-face "dark red")
  (set-face-background 'cal-china-x-general-holiday-face "forest green"))

;; Show buffers
(use-package ibuffer
  :config
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 30 30 :left :elide) " "
                (size-h 9 -1 :right) " "
                (mode 24 24 :left :elide) " "
                filename-and-process))))

(use-package ibuf-ext
  :config
  (setq ibuffer-saved-filter-groups
        `(("default"
           ("Planner"
            (or (mode . org-agenda-mode)
                (name . "\\.bbdb")
                (mode . bbdb-mode)
                (name . "^\\*Calendar\\*$")
                (name . "^diary$")
                (name . "todo.org")
                (name . "gcal.org")
                (name . "time-machine.org")
                (filename . "Dropbox/plan.*")))
           ("Dired" (mode . dired-mode))
           ("Web"
            (or (name . "\\.js")
                (name . "\\.s?css")
                (name . "\\.html")
                (name . "\\.php")
                (name . "\\.xml")
                (mode . yaml-mode)))
           ("PDF"
            (or (mode . pdf-view-mode)))
           ("Bibliography"
            (or (filename . "bibliography")))
           ("Text"
            (or (name . "\\.\\(tex\\|bib\\|csv\\)")
                (mode . org-mode)
                (name . "\\.md")
                (mode . text-mode)))
           ("Emacs Config"
            (or (mode . emacs-lisp-mode)))
           ("Coding"
            (or (mode . shell-script-mode)
                (mode . sh-mode)
                (name . "\\.[ch]\\(pp\\|xx\\|\\+\\+\\)?")
                (mode . python-mode)
                (name . "\\.ya?ml")
                (name . "\\.R")
                (name . "\\.lua")))
           ("Mail"
            (or (mode . message-mode)
                (mode . mu4e-compose-mode)))
           ("Console"
            (or (mode . inferior-ess-mode)
                (mode . inferior-python-mode)
                (mode . eshell-mode)
                (mode . gnuplot-comint-mode)
                (mode . comint-mode)))
           ("Helper"
            (or (mode . makefile-mode)
                (mode . makefile-gmake-mode)
                (mode . cmake-mode)
                (mode . calc-mode)
                (mode . Info-mode)
                (mode . help-mode)
                (mode . ess-help-mode)
                (name . "^\\*scratch\\*$")))
           ("Image"
            (or (mode . image-mode)))
           ("Magit"
            (or (derived-mode . magit-mode)))
           ("Helm"
            (or (predicate string-match "Hmm" mode-name))))))

  (add-hook
   'ibuffer-mode-hook
   (lambda ()
     (ibuffer-auto-mode 1)
     (ibuffer-switch-to-saved-filter-groups "default")
     (local-set-key (kbd "<right>") 'ibuffer-forward-filter-group)
     (local-set-key (kbd "<left>") 'ibuffer-backward-filter-group)
     (hl-line-mode 1)))

  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond ((> (buffer-size) 1000)
           (format "%7.1fk" (/ (buffer-size) 1000.0)))
          ((> (buffer-size) 1000000)
           (format "%7.1fM" (/ (buffer-size) 1000000.0)))
          (t (format "%8dB" (buffer-size))))))

;; =============================================================================
;; Major modes
;; =============================================================================

(use-package cc-mode
  :config
  (add-hook 'c-mode-common-hook #'google-set-c-style)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

  (use-package clang-format
    :after c++-mode
    :bind (:map c++-mode-map
                ("C-!" . clang-format-region))))

(use-package web-mode
  :mode "\\.\\(html\\|htm\\)\\'"
  :config
  (defun me//init-web-mode()
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2)
    (setq web-mode-enable-current-element-highlight t))
  (add-hook 'web-mode-hook #'me//init-web-mode))

(defun me//init-python()
  (python-docstring-mode)
  (sphinx-doc-mode)
  (set (make-local-variable 'comment-inline-offset) 2)
  (setq fill-column 78))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :bind (:map python-mode-map
              ("C-!" . yapfify-region))
  :config
  (use-package sphinx-doc)
  (use-package yapfify)
  (add-hook 'python-mode-hook #'me//init-python))

(use-package json-mode
  :config
  (setq json-reformat:indent-width 2))

(use-package js2-mode
  :mode "\\.js\\'"
  :delight (js2-mode "JS2")
  :config
  (setq js2-basic-offset 2
        js-indent-level 2
        js2-include-node-externs t
        js2-include-browser-externs t))

(use-package image-mode
  :bind (:map image-mode-map
              ("H" . image-transform-fit-to-height)
              ("q" . quit-window)
              ("Q" . kill-this-buffer)
              ("r" . image-transform-set-rotation)
              ("W" . image-transform-fit-to-width)
              ("SPC" . image-transform-reset)))

(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
         ("/sshd?_config\\'" . ssh-config-mode)
         ("/known_hosts\\'" . ssh-known-hosts-mode)
         ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

(defun me//init-org ()
  "Init orgmode."
  (turn-on-auto-fill)
  (flyspell-mode)

  (defun me--org-show-context-advice (&rest ignore)
    (org-show-context 'default))
  (advice-add 'ispell-highlight-spelling-error :before
              'me--org-show-context-advice)

  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-modules '(org-bbdb org-bibtex org-clock org-gnus
                               org-habit org-table))
  (setq org-export-backends '(ascii beamer html latex md))

  :config
  (setq org-directory me-emacs-data)

  (add-hook 'org-mode-hook #'me//init-org)

  (setq org-adapt-indentation nil)
  (setq org-list-description-max-indent 0)
  (setq org-support-shift-select t)

  (add-to-list 'org-structure-template-alist '("A" "#+AUTHOR: ?" ""))
  (add-to-list 'org-structure-template-alist '("a" "#+BEGIN_abstract\n?\n#+END_abstract" ""))
  (add-to-list 'org-structure-template-alist '("B" "#+BIBLIOGRAPHY: ?" ""))
  (add-to-list 'org-structure-template-alist '("C" "#+CAPTION: ?" ""))
  (add-to-list 'org-structure-template-alist '("D" "#+DESCRIPTION: ?" ""))
  (add-to-list 'org-structure-template-alist '("K" "#+KEYWORDS: ?" ""))
  (add-to-list 'org-structure-template-alist '("M" "#+MACRO: ?" ""))
  (add-to-list 'org-structure-template-alist '("N" "#+NAME: ?" ""))
  (add-to-list 'org-structure-template-alist '("O" "#+OPTIONS: ?" ""))
  (add-to-list 'org-structure-template-alist '("p" "#+LATEX_HEADER: ?" ""))
  (add-to-list 'org-structure-template-alist '("R" "#+ATTR_BEAMER: ?" ""))
  (add-to-list 'org-structure-template-alist '("S" "#+SETUPFILE: ?" ""))
  (add-to-list 'org-structure-template-alist '("T" "#+TITLE: ?" ""))
  (add-to-list 'org-structure-template-alist '("X" "#+ATTR_LaTeX: ?" ""))

  (define-key org-mode-map [remap fill-paragraph] #'org-fill-paragraph)
  (define-key org-mode-map (kbd "C-c [") nil)

  ;; Recursive update todo statistics
  (setq org-provide-todo-statistics      t
        org-hierarchical-todo-statistics nil)

  ;; Fontify src blocks
  (setq org-src-fontify-natively t
        org-src-preserve-indentation t)

  (setq org-catch-invisible-edits 'smart)

  ;; Use prefix key as tag selection
  (setq org-use-fast-todo-selection t)

  ;; Bypassing logging if change state with Shift key
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  (setq org-todo-keywords
        '((sequence
           "TODO(t)" "NEXT(n)" "|"
           "DONE(d!)")
          (sequence
           "WAIT(w@/!)" "HOLD(h@/!)" "|"
           "KILL(k@)")))

  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red"          :weight bold)
          ("NEXT" :foreground "cyan"         :weight bold)
          ("DONE" :foreground "green"        :weight bold)
          ("WAIT" :foreground "yellow"       :weight bold)
          ("HOLD" :foreground "magenta"      :weight bold)
          ("KILL" :foreground "forest green" :weight bold)))

  (set-face-foreground 'org-level-1 (me//colir-blend "LightSalmon" "gray90" 0.3))
  (set-face-foreground 'org-level-2 (me//colir-blend "yellow" "gray90" 0.2))
  (set-face-foreground 'org-level-3 (me//colir-blend "LightGreen" "gray90" 0.3))
  (set-face-foreground 'org-level-4 (me//colir-blend "LightSkyBlue" "gray90" 0.3))
  (set-face-foreground 'org-level-5 (me//colir-blend "LightSkyBlue" "gray90" 0.1))
  (set-face-foreground 'org-level-6 (me//colir-blend "LightGreen" "gray90" 0.1))
  (set-face-foreground 'org-level-7 (me//colir-blend "yellow" "gray90" 0.1))
  (set-face-foreground 'org-level-8 (me//colir-blend "LightSalmon" "gray90" 0.1))

  (set-face-foreground 'org-special-keyword (me//colir-blend "SteelBlue" "gray60" 0.2))
  (set-face-foreground 'org-date (me//colir-blend "goldenrod" "gray70" 0.3))
  (set-face-foreground 'org-link (me//colir-blend "SteelBlue" "gray90" 0.2))
  (set-face-foreground 'org-footnote (me//colir-blend "SteelBlue" "gray90" 0.5))

  (setq org-time-stamp-custom-formats
        '("<%m/%d/%y %a>" . "<%Y-%m-%d %a %R %z>"))

  (setq org-use-fast-tag-selection 'auto)
  (load-file (expand-file-name "my-org-misc.el" org-directory)))

(defun me//org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun me//org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(use-package org-habit
  :config
  (setq org-habit-graph-column         50
        org-habit-preceding-days       28
        org-habit-following-days       1))

(use-package org-clock
  :config
  (setq org-clock-history-length 32
        org-clock-in-resume t)
  (setq org-log-into-drawer   t
        org-clock-into-drawer t)

  (setq org-clock-persist t)
  (org-clock-persistence-insinuate))

(use-package org-agenda
  :bind (:map org-agenda-mode-map
              ("F" . org-gcal-fetch))
  :config
  (setq org-agenda-include-diary t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)
  (setq org-agenda-files
        (expand-file-name "orgfile" org-directory))
  (setq org-agenda-dim-blocked-tasks            t
        org-agenda-compact-blocks               nil
        org-agenda-show-all-dates               t)

  (setq org-agenda-prefix-format
        '((agenda   . " %i %-15:c%?-12t% s")
          (timeline . "  % s")
          (todo     . " %i %-15:T")
          (tags     . " %i %-15:T")
          (search   . " %i %-15:T")))

  (setq org-agenda-tags-column         -100
        org-agenda-start-with-log-mode t)

  (setq org-agenda-custom-commands
        '(("d" "Daily agenda and all TODOs"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header
                    "High-priority unfinished tasks:")))
            (agenda "" ((org-agenda-ndays 1)))
            (alltodo ""
                     ((org-agenda-skip-function
                       '(or (me//org-skip-subtree-if-habit)
                            (me//org-skip-subtree-if-priority ?A)
                            (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header
                       "ALL normal priority tasks:"))))
           ((org-agenda-compact-blocks nil))))))

(use-package org-capture
  :config
  (setq org-capture-templates
        `(("m" "Save mail link" entry
           (file "todo.org")
           (file "capture/mail.org")
           :empty-lines 1
           :jump-to-captured t)

          ("r" "Related Work" item
           (file+headline "notes/bibliography/related/orphan.org" "Recent Work"))

          ("t" "TODO" entry
           (file "todo.org")
           (file "capture/todo.org")
           :empty-lines 1
           :jump-to-captured t)

          ("l" "Log daily" plain
           (file+olp+datetree "time-machine.org")
           "%?"
           :empty-lines 1
           :jump-to-captured t
           :tree-type week))))

(use-package ox-extra
  :config
  (ox-extras-activate '(ignore-headlines)))

(use-package ox-html
  :config
  (setq org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        org-html-allow-name-attribute-in-anchors t
        org-html-htmlize-output-type 'css)

  ;; Postamble.
  (setq org-html-postamble t
        org-html-postamble-format
        '(("en" "<a class=\"author\"
           href=\"http://gongzhitaao.org\">%a</a> / <span
           class=\"date\">%T</span><span class=\"creator\">%c</span>"))))

(use-package ox-latex
  :config
  (setq org-latex-prefer-user-labels t)
  (setq org-latex-pdf-process
        (quote ("PDFLATEX=%latex texi2dvi --shell-escape --pdf --clean --verbose --batch %f")))

  (setq org-latex-listings 'minted)
  ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
  ;; (add-to-list 'org-latex-packages-alist '("activate={true,nocompatibility},final,tracking=true,kerning=true,spacing=basictext,factor=1100,stretch=10,shrink=10" "microtype"))
  ;; (add-to-list 'org-latex-packages-alist '("" "geometry"))

  (add-to-list 'org-latex-classes
               '("ctexart"
                 "\\documentclass[11pt]{ctexart} [NO-DEFAULT-PACKAGES] [NO-PACKAGES] [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("IEEEtran"
                 "\\documentclass{IEEEtran} [NO-PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (setq org-latex-hyperref-template "\\hypersetup{
      pdfauthor={%a},
      pdftitle={%t},
      pdfkeywords={%k},
      pdfsubject={%d},
      pdfcreator={%c},
      pdflang={%L},
      bookmarks=true,
      unicode=true,
      pdftoolbar=true,
      pdfmenubar=true,
      pdffitwindow=false,
      pdfstartview={FitH},
      pdfnewwindow=true,
      colorlinks=true,
      linkcolor=Maroon,
      citecolor=ForestGreen,
      filecolor=Mulberry,
      urlcolor=MidnightBlue}\n"))

(use-package ox-beamer
  :config
  (add-to-list 'org-beamer-environments-extra '("only" "O" "\\only%a{" "}"))
  (add-to-list 'org-beamer-environments-extra '("action" "A" "\\action%a{" "}")))

;; Helper functions
;; -----------------------------------------------------------------------------

(use-package org-id)

(defun me//clean-up-heading (heading)
  "Clean up HEADING text."
  (replace-regexp-in-string "[^[:alpha:][:digit:][:space:]_-]" ""
                            (downcase heading)))

(defun me//org-id-from-heading (heading &optional level sep)
  "Format HEADING to use as custom ID and return it.

LEVEL is provided, the level of heading is prefixed.  This
function normalize HEADING by replacing spaces with SEP.  if SEP
is the separator used to glue different parts."
  (let ((sep (or sep "-")))
    (concat (when org-id-prefix (format "%s%s" org-id-prefix sep))
            (when level (format "h%d%s" level sep))
            (replace-regexp-in-string "\\s-+" sep
                                      (me//clean-up-heading heading))
            (concat sep (me//org-id-hash (concat (buffer-name) heading))))))

(defun me//org-id-hash (s &optional length)
  "Generate a unique string hash for S, truncated at LENGTH."
  (let ((length (or length 5))
        (hash (secure-hash 'sha1 s)))
    (substring-no-properties hash nil length)))

(defun me/org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.

If POM is nil, refer to the entry at point.  If the entry does
not have an CUSTOM_ID, the function returns nil.  However, when
CREATE is non nil, create a CUSTOM_ID if none is present already.
PREFIX will be passed through to `org-id-new'.  In any case, the
CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (me//org-id-from-heading (org-get-heading t t t t)
                                          (org-current-level)
                                          "-"))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

(defun me/org-custom-id-get-create (&optional force)
  "Create an ID w/o a suffix for the current entry and return it.

If the entry already has an ID, just return it.  With optional
argument FORCE, force the creation of a new ID."
  (interactive "P")
  (when (derived-mode-p 'org-mode)
    (when force
      (org-entry-put (point) "CUSTOM_ID" nil))
    (me/org-custom-id-get (point) 'create)))

(defun me/org-custom-id-get-create-all (&optional force)
  "Create custom ID for every heading.  Overwrite current if FORCE."
  (interactive "P")
  (when (derived-mode-p 'org-mode)
    (when force
      (org-entry-put (point) "CUSTOM_ID" nil))
    (let ((me//org-custom-id-get-wrapper
           (if force
               (lambda ()
                 (org-entry-put (point) "CUSTOM_ID" nil)
                 (me/org-custom-id-get (point) 'create))
             (lambda () (me/org-custom-id-get (point) 'create)))))
      (org-map-entries me//org-custom-id-get-wrapper))))

;; The gcal contains some senstive information, thus in a separate file.
;; -----------------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(require 'gcal-conf)

;; =============================================================================
;; Helm
;; =============================================================================

(use-package helm-mode
  :delight
  :config
  (setq helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t)
  (helm-mode 1))

(use-package helm-config
  :config
  (bind-keys ("M-x" . helm-M-x)
             ("C-x C-f" . helm-find-files)
             ("M-s a" . helm-do-ag)
             ("M-s g" . helm-do-grep-ag)
             ("C-b" . helm-mini)
             ("M-/" . helm-dabbrev)
             :map helm-map
             ("<tab>" . helm-execute-persistent-action)
             ("C-i" . helm-execute-persistent-action)
             ("C-z" . helm-select-action)
             :map isearch-mode-map
             ("M-i" . helm-occur-from-isearch)
             :map helm-moccur-map
             ("C-s" . me//isearch-from-helm-occur))

  (helm-autoresize-mode t)
  (helm-adaptive-mode 1)

  (set-face-background 'helm-selection "#097209")

  (global-set-key (kbd "C-c h") #'helm-command-prefix)

  (setq helm-split-window-inside-p t))

(defun me//isearch-from-helm-occur ()
  "Continue isearch from helm-occur."
  (interactive)
  (helm-run-after-exit
   (lambda (initial)
     (isearch-forward nil t)
     (isearch-yank-string initial))
   helm-pattern))

(use-package helm-buffers
  :config
  (setq helm-buffer-max-length 40
        helm-buffers-fuzzy-matching t))

(use-package helm-files
  :config
  (setq helm-ff-file-name-history-use-recentf t
        helm-ff-search-library-in-sexp t))

(use-package helm-for-files
  :config
  (setq helm-recentf-fuzzy-match t))

(use-package helm-elisp
  :config
  (setq helm-lisp-fuzzy-completion t))

(use-package helm-locate
  :config
  (setq helm-locate-fuzzy-match t))

(use-package helm-command
  :config
  (setq helm-M-x-fuzzy-match t))

(use-package flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . helm-flyspell-correct)))

(use-package helm-flyspell)

;; helm projectile
;; -----------------------------------------------------------------------------

(use-package projectile
  :config (projectile-mode 1)
  :delight)

(use-package helm-projectile
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

;; delight
;; -----------------------------------------------------------------------------

(delight
 '((auto-fill-function " " t)
   (auto-revert-mode " " autorevert)
   (flyspell-mode nil flyspell)
   (global-subword-mode nil subword)
   (isearch-mode " " t)
   (python-docstring-mode nil python-docstring)
   (subword-mode nil subword)
   (view-mode " " view)
   (whitespace-mode nil whitespace)))

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

;; ace-window
;; -----------------------------------------------------------------------------

;; (use-package ace-window
;;   :bind ("M-p" . ace-window)
;;   :config
;;   (setq aw-dispatch-always t))

;; x - delete window
;; m - swap (move) window
;; c - split window fairly, either vertically or horizontally
;; v - split window vertically
;; b - split window horizontally
;; n - select the previous window
;; i - maximize window (select which window)
;; o - maximize current window

;; helm-gtags
;; -----------------------------------------------------------------------------

;; (use-package helm-gtags
;;   :delight (helm-gtags-mode " ")
;;   :config
;;   (setq helm-gtags-suggested-key-mapping t)
;;   (setq helm-gtags-ignore-case t
;;         helm-gtags-auto-update t
;;         helm-gtags-display-style 'detail
;;         helm-gtags-direct-helm-completing t))

;; deft
;; -----------------------------------------------------------------------------

(use-package deft
  :bind ("<f8>" . deft)
  :config
  (setq deft-default-extension "org")
  (setq deft-directory (expand-file-name "notes" me-emacs-data))
  (setq deft-use-filename-as-title nil)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-recursive t)
  (setq deft-auto-save-interval 0)
  (setq deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase))))

;; Appt
;; -----------------------------------------------------------------------------

(appt-activate 1)
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; =============================================================================
;; mail
;; =============================================================================

(setq user-mail-address "zhitaao.gong@gmail.com"
      user-full-name "Zhitao Gong")

;; Core settings
;; -----------------------------------------------------------------------------

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(defun me//process-sent-messages ()
  "Post-process sent messages based on email address.

Gmail/IMAP takes care of copying sent messages to sent folder.
So we just delete it locally."
  (if (string-match-p "\\(@gmail\\.com\\)" (message-sendmail-envelope-from))
      'delete 'sent))

(use-package time
  :config
  (setq display-time-mail-string " "
        display-time-use-mail-icon nil))

(use-package mu4e
  :config
  (setq mu4e-maildir (expand-file-name ".mail" me-home))

  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Personal"
            :match-func
            (lambda (msg)
              (when msg (string-prefix-p
                         "/personal" (mu4e-message-field msg :maildir))))
            :vars
            `((mu4e-trash-folder . "/personal/trash")
              (mu4e-sent-folder . "/personal/sent")
              (mu4e-drafts-folder . "/personal/draft")
              (mu4e-refile-folder . "/personal/archive")
              (user-mail-address . "zhitaao.gong@gmail.com")
              (message-signature-file . ,(expand-file-name "signature/personal"
                                                           me-emacs-data))))
          ,(make-mu4e-context
            :name "Tiger"
            :match-func
            (lambda (msg)
              (when msg (string-prefix-p
                         "/tiger" (mu4e-message-field msg :maildir))))
            :vars
            `((mu4e-trash-folder . "/tiger/trash")
              (mu4e-sent-folder . "/tiger/sent")
              (mu4e-drafts-folder . "/tiger/draft")
              (mu4e-refile-folder . "/tiger/archive")
              (user-mail-address . "zzg0009@auburn.edu")
              (message-signature-file . ,(expand-file-name "signature/tiger"
                                                           me-emacs-data))))
          ,(make-mu4e-context
            :name "Reg"
            :match-func
            (lambda (msg)
              (when msg (string-prefix-p
                         "/reg" (mu4e-message-field msg :maildir))))
            :vars
            `((mu4e-trash-folder . "/reg/trash")
              (mu4e-sent-folder . "/reg/sent")
              (mu4e-drafts-folder . "/reg/draft")
              (mu4e-refile-folder . "/reg/archive")
              (user-mail-address . "zhitaao.gong.reg@gmail.com")
              (message-signature-file . ,(expand-file-name "signature/personal"
                                                           me-emacs-data))))))

  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-user-mail-address-list
        '("zzg0009@auburn.edu" "zzg0009@tigermail.auburn.edu" "gong@auburn.edu"
          "zhitaao.gong@gmail.com"
          "gongzhitaao@google.com" "gongzhitaao@fb.com"))

  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy nil)
  (setq mu4e-view-mode-hook '(bbdb-mua-auto-update))
  (setq mu4e-compose-complete-addresses t)
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-view-scroll-to-next nil)

  (setq mu4e-sent-messages-behavior #'me//process-sent-messages)

  (setq mu4e-attachment-dir (expand-file-name "Downloads" me-home))

  ;; (setq mu4e-get-mail-command "true")
  (setq mu4e-index-cleanup nil
        mu4e-index-lazy-check t)

  (setq mu4e-update-interval 300)

  (add-to-list 'mu4e-bookmarks
               '("flag:flagged AND NOT flag:trashed" "Flagged messages" ?f))

  (setq mu4e-use-fancy-chars t)

  (setq mu4e-headers-has-child-prefix    '("+"  . "○╮")
        mu4e-headers-empty-parent-prefix '("-"  . "● ")
        mu4e-headers-first-child-prefix  '("\\" . "╰─")
        mu4e-headers-duplicate-prefix    '("="  . "≡ ")
        mu4e-headers-default-prefix      '("|"  . "● "))

  (setq mu4e-headers-draft-mark     '("D" . "℮")
        mu4e-headers-flagged-mark   '("F" . "⚐")
        mu4e-headers-new-mark       '("N" . "⋆")
        mu4e-headers-passed-mark    '("P" . "⟫")
        mu4e-headers-replied-mark   '("R" . "")
        mu4e-headers-seen-mark      '("S" . "░")
        mu4e-headers-trashed-mark   '("T" . "✖")
        mu4e-headers-attach-mark    '("a" . "◥")
        mu4e-headers-encrypted-mark '("x" . "")
        mu4e-headers-signed-mark    '("s" . "☡")
        mu4e-headers-unread-mark    '("u" . "█"))

  (let ((marks '((refile  . ("r" . "▶"))
                 (delete  . ("D" . "✖"))
                 (flag    . ("+" . "⚑"))
                 (move    . ("m" . "▷"))
                 (read    . ("!" . "░"))
                 (trash   . ("d" . "♣"))
                 (unflag  . ("-" . "⚐"))
                 (untrash . ("=" . "♧"))
                 (unread  . ("?" . "█"))
                 (unmark  . (" " . " "))
                 (action  . ("a" . "◯"))
                 (something . ("*" . "♯")))))
    (dolist (elm marks)
      (plist-put (alist-get (car elm) mu4e-marks) :char (cdr elm))))

  (setq mu4e-headers-from-or-to-prefix '("" . "▶ "))

  (setq mu4e-headers-fields '((:human-date . 18)
                              (:flags . 10)
                              (:from-or-to . 30)
                              (:thread-subject)))

  (set-face-foreground 'mu4e-context-face "dark green")
  (set-face-foreground 'mu4e-modeline-face "DarkOrange4")
  (set-face-background 'mu4e-header-highlight-face "black")
  (set-face-foreground 'mu4e-cited-2-face "SteelBlue2"))

;; Contacts
;; -----------------------------------------------------------------------------

(use-package bbdb
  :config
  (bbdb-initialize 'message 'anniv 'mu4e)
  (setq bbdb-complete-mail-allow-cycling t
        bbdb-allow-duplicates t
        bbdb-message-all-addresses t
        bbdb-file (expand-file-name "contacts.bbdb.gz" me-emacs-data))
  (setq bbdb-mail-user-agent 'message-user-agent)
  (add-hook 'message-setup-hook 'bbdb-mail-aliases))

;; Message buffer
;; -----------------------------------------------------------------------------

(defun me//init-message ()
  "Init setup for message mode."
  (turn-on-orgstruct)
  (turn-on-orgstruct++)
  (turn-on-orgtbl)
  (turn-on-flyspell)
  (setq fill-column 80))

(use-package message
  :config
  (setq message-forward-ignored-headers ""
        message-make-forward-subject-function #'message-forward-subject-fwd)

  (setq message-forward-as-mime nil)
  (setq message-forward-before-signature nil)

  (setq message-citation-line-function #'message-insert-formatted-citation-line)
  (setq message-citation-line-format "On %a, %b %d %Y at %R, %N wrote:\n")

  (add-hook 'message-mode-hook #'me//init-message)
  (add-hook 'message-send-hook 'ispell-message))

;; sending mails
;; -----------------------------------------------------------------------------

(use-package sendmail
  :config
  (setq send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")
  (add-hook 'mail-send-hook  'ispell-message))

;; message notification, only in the modeline
;; -----------------------------------------------------------------------------

(use-package mu4e-alert
  :config
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))

;; Automatically start.
;;
;; The mbsync runs on 5, 10, 15, ... every hour.  It does not make sense for
;; mu4e to update before mbsync.  Thus we start mu4e on x+0.5min, where x is the
;; next nearest 5-min.
;; -----------------------------------------------------------------------------

(defun me//seconds-from-now (interval &optional wait)
  "Calculate INTERVAL+WAIT seconds from now."
  (let* ((m (mod (string-to-number (format-time-string "%M")) interval))
         (s (string-to-number (format-time-string "%S")))
         (elapsed (+ (* m 60) s))
         (w (or wait 30)))
    (if (< elapsed (- w 15))
        w
      (- (+ (* interval 60) w) elapsed))))

(defun me//start-mu4e-bg ()
  "Start mu4e in background."
  (mu4e t))
(run-at-time (me//seconds-from-now 5) nil #'me//start-mu4e-bg)

;; =============================================================================
;; Bibliography manager
;; =============================================================================

(defvar me-bib (expand-file-name ".local/data/bibliography" me-home)
  "My bibliography collection path.")
(defvar me-bib-files
  `(,(expand-file-name "refdb.bib" me-bib)
    ,(expand-file-name "orphan.bib" me-bib))
  "My bibliography files.")
(defvar me-bib-pdfs
  `(,(expand-file-name "pdf" me-bib)
    ,(expand-file-name "orphan" me-bib))
  "Paths containing my PDFs of the bibliography.")
(defvar me-bib-notes
  (expand-file-name "notes" me-bib)
  "Path to store my notes on each papers.")

(use-package helm-bibtex
  :bind ("C-c b" . helm-bibtex))

(defun me//init-bibtex ()
  "Init bibtex mode."
  (setq fill-column 140))

(defun me/bibtex-find-text-begin ()
  "Go to the beginning of a field entry."
  (interactive)
  (bibtex-find-text t))

(use-package bibtex
  :bind (:map bibtex-mode-map
              ([remap fill-paragraph] . bibtex-fill-entry)
              ([remap bibtex-clean-entry] . org-ref-clean-bibtex-entry)
              ("C-c C-v" . bibtex-validate)
              ("<backtab>" . me/bibtex-find-text-begin)
              ("M-<down>" . bibtex-end-of-entry)
              ("M-<up>" . bibtex-beginning-of-entry))
  :config
  (add-hook 'bibtex-mode-hook #'me//init-bibtex)

  (setq bibtex-dialect 'biblatex)
  (setq bibtex-align-at-equal-sign t)
  (setq bibtex-text-indentation 20)

  (add-to-list 'bibtex-entry-format 'unify-case)
  (add-to-list 'bibtex-entry-format 'sort-fields)
  (add-to-list 'bibtex-entry-format 'whitespace)
  (add-to-list 'bibtex-entry-format 'last-comma)

  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "_"
        bibtex-autokey-titlewords 1
        bibtex-autokey-titlewords-stretch 0
        bibtex-autokey-titleword-length nil)
  (setq bibtex-maintain-sorted-entries t)

  (setq bibtex-completion-bibliography me-bib-files
        bibtex-completion-library-path me-bib-pdfs
        bibtex-completion-notes-path me-bib-notes)
  (setq bibtex-completion-notes-extension ".org")
  (setq bibtex-completion-pdf-symbol "⚐"
        bibtex-completion-notes-symbol "✔"))

;; reftex
;; -----------------------------------------------------------------------------

(use-package reftex
  :diminish reftex-mode
  :config
  (add-hook 'tex-mode-hook #'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t
        reftex-ref-style-default-list '("Cleveref" "Hyperref" "Fancyref")
        reftex-default-bibliography me-bib-files))

;; org-ref
;; -----------------------------------------------------------------------------

(defun me//org-ref-notes-function (thekey)
  "Return the name of the note file by THEKEY."
  (bibtex-completion-edit-notes
   (list (car (org-ref-get-bibtex-key-and-file thekey)))))

(use-package org-ref
  :init
  (setq org-ref-default-bibliography me-bib-files
        org-ref-pdf-directory me-bib-pdfs
        org-ref-notes-directory me-bib-notes)

  (setq org-ref-ref-color (me//colir-blend "goldenrod" "grey90" 0.4))
  (setq org-ref-cite-color (me//colir-blend "dark sea green" "grey90" 0.4))
  (setq org-ref-show-citation-on-enter nil)

  :config
  (setq org-ref-notes-function #'me//org-ref-notes-function)
  (add-hook 'org-ref-clean-bibtex-entry-hook #'org-ref-downcase-bibtex-entry)

  (define-key org-ref-cite-keymap (kbd "M-<right>") #'org-ref-next-key)
  (define-key org-ref-cite-keymap (kbd "M-<left>") #'org-ref-previous-key)
  (define-key org-ref-cite-keymap (kbd "C-<left>") nil)
  (define-key org-ref-cite-keymap (kbd "C-<right>") nil)

  (setq bibtex-completion-display-formats
        `((article       . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${journal:20}  ${keywords:40}")
          (inbook        . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${chapter:20}  ${keywords:40}")
          (incollection  . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${booktitle:20}  ${keywords:40}")
          (inproceedings . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${booktitle:20}  ${keywords:40}")
          (t             . ,(format "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  %s  ${keywords:40}" (make-string 20 ? )))))
  (setq bibtex-completion-additional-search-fields '(keywords journal booktitle)))

;; =============================================================================
;; Working with PDF
;; =============================================================================

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install))

;; Helper functions
;; -----------------------------------------------------------------------------

(defun me/pdf-view-next-few-lines ()
  "Scroll down few lines."
  (interactive)
  (pdf-view-next-line-or-next-page 10))

(defun me/pdf-view-prev-few-lines ()
  "Score up few lines."
  (interactive)
  (pdf-view-previous-line-or-previous-page 10))

;; copied directly from view-window-size
(defun me//window-size ()
  "Return the height of the current window, excluding the mode line.
Using `window-line-height' accounts for variable-height fonts."
  (let ((h (window-line-height -1)))
    (if h
        (1+ (nth 1 h))
      ;; This should not happen, just in case `window-line-height' returns
      ;; nil, fall back on `window-height'.
      (1- (window-height)))))

(defun me/pdf-view-scroll-half-forward ()
  "Score down half page."
  (interactive)
  (pdf-view-next-line-or-next-page (/ (me//window-size) 2)))

(defun me/pdf-view-scroll-half-backward ()
  "Score up half page."
  (interactive)
  (pdf-view-previous-line-or-previous-page (/ (me//window-size) 2)))

(defun me/org-ref-open-entry ()
  "Open bibtex file to key with which the note associated."
  (interactive)
  (let* ((key (cond
               ((derived-mode-p 'org-mode)
                (condition-case nil
                    (save-excursion
                      (org-ref-get-bibtex-key-under-cursor))
                  (error (file-name-base (buffer-file-name)))))
               ((derived-mode-p 'pdf-view-mode)
                (file-name-base (buffer-file-name)))
               (t nil)))
         (bibfile ""))
    (if key
        (progn
          (setq bibfile (cdr (org-ref-get-bibtex-key-and-file key)))
          (find-file bibfile)
          (bibtex-search-entry key))
      (message "Non existing key %s" key))))

(defun me/org-ref-open-note ()
  "Open the associated note file."
  (interactive)
  (let* ((key (cond
               ((derived-mode-p 'org-mode)
                (ignore-errors (org-ref-get-bibtex-key-under-cursor)))
               ((derived-mode-p 'bibtex-mode)
                (save-excursion
                  (bibtex-beginning-of-entry)
                  (reftex-get-bib-field "=key=" (bibtex-parse-entry t))))
               ((derived-mode-p 'pdf-view-mode)
                (file-name-base (buffer-file-name)))
               (t nil)))
         (pdf-file (funcall org-ref-get-pdf-filename-function key)))
    (if (file-exists-p pdf-file)
        (org-ref-open-notes-at-point key)
      (message "Not open note for non-existing PDF %s" key))))

(use-package pdf-view
  :bind (:map pdf-view-mode-map
              ("<delete>" . pdf-view-scroll-up-or-next-page)
              ("<down>" . me/pdf-view-next-few-lines)
              ("<left>" . pdf-view-previous-page-command)
              ("<right>" . pdf-view-next-page-command)
              ("<up>" . me/pdf-view-prev-few-lines)
              ("b" . helm-mini)
              ("c" . me/org-ref-open-entry)
              ("d" . me/pdf-view-scroll-half-forward)
              ("e" . me/pdf-view-scroll-half-backward)
              ("g" . pdf-view-goto-page)
              ("j" . me/pdf-view-scroll-half-forward)
              ("k" . me/pdf-view-scroll-half-backward)
              ("n" . me/org-ref-open-note)
              ("z" . delete-other-windows))
  :config (setq pdf-view-midnight-colors '("#e5e5e5" . "#333333")))

;; helper functions
;; -----------------------------------------------------------------------------

(defun me//getkey-orgref ()
  "Get the year part of orgref citation.

My bib key is (lastname)(YYYY)-(title), where title is the first
non-trivial word in title, This function will
return (YYYY)(lastname)(title).  Note the parenthesis is only for
readability, no parenthesis actually exist."
  (save-excursion
    (if (re-search-forward org-ref-cite-re nil t)
        (let* ((bibkey (match-string 0))
               (YYYY-re "\\([0-9]\\{4\\}\\)"))
          (string-match YYYY-re bibkey)
          (concat (match-string 0 bibkey)
                  (replace-regexp-in-string YYYY-re "" bibkey)))
      "")))

(defun me/org-sort-orgref-citation-list-by-year
    (&optional with-case sorting-type)
  "Sort the list of citations by year.

Case sensitive if WITH-CASE.  SORTING-TYPE is not used here.

The list looks like:
- [X] cite:someone2017 dummy
- [ ] cite:others2013 dummy
- [ ] cite:hello2018 dummy

I want to sort the list first by year (newest first), then
alphabetically (in ascending or descending order)."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (cond ((org-at-item-p)
           (org-sort-list with-case ?F #'me//getkey-orgref))
          ((org-at-heading-p)
           (org-sort-entries with-case ?F #'me//getkey-orgref))
          (t nil))))

;; The following three functions jump among PDF, bibtex entry and note.  For
;; instance, me/org-ref-open-pdf opens the PDF file when your cursor is at the
;; bibtex entry or the note that is associated with this bibtex entry.

(defun me/org-ref-open-pdf (&optional arg)
  "Open the associated PDF.

If ARG, open with external program.  Otherwise open in Emacs."
  (interactive "P")
  (let* ((key (cond
               ((derived-mode-p 'org-mode)
                (condition-case nil
                    (save-excursion
                      (org-ref-get-bibtex-key-under-cursor))
                  (error (file-name-base (buffer-file-name)))))
               ((derived-mode-p 'bibtex-mode)
                (save-excursion
                  (bibtex-beginning-of-entry)
                  (reftex-get-bib-field "=key=" (bibtex-parse-entry t))))
               (arg
                (file-name-base (buffer-name)))
               (t nil)))
         (pdf-file (funcall org-ref-get-pdf-filename-function key)))
    (if (file-exists-p pdf-file)
        (if arg
            (start-process "xdg-open" nil "setsid" "xdg-open" pdf-file)
          (org-open-file pdf-file))
      (if (derived-mode-p 'pdf-view-mode)
          (message "Already opened")
        (message "No PDF found with name %s" pdf-file)))))

(define-pdf-cache-function pagelabels)
(defun me//pdf-view-page-number ()
  "For telephone-mode line."
  (interactive)
  (if (called-interactively-p 'interactive)
      (message "[pg %s/%s/%s]"
               (nth (1- (pdf-view-current-page))
                    (pdf-cache-pagelabels))
               (number-to-string (pdf-view-current-page))
               (number-to-string (pdf-cache-number-of-pages)))
    (format "[%s/%s]"
            (number-to-string (pdf-view-current-page))
            (number-to-string (pdf-cache-number-of-pages)))))

;; =============================================================================
;; Key logger
;; =============================================================================

(open-dribble-file
 (expand-file-name
  (format-time-string "key-%FT%H%M%S.log") me-keylog))

;; The undo and Evil.  It is weird that I have to load undo-tree at the end,
;; otherwise some pakcages, e.g., helm-elisp fails loading.
;; -----------------------------------------------------------------------------

(use-package undo-tree
  :init (global-undo-tree-mode)
  :delight
  :bind ("C-c u" . undo-tree-visualize)
  :config
  (setq undo-tree-enable-undo-in-region t)
  (setq undo-tree-history-directory-alist `(("." . ,me-emacs-tmp)))
  (setq undo-tree-auto-save-history nil)
  (setq undo-limit 80000))

;; Evil
;; -----------------------------------------------------------------------------

(use-package view)
(use-package evil
  :config
  (evil-mode 1)

  (bind-keys :map evil-normal-state-map
             ("<escape>" . evil-emacs-state)
             ("C-z"      . delete-other-windows)
             ("Q"        . kill-this-buffer)
             ("q"        . bury-buffer)
             ("s"        . helm-swoop)
             :map evil-emacs-state-map
             ("<escape>" . evil-force-normal-state)
             ("C-z"      . delete-other-windows)
             :map evil-motion-state-map
             ("<backspace>" . View-scroll-half-page-backward)
             ("<delete>"    . View-scroll-half-page-forward)
             ("C-e"         . move-end-of-line)
             ("b"           . helm-mini))

  (setq cursor-type 'box)
  (setq evil-insert-state-cursor "chartreuse3"
        evil-emacs-state-cursor "SkyBlue2"
        evil-normal-state-cursor "DarkGoldenrod2"
        evil-visual-state-cursor "gray"
        evil-motion-state-cursor "plum3")

  (evil-make-overriding-map help-mode-map 'motion)

  ;; Use emacs state instead of insert mode
  (defalias 'evil-insert-state 'evil-emacs-state)
  (setq-default evil-shift-width 2)

  (mapc (lambda (x) (add-to-list 'evil-emacs-state-modes x))
        '(dired-mode image-dired-thumbnail-mode image-mode mu4e-compose-mode)))

;; =============================================================================
;; Now start the server
;; =============================================================================

(require 'server)
(unless (server-running-p) (server-start))

;;; init.el ends here
