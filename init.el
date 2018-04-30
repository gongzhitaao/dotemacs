;;; init.el --- Yet another Emacs config
;; Time-stamp: <2018-04-30 13:17:13 gongzhitaao>

;;; Naming conventions:
;; me/xxx: mostly interactive functions, may be executed with M-x or keys
;; me//xxx: internal helper functions, not called directly by user
;; me-xxx: custom variables

;;; Code:

;; Added by Package.el.  This must come before configurations of installed
;; packages.  Don't delete this line.  If you don't want it, just comment it out
;; by adding a semicolon to the start of the line.  You may delete these
;; explanatory comments.
(package-initialize)

;; -------------------------------------------------------------------
;; Key binding
;; -------------------------------------------------------------------

(global-set-key (kbd "C-z") #'delete-other-windows)
(global-set-key (kbd "<backtab>") #'decrease-left-margin)
(global-set-key (kbd "<escape>") #'view-mode)

(global-set-key (kbd "C-|") #'fci-mode)
(global-set-key (kbd "C->") #'me/join-next-line)
(global-set-key (kbd "s-;") #'comment-or-uncomment-region)

;; FN keys
;; --------------------------------------------------------------------

(global-set-key (kbd "<f5>") #'bookmark-bmenu-list)
(global-set-key (kbd "<f6>") #'calendar)
;; f8 -- deft
;; f10 -- menu
(global-set-key (kbd "<f11>") #'ispell)
(global-set-key (kbd "<f12>") #'mu4e)

;; Remaping
;; --------------------------------------------------------------------

(global-set-key [remap execute-extended-command] #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key [remap isearch-backward] #'isearch-backward-regexp)
(global-set-key [remap isearch-forward]  #'isearch-forward-regexp)
(global-set-key [remap list-buffers]     #'ibuffer)
(global-set-key [remap switch-to-buffer] #'helm-mini)
(global-set-key [remap yank-pop]         #'helm-show-kill-ring)

;; C-c user key
;; --------------------------------------------------------------------

;; C-c b -- helm-bibtex
;; C-c d -- drag-stuff-mode
(global-set-key (kbd "C-c f") #'me-buffer-file-command-prefix)
;; C-c g -- magit-status
(global-set-key (kbd "C-c m") #'me-multiple-cursors-command-prefix)
(global-set-key (kbd "C-c o") #'me-org-command-prefix)
;; C-c s -- smartparens
;; C-c u -- undo-tree
(global-set-key (kbd "C-c =") #'align-regexp)

(global-set-key (kbd "C-c C-q") #'bury-buffer)

;; M- meta keys
;; --------------------------------------------------------------------

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
(global-set-key (kbd "M-s g") #'helm-do-grep-ag)
;; M-s h highlight-xxx
(global-set-key (kbd "M-s q") #'vr/query-replace)
;; M-s s helm-swoop

;; -------------------------------------------------------------------
;; Variable
;; -------------------------------------------------------------------

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

;; -------------------------------------------------------------------
;; Helper
;; -------------------------------------------------------------------

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

(me-with-region-or-line #'comment-or-uncomment-region)
(me-with-region-or-line #'kill-ring-save)

(defun me/join-next-line (arg)
  "Join this line with next line.

Convenient extension to `delete-indentation' which joins this
line with previous line."
  (interactive "P")
  (save-excursion
    (forward-line)
    (delete-indentation arg)))

;; -------------------------------------------------------------------
;; command map groups
;; -------------------------------------------------------------------

;; file command map

(defvar me-buffer-file-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'magit-log-buffer-file)
    (define-key map (kbd "s") #'set-buffer-file-coding-system)
    (define-key map (kbd "s") #'me/sudo-edit)
    (define-key map (kbd "v") #'add-file-local-variable)
    (define-key map (kbd "V") #'add-file-local-variable-prop-line)
    map)
  "Buffer file related commands.")

(defvar me-buffer-file-command-prefix nil
  "Prefix key for my mnemonic file related commands.")
(define-prefix-command 'me-buffer-file-command-prefix)
(fset 'me-buffer-file-command-prefix me-buffer-file-command-map)
(setq me-buffer-file-command-prefix me-buffer-file-command-map)

;; org command map

(defvar me-org-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'org-agenda)
    (define-key map (kbd "c") #'org-capture)
    (define-key map (kbd "e") #'me/org-ref-open-entry)
    (define-key map (kbd "h") #'me/org-custom-id-get-create)
    (define-key map (kbd "H") #'me/org-custom-id-get-create-all)
    (define-key map (kbd "l b") #'org-ref-extract-bibtex-entries)
    (define-key map (kbd "l f") #'org-ref-list-of-figures)
    (define-key map (kbd "l t") #'org-ref-list-of-tables)
    (define-key map (kbd "s") #'me/org-sort-orgref-citation-list-by-year)
    (define-key map (kbd "n") #'me/org-ref-open-note)
    (define-key map (kbd "p") #'me/org-ref-open-pdf)
    map)
  "Org mode related commands.")

(defvar me-org-command-prefix nil
  "Prefix key for my mnemonic orgmode related commands.")
(define-prefix-command 'me-org-command-prefix)
(fset 'me-org-command-prefix me-org-command-map)
(setq me-org-command-prefix me-org-command-map)

;; -------------------------------------------------------------------
;; Misc
;; -------------------------------------------------------------------

(setq display-time-24hr-format t
      display-time-day-and-date nil)
(display-time)

(setq confirm-kill-emacs 'yes-or-no-p)

(tooltip-mode -1)

(setq-default indent-tabs-mode nil
              tab-width 2
              standard-indent 2
              tab-stop-list (number-sequence 2 120 2))

(setq tab-always-indent 'complete)

(delete-selection-mode)
(add-hook 'before-save-hook 'time-stamp)

(blink-cursor-mode 0)
(setq scroll-preserve-screen-position t)
(mouse-avoidance-mode 'animate)

(tool-bar-mode 0)
(menu-bar-mode 1)

(scroll-bar-mode 0)
(setq scroll-margin 0
      scroll-preserve-screen-position 1)

(global-subword-mode 1)
(setq visible-bell t)

(setq inhibit-startup-message t
      resize-mini-windows t)

(column-number-mode 1)
(setq size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(file-name-shadow-mode t)

(setq-default frame-title-format
              '(("" invocation-name "@" system-name "\t")
                (buffer-file-name "%f" "%b")))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)

(setq delete-by-moving-to-trash t)

(setq-default fill-column 80)

(setq custom-file (expand-file-name "custom.el" me-emacs))

(if (file-exists-p custom-file)
    (load custom-file))

(add-hook 'focus-out-hook #'garbage-collect)

(setq select-enable-clipboard t)

;; -------------------------------------------------------------------
;; backup
;; -------------------------------------------------------------------

(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" me-emacs-data))

(setq auto-save-list-file-prefix
      (expand-file-name ".saves-" me-emacs-tmp))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq backup-directory-alist `((".*" . ,me-emacs-tmp)))
(setq backup-by-copying    t
      delete-old-versions  t
      kept-new-versions    6
      kept-old-versions    2
      version-control      t)

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

;; -------------------------------------------------------------------
;; Encoding
;; -------------------------------------------------------------------

(let ((my-prefer-coding-system
       '(cp950 gb2312 cp936 gb18030 utf-16 utf-8)))
  (dolist (c my-prefer-coding-system)
    (prefer-coding-system c)))

;; -------------------------------------------------------------------
;; Dired
;; -------------------------------------------------------------------

(require 'dired)
(require 'dired-x)

(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-listing-switches "-alh")

(define-key dired-mode-map (kbd "b") #'helm-mini)

;; -------------------------------------------------------------------
;; recentf
;; -------------------------------------------------------------------

(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" me-emacs-tmp))
(setq recentf-max-saved-items 20)
(add-to-list 'recentf-exclude (expand-file-name ".*" me-emacs-tmp))
(add-to-list 'recentf-exclude (expand-file-name "~/.newsrc*"))
(add-to-list 'recentf-exclude (expand-file-name "Mail/.*" me-home))
(add-to-list 'recentf-exclude (expand-file-name ".cask/.*" user-emacs-directory))
(add-to-list 'recentf-exclude (expand-file-name "/usr/.*"))
(recentf-mode 1)

;; -------------------------------------------------------------------
;; savehist
;; -------------------------------------------------------------------

(require 'savehist)
(setq savehist-additional-variables '(search ring regexp-search-ring)
      savehist-file (expand-file-name "savehist" me-emacs-tmp))
(savehist-mode)

;; -------------------------------------------------------------------
;; saveplace
;; -------------------------------------------------------------------

(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" me-emacs-tmp))
(save-place-mode)

;; -------------------------------------------------------------------
;; Eshell
;; -------------------------------------------------------------------

(require 'eshell)
(add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

;; -------------------------------------------------------------------
;; uniquify
;; -------------------------------------------------------------------

(require 'uniquify)
(setq uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-ignore-buffers-re "^\\*")

;; -------------------------------------------------------------------
;; abbrev
;; -------------------------------------------------------------------

(setq abbrev-file-name (expand-file-name "abbrev_defs" me-emacs))

;; -------------------------------------------------------------------
;; whitespace
;; -------------------------------------------------------------------

(require 'whitespace)
(setq whitespace-line-column fill-column)
(setq whitespace-style '(face trailing tabs spaces lines-tail
                              empty indentation space-after-tab
                              space-before-tab))

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'org-mode-hook 'whitespace-mode)
(add-hook 'tex-mode-hook 'whitespace-mode)
(add-hook 'message-mode-hook 'whitespace-mode)

;; -------------------------------------------------------------------
;; view mode
;; -------------------------------------------------------------------

(require 'view)

(defun me//view-mode-indicator ()
  "Change cursor when variable ‘view-mode’ or read-only."
  (cond (view-mode
         (set-cursor-color "red"))
        (buffer-read-only
         (set-cursor-color "cyan"))
        (t
         (set-cursor-color "white"))))

(add-hook 'post-command-hook #'me//view-mode-indicator)
(define-key view-mode-map (kbd "<delete>") #'View-scroll-page-forward)
(define-key view-mode-map (kbd "^") #'dired-jump) ; relies on dired+
(define-key view-mode-map (kbd "a") #'move-beginning-of-line)
(define-key view-mode-map (kbd "b") #'helm-mini)
(define-key view-mode-map (kbd "e") #'move-end-of-line)
(define-key view-mode-map (kbd "f") #'helm-find-files)
(define-key view-mode-map (kbd "j") #'View-scroll-line-forward)
(define-key view-mode-map (kbd "k") #'View-scroll-line-backward)
(define-key view-mode-map (kbd "o") #'other-window)
(define-key view-mode-map (kbd "q") #'bury-buffer)
(define-key view-mode-map (kbd "s") #'helm-swoop)
(define-key view-mode-map (kbd "Q") #'View-kill-and-leave)
(define-key view-mode-map (kbd "z") #'delete-other-windows)

(setq view-read-only t)

;; -------------------------------------------------------------------
;; Theme
;; -------------------------------------------------------------------

(add-to-list 'default-frame-alist '(background-color . "gray20"))
(add-to-list 'default-frame-alist '(foreground-color . "gray90"))

(global-hl-line-mode)
(set-face-background 'hl-line "gray10")

;; -------------------------------------------------------------------
;; Vendor minimal
;;
;; The above is the minimal configuration for builtin packages or
;; functionalities, the followings include third-party library for
;; general extended efficiency.
;; -------------------------------------------------------------------

;; With Cask, the following package-archives is not necessary

;; (setq
;;  package-archives
;;  '(("melpa-stable" . "https://stable.melpa.org/packages/")
;;    ("melpa"        . "https://melpa.org/packages/")
;;    ("marmalade"    . "https://marmalade-repo.org/packages/")
;;    ("org"          . "http://orgmode.org/elpa/")
;;    ("gnu"          . "https://elpa.gnu.org/packages/")
;;    ("sc"           . "http://joseito.republika.pl/sunrise-commander/")))

(defun me//init-package ()
  "Load packages manually in just in case cask fails.

If cask fails mysteriously, use the following code to get things
going, at least for now.  Basically add every package path to
`load-path', and autoload the functions."
  (dolist (elem
           (directory-files-and-attributes
            "~/.emacs.d/.cask/24.5/elpa/" t))
    (let ((path (car elem))
          (dir (cadr elem))
          (auto nil))
      (if (and dir
               (not (string= path "."))
               (not (string= path "..")))
          (progn
            (add-to-list 'load-path path)
            (setq auto (directory-files path nil "-autoloads\.el$"))
            (if auto (autoload (car auto))))))))
;; (me//init-package)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; -------------------------------------------------------------------
;; exec-path
;; -------------------------------------------------------------------

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; -------------------------------------------------------------------
;; Helm
;; -------------------------------------------------------------------

(use-package helm-config
  :diminish helm-mode
  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  (helm-adaptive-mode 1)

  (set-face-background 'helm-selection "#097209")

  (global-set-key (kbd "C-c h") #'helm-command-prefix)

  (setq helm-recentf-fuzzy-match               t
        helm-buffers-fuzzy-matching            t
        helm-locate-fuzzy-match                t
        helm-split-window-in-side-p            t
        helm-ff-search-library-in-sexp         t
        helm-ff-file-name-history-use-recentf  t
        helm-completion-in-region-fuzzy-match  t
        helm-mode-fuzzy-match                  t
        helm-semantic-fuzzy-match              t
        helm-lisp-fuzzy-completion             t
        helm-imenu-fuzzy-match                 t
        helm-M-x-fuzzy-match                   t)

  ;; rebind tab to run persistent action
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; make TAB works in terminal
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  ;; list actions using C-z
  (define-key helm-map (kbd "C-z")  #'helm-select-action)
  (setq helm-buffer-max-length 40))

(use-package helm-files
  :after helm-config)

(use-package helm-regexp
  :defer t
  :init (bind-key "M-i" #'helm-occur-from-isearch isearch-mode-map)
  :config
  (defun isearch-from-helm-occur ()
    (interactive)
    (helm-run-after-exit
     (lambda (initial)
       (isearch-forward nil t)
       (isearch-yank-string initial))
     helm-pattern))
  (bind-key "C-s" #'isearch-from-helm-occur helm-moccur-map))

;; -------------------------------------------------------------------
;; helm projectile
;; -------------------------------------------------------------------

(use-package projectile
  :config (projectile-mode 1)
  :diminish projectile-mode)
(use-package helm-projectile
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

;; -------------------------------------------------------------------
;; volatile-highlights
;; -------------------------------------------------------------------

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

;; -------------------------------------------------------------------
;; diminish
;; -------------------------------------------------------------------

(diminish 'auto-fill-function " ")
(diminish 'isearch-mode " ")
(diminish 'whitespace-mode)
(diminish 'global-whitespace-mode)
(diminish 'global-visual-line-mode)
(diminish 'visual-line-mode)

(defun me//diminish-flyspell ()
  (diminish 'flyspell-mode))
(add-hook 'flyspell-mode-hook #'me//diminish-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(defun me//diminish-view ()
  "Diminish variable `view-mode'."
  (diminish 'view-mode " "))
(add-hook 'view-mode-hook #'me//diminish-view)

(defun me//diminish-auto-revert ()
  "Set icon for command `auto-revert-mode'."
  (diminish 'auto-revert-mode " "))
(add-hook 'auto-revert-mode-hook #'me//diminish-auto-revert)

;; -------------------------------------------------------------------
;; dired+
;; -------------------------------------------------------------------

;; (use-package dired+
;;   :load-path "site-lisp")

(use-package ls-lisp
  :config
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-insert-directory-program nil))

;; -------------------------------------------------------------------
;; Async
;; -------------------------------------------------------------------

(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; -------------------------------------------------------------------
;; Async
;; -------------------------------------------------------------------

(use-package async
  :diminish dired-async-mode
  :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

;; -------------------------------------------------------------------
;; visual regexp steroids
;; -------------------------------------------------------------------

(use-package visual-regexp-steroids)

;; -------------------------------------------------------------------
;; magit
;; -------------------------------------------------------------------

(use-package magit
  :bind ("C-c g" . magit-status)
  :config
  (setq git-commit-major-mode 'org-mode))

;; -------------------------------------------------------------------
;; multiple cursors
;; -------------------------------------------------------------------

(use-package multiple-cursors
  :config
  (defvar me-multiple-cursors-command-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-a") #'mc/edit-beginnings-of-lines)
      (define-key map (kbd "C-e") #'mc/edit-ends-of-lines)
      (define-key map (kbd "a") #'mc/mark-all-like-this-dwim)
      (define-key map (kbd "l") #'mc/edit-lines)
      map)
    "Multiple cursors editing key mappings.")

  (defvar me-multiple-cursors-command-prefix nil
    "Prefix key for multiple cursors editing.")
  (define-prefix-command 'me-multiple-cursors-command-prefix)
  (fset 'me-multiple-cursors-command-prefix me-multiple-cursors-command-map)
  (setq me-multiple-cursors-command-prefix me-multiple-cursors-command-map))

;; -------------------------------------------------------------------
;; tramp
;; -------------------------------------------------------------------

(use-package tramp
  :config
  (setq tramp-default-method "ssh"
        tramp-persistency-file-name (expand-file-name "tramp"
                                                      me-emacs-tmp)))

;; -------------------------------------------------------------------
;; writeroom
;; -------------------------------------------------------------------

(use-package writeroom-mode
  :config
  (setq writeroom-fullscreen-effect nil)
  (setq writeroom-maximize-window nil)
  (setq writeroom-width (+ fill-column 15))
  (setq writeroom-major-modes
        '(prog-mode dired-mode conf-mode Info-mode calendar-mode prog-mode
                    tex-mode org-mode mu4e-compose-mode))
  (setq writeroom-mode-line t)
  (delete 'writeroom-set-menu-bar-lines writeroom-global-effects)
  (global-writeroom-mode))

;; -------------------------------------------------------------------
;; regex builder
;; -------------------------------------------------------------------

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

;; -------------------------------------------------------------------
;; highlight-indent-guides
;; -------------------------------------------------------------------

(use-package highlight-indent-guides)
(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)

;; Whitespace-mode need to be called before highlight-indent-guides, otherwise
;; no guides are shown.
(add-hook 'prog-mode-hook #'whitespace-mode)

;; -------------------------------------------------------------------
;; ace-window
;; -------------------------------------------------------------------

(use-package ace-window
  :bind ("M-p" . ace-window)
  :config
  (setq aw-dispatch-always t))

;; x - delete window
;; m - swap (move) window
;; c - split window fairly, either vertically or horizontally
;; v - split window vertically
;; b - split window horizontally
;; n - select the previous window
;; i - maximize window (select which window)
;; o - maximize current window

;; -------------------------------------------------------------------
;; ace-jump
;; -------------------------------------------------------------------

(use-package ace-jump
  :bind ("M-j" . ace-jump-mode)
  :init
  (setq ace-jump-mode-case-fold nil))

;; -------------------------------------------------------------------
;; anzu
;; -------------------------------------------------------------------

(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode +1)
  (setq anzu-search-threshold 1000)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "green4" :weight 'bold))

;; -------------------------------------------------------------------
;; fci
;; -------------------------------------------------------------------

(use-package fill-column-indicator
  :config
  (setq fci-rule-color "gray50")
  (add-hook 'prog-mode-hook #'turn-on-fci-mode)
  (add-hook 'org-mode-hook #'turn-on-fci-mode)
  (add-hook 'tex-mode-hook #'turn-on-fci-mode)
  (add-hook 'message-mode-hook #'turn-on-fci-mode))

;; -------------------------------------------------------------------
;; persistent scratch
;; -------------------------------------------------------------------

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

;; -------------------------------------------------------------------
;; helm-gtags
;; -------------------------------------------------------------------

(use-package helm-gtags
  :diminish (helm-gtags-mode . " ")
  :config
  (setq helm-gtags-suggested-key-mapping t)
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-display-style 'detail
        helm-gtags-direct-helm-completing t))

;; -------------------------------------------------------------------
;; helm-ag
;; -------------------------------------------------------------------

(use-package helm-ag
  :after (helm)
  :bind ("M-s a" . helm-do-ag))

;; -------------------------------------------------------------------
;; helm-swoop
;; -------------------------------------------------------------------

(use-package helm-swoop
  :after (helm)
  :bind ("M-s s" . helm-swoop)
  :config
  (define-key isearch-mode-map (kbd "M-i") #'helm-swoop-from-isearch))

;; -------------------------------------------------------------------
;; helm-flyspell
;; -------------------------------------------------------------------

(use-package helm-flyspell
  :if (not noninteractive)
  :commands helm-flyspell-correct
  :config
  (bind-key "C-;" #'helm-flyspell-correct flyspell-mode-map))

;; -------------------------------------------------------------------
;; wgrep
;; -------------------------------------------------------------------

(use-package wgrep
  :config (setq wgrep-auto-save-buffer t))
(use-package wgrep-helm
  :after (helm))

;; -------------------------------------------------------------------
;; rainbow delimiter
;; -------------------------------------------------------------------

(use-package rainbow-delimiters
  :requires cl-lib
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

  (let ((me//paren-dual-colors '("salmon" "navajo white")))
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (set-face-foreground
      (intern (format "rainbow-delimiters-depth-%d-face" index))
      (elt me//paren-dual-colors
           (if (cl-evenp index) 0 1))))))

;; -------------------------------------------------------------------
;; beacon
;; -------------------------------------------------------------------

(use-package beacon
  :diminish beacon-mode
  :config
  (beacon-mode)
  (setq beacon-color 0.4))

;; -------------------------------------------------------------------
;; Drag
;; -------------------------------------------------------------------

(use-package golden-ratio-scroll-screen
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

;; -------------------------------------------------------------------
;; Drag
;; -------------------------------------------------------------------

(use-package drag-stuff
  :bind ("C-c d" . drag-stuff-mode)
  :diminish (drag-stuff-mode . " ")
  :config (drag-stuff-define-keys))

;; -------------------------------------------------------------------
;; deft
;; -------------------------------------------------------------------

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

;; -------------------------------------------------------------------
;; expand region
;; -------------------------------------------------------------------

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; -------------------------------------------------------------------
;; markdown-mode
;; -------------------------------------------------------------------

(use-package markdown-mode
  :defer t
  :config
  (add-hook 'markdown-mode-hook #'turn-on-auto-fill)
  (add-hook 'markdown-mode-hook #'turn-on-flyspell)
  (add-hook 'markdown-mode-hook #'pandoc-mode)

  (setq markdown-command "pandoc")
  (setq markdown-hide-urls t)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-asymmetric-header t)
  (setq markdown-unordered-list-item-prefix "- "
        markdown-list-indent-width 2)
  (setq markdown-use-pandoc-style-yaml-metadata t))

;; -------------------------------------------------------------------
;; smartparens
;; -------------------------------------------------------------------

(use-package smartparens-config
  :diminish smartparens-mode
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode)

  (setq sp-highlight-pair-overlay nil)
  (setq sp-show-pair-from-inside t)

  (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

  (define-key smartparens-mode-map (kbd "C-c s D") 'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "C-c s E") 'sp-up-sexp)
  (define-key smartparens-mode-map (kbd "C-c s a") 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "C-c s b") 'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd "C-c s d") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-c s e") 'sp-end-of-sexp)
  (define-key smartparens-mode-map (kbd "C-c s f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-c s k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-c s n") 'sp-next-sexp)
  (define-key smartparens-mode-map (kbd "C-c s p") 'sp-previous-sexp)
  (define-key smartparens-mode-map (kbd "C-c s r") 'sp-rewrap-sexp)
  (define-key smartparens-mode-map (kbd "C-c s s") 'sp-split-sexp)
  (define-key smartparens-mode-map (kbd "C-c s t") 'sp-transpose-sexp)
  (define-key smartparens-mode-map (kbd "C-c s w") 'sp-copy-sexp)
  (define-key smartparens-mode-map (kbd "C-c s <right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c s <left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-<right>") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-c s <delete>") 'sp-splice-sexp-killing-forward)
  (define-key smartparens-mode-map (kbd "C-c s <backspace>") 'sp-splice-sexp-killing-backward)
  (define-key smartparens-mode-map (kbd "C-c s C-<delete>") 'sp-splice-sexp-killing-around)

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

  (setq sp-cancel-autoskip-on-backward-movement nil)
  (setq sp-navigate-consider-stringlike-sexp
        '(lisp-mode emacs-lisp-mode latex-mode LaTeX-mode TeX-mode)))

;; -------------------------------------------------------------------
;; undo tree
;; -------------------------------------------------------------------

(use-package undo-tree
  :bind ("C-c u" . undo-tree-visualize)
  :diminish undo-tree-mode
  :config
  (setq undo-tree-enable-undo-in-region nil)
  (setq undo-tree-visualizer-diff t)
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist `(("." . ,me-emacs-tmp)))
  (setq undo-tree-auto-save-history t))

;; -------------------------------------------------------------------
;; which key
;; -------------------------------------------------------------------

(use-package which-key
  :config (which-key-mode)
  :diminish which-key-mode)

;; -------------------------------------------------------------------
;; ssh-config-mode
;; -------------------------------------------------------------------

(use-package ssh-config-mode
  :config
  (add-to-list 'auto-mode-alist '("/\\.ssh/config\\'" . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("/sshd?_config\\'" . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("/known_hosts\\'" . ssh-known-hosts-mode))
  (add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

;; -------------------------------------------------------------------
;; latex-mode
;; -------------------------------------------------------------------

(defun me//init-LaTeX ()
  "Initialize LaTeX mode."
  (TeX-fold-mode 1)
  (turn-on-auto-fill)
  (flyspell-mode)
  (whitespace-mode)
  (setq LaTeX-indent-level 1
        LaTeX-item-indent 0))
(add-hook 'LaTeX-mode-hook #'me//init-LaTeX)

;; -------------------------------------------------------------------
;; ibuffer
;; -------------------------------------------------------------------

(use-package ibuffer
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
                (name . "time-machine.org")))
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
                (name . "^\\*scratch\\*$"))))))

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
          (t (format "%8dB" (buffer-size)))))

  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 30 30 :left :elide) " "
                (size-h 9 -1 :right) " "
                (mode 24 24 :left :elide) " "
                filename-and-process))))

;; -------------------------------------------------------------------
;; Calendar
;; -------------------------------------------------------------------

(require 'calendar)

(setq diary-file (expand-file-name "diary" me-emacs-data))
(setq calendar-latitude 32.6)
(setq calendar-longitude -85.5)
(setq calendar-week-start-day 1)
(setq calendar-chinese-all-holidays-flag t)
(calendar-set-date-style 'iso)

(defface calendar-iso-week-face
  '((default :weight bold :foreground "pink"))
  "Face for calendar week number")

(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

;; Title for week number
(defface calendar-iso-week-header-face
  '((default :foreground "cyan1")) "Face for calendar week title")

(setq calendar-intermonth-header
      (propertize "Wk" 'font-lock-face 'calendar-iso-week-header-face))

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
          (holiday-lunar 9 9   "重陽節")
          ))

  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays
                holiday-other-holidays))

  (set-face-background 'cal-china-x-important-holiday-face "dark red")
  (set-face-background 'cal-china-x-general-holiday-face "forest green"))

;; -------------------------------------------------------------------
;; Appt
;; -------------------------------------------------------------------

(appt-activate 1)
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; -------------------------------------------------------------------
;; Font
;; -------------------------------------------------------------------

(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 135)

(set-fontset-font "fontset-default"
                  (cons (decode-char 'ucs #xF000)
                        (decode-char 'ucs #xF4E3))
                  (font-spec :family "Font Awesome 5 Free" :size 16))

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font) charset (font-spec
                                        :family "Sarasa Mono TC"
                                        :size 18)))

(set-face-attribute 'fixed-pitch nil :height 120)

;; -------------------------------------------------------------------
;; Key logger
;; -------------------------------------------------------------------

(open-dribble-file
 (expand-file-name
  (format-time-string "key-%FT%H%M%S.log") me-keylog))

;; -------------------------------------------------------------------
;; mail
;; -------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(require 'mail-conf)
(require 'bib-conf)
(require 'org-conf)

;; -------------------------------------------------------------------
;; C/C++
;; -------------------------------------------------------------------

(add-hook 'c-mode-common-hook #'google-set-c-style)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(use-package clang-format
  :bind (:map c++-mode-map
              ("C-M-q" . clang-format-region)))

;; -------------------------------------------------------------------
;; Javascript
;; -------------------------------------------------------------------

(use-package js2-mode
  :mode "\\.js\\'"
  :diminish (js2-mode . "JS2")
  :config
  (setq js2-basic-offset 2
        js2-include-node-externs t
        js2-include-browser-externs t))
(setq js-indent-level 2)

;; -------------------------------------------------------------------
;; web-mode
;; -------------------------------------------------------------------

(defun me//init-web-mode()
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t))

(use-package web-mode
  :mode "\\.\\(html\\|htm\\)\\'"
  :config
  (add-hook 'web-mode-hook #'me//init-web-mode))

;; -------------------------------------------------------------------
;; Python
;; -------------------------------------------------------------------

(use-package python
  :config
  (defun me//init-python()
    (local-set-key (kbd "M-<left>") #'decrease-left-margin)
    (local-set-key (kbd "M-<right>") #'increase-left-margin)
    (python-docstring-mode)
    (diminish 'python-docstring-mode)
    (set (make-local-variable 'comment-inline-offset) 2)
    (setq fill-column 78)
    (setq python-check-command "flake8"))
  (add-hook 'python-mode-hook #'me//init-python)

  (python-skeleton-define args
      "Insert argpass template for Python script"
    ""
    "def parse_args():\n"
    "    parser = argparse.ArgumentParser(description='TODO')\n"
    "    parser.add_argument('--n', metavar='N', type=int)\n"
    "    parser.add_argument('--varlist', metavar='N1 [N2 N3 ...]', nargs='+')\n"
    "    parser.add_argument('--fixlist', metavar='A B C', nargs='3')\n"
    "\n"
    "    mut = parser.add_mutually_exclusive_group(required=True)\n"
    "    mut.add_argument('--yes', dest='yes', action='store_true', help='TODO')\n"
    "    mut.add_argument('--no', dest='yes', action='store_false', help='TODO')\n"
    "    parser.set_defaults(mut=False)\n"
    "\n"
    "    return parser.parse_args()\n"
    "\n\n"
    "def main(args):\n"
    "    pass\n"
    "\n\n"
    "if __name__ == '__main__':\n"
    "    info('THE BEGIN')\n"
    "    main(parse_args())\n"
    "    info('THE END')\n")

  (python-skeleton-define logging
      "Insert logging template code."
    ""
    "logging.basicConfig(format='%(asctime)-15s %(message)s', level=logging.INFO)\n"
    "logger = logging.getLogger(__name__)\n"
    "info = logger.info\n")

  (python-skeleton-define matplotlib
      "Insert matplotlib template code."
    ""
    "import matplotlib\n"
    "matplotlib.use('Agg')           # noqa: E402\n"
    "import matplotlib.pyplot as plt\n"
    "import matplotlib.gridspec as gridspec\n"
    "\n"
    "fig = plt.figure(figsize=(w, h))\n"
    "gs = gridspec.GridSpec(row, col, width_ratios=0.5, wspace=0.01, hspace=0.01)\n"
    "\n"
    "ax = fig.add_subplot(gs[0, 0])\n"
    "\n"
    "gs.tight_layout(fig)\n"
    "plt.savefig('name.png')\n")

  (define-key python-mode-map (kbd "C-c C-t a") #'python-skeleton-args)
  (define-key python-mode-map (kbd "C-c C-t l") #'python-skeleton-logging)
  (define-key python-mode-map (kbd "C-c C-t m") #'python-skeleton-matplotlib))

;; -------------------------------------------------------------------
;; shell script
;; -------------------------------------------------------------------

(defun me//init-sh()
  (local-set-key (kbd "M-<left>") #'decrease-left-margin)
  (local-set-key (kbd "M-<right>") #'increase-left-margin))
(add-hook 'sh-mode-hook #'me//init-sh)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; -------------------------------------------------------------------
;; Lua
;; -------------------------------------------------------------------

(defun me/lua-send-current-line-and-next()
  (lua-send-current-line)
  (next-line))

(defun me//init-lua()
  (local-set-key (kbd "M-<left>") #'decrease-left-margin)
  (local-set-key (kbd "M-<right>") #'increase-left-margin)
  (setq standard-indent 3)
  (setq tab-stop-list (number-sequence 3 120 3)))

(use-package lua-mode
  :mode "\\.lua\\'"
  :config
  (add-hook 'lua-mode-hook #'me//init-lua)
  (define-key lua-mode-map (kbd "C-<return>")
    #'me/lua-send-current-line-and-next)
  (define-key lua-mode-map (kbd "C-c b")   #'lua-send-buffer)
  (define-key lua-mode-map (kbd "C-c C-b") #'lua-send-buffer)
  (define-key lua-mode-map (kbd "C-c f")   #'lua-send-defun)
  (define-key lua-mode-map (kbd "C-c C-f") #'lua-send-defun)
  (define-key lua-mode-map (kbd "C-c r")   #'lua-send-region)
  (define-key lua-mode-map (kbd "C-c C-r") #'lua-send-region))

;; -------------------------------------------------------------------
;; R
;; -------------------------------------------------------------------

(use-package ess-site
  :mode "\\.R\\'"
  :config
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers  . t)
          (ess-R-fl-keyword:fun-defs   . t)
          (ess-R-fl-keyword:keywords   . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants  . t)
          (ess-fl-keyword:fun-calls)
          (ess-fl-keyword:numbers)
          (ess-fl-keyword:operators)
          (ess-fl-keyword:delimiters)
          (ess-fl-keyword:=)
          (ess-R-fl-keyword:F&T        . t)
          (ess-R-fl-keyword:%op%       . t)))

  (setq inferior-R-font-lock-keywords
        '((ess-S-fl-keyword:prompt      . t)
          (ess-R-fl-keyword:messages    . t)
          (ess-R-fl-keyword:modifiers   . t)
          (ess-R-fl-keyword:fun-defs    . t)
          (ess-R-fl-keyword:keywords    . t)
          (ess-R-fl-keyword:assign-ops  . t)
          (ess-R-fl-keyword:constants   . t)
          (ess-fl-keyword:matrix-labels . t)
          (ess-fl-keyword:fun-calls)
          (ess-fl-keyword:numbers)
          (ess-fl-keyword:operators)
          (ess-fl-keyword:delimiters)
          (ess-fl-keyword:=)
          (ess-R-fl-keyword:F&T         . t)))

  (defun me//init-ess ()
    "Init my ess mode."
    (setq ess-help-own-frame 'one)
    (setq ess-tab-complete-in-script t)
    (setq ess-first-tab-never-complete 'symbol-or-paren-or-punct))

  (add-hook 'ess-mode-hook #'me//init-ess)
  (add-hook 'inferior-ess-mode-hook #'turn-on-smartparens-mode))

;; -------------------------------------------------------------------
;; json
;; -------------------------------------------------------------------

(setq json-reformat:indent-width 2)

;; -------------------------------------------------------------------
;; calendar in emacs
;; -------------------------------------------------------------------

(if (file-exists-p me-local-conf)
    (load me-local-conf))

(require 'server)
(unless (server-running-p) (server-start))

;;; init.el ends here
