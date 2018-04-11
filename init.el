;;; init.el
;;; Time-stamp: <2018-04-11 14:09:28 gongzhitaao>

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; -------------------------------------------------------------------
;; Key binding
;; -------------------------------------------------------------------

(global-set-key (kbd "C-z") #'delete-other-windows)

;; FN keys
;; --------------------------------------------------------------------

(global-set-key (kbd "<f5>") #'bookmark-bmenu-list)
(global-set-key (kbd "<f6>") #'calendar)
;; f8 -- deft
;; f10 -- menu
(global-set-key (kbd "<f11>") #'ispell)
(global-set-key (kbd "<f12>") #'gnus-other-frame)

;; Remaping
;; --------------------------------------------------------------------

(global-set-key [remap execute-extended-command] #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key [remap isearch-backward] #'isearch-backward-regexp)
(global-set-key [remap isearch-forward]  #'isearch-forward-regexp)
(global-set-key [remap list-buffers]     #'ibuffer)
(global-set-key [remap switch-to-buffer] #'helm-mini)
(global-set-key [remap yank-pop]         #'helm-show-kill-ring)
(global-set-key (kbd "C-x m")            #'compose-mail-other-frame)

;; C-c key
;; --------------------------------------------------------------------

;; C-c b -- helm-bibtex
;; C-c d -- drag-stuff-mode
;; C-c f -- file related commands
;; C-c f h magit-log-buffer-file
(global-set-key (kbd "C-c f s") #'me//sudo-edit)
;; C-c g -- magit-status
;; C-c m -- multiple-cursor
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c o s") #'me//org-sort-orgref-citation-list-by-year)
(global-set-key (kbd "C-c r c") #'me//org-ref-open-citation)
(global-set-key (kbd "C-c r n") #'me//org-ref-open-note)
(global-set-key (kbd "C-c r p") #'me//org-ref-open-pdf)
;; C-c s -- smartparens
;; C-c u -- undo-tree
(global-set-key (kbd "C-c =") #'align-regexp)

(global-set-key (kbd "C-c C-q") #'bury-buffer)

(global-set-key (kbd "s-;") #'comment-or-uncomment-region)

;; M-s search
;; --------------------------------------------------------------------

;; M-s a helm-do-ag
(global-set-key (kbd "M-s g") #'helm-do-grep-ag)
;; M-s h highlight-xxx
(global-set-key (kbd "M-s q") #'vr/query-replace)
;; M-s s helm-swoop

;; Meta
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

;; special key
;; -------------------------------------------------------------------

(global-set-key (kbd "<backtab>") #'decrease-left-margin)
(global-set-key (kbd "<escape>") #'view-mode)

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
  "Operate on line or region."
  (if (region-active-p)
      args
    (let ((bol (+ (line-beginning-position) (current-indentation)))
          (eol (line-end-position)))
      (push-mark bol)
      (goto-char eol)
      (list bol eol (nth 2 args)))))

(defun me-with-region-or-line (func &optional remove)
  "If not REMOVE, add advice to FUNC, i.e., when called with no
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

(defun me//sudo-edit (&optional arg)
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
(if (file-exists-p me-local-conf)
    (load me-local-conf))

(add-hook 'focus-out-hook #'garbage-collect)

;; -------------------------------------------------------------------
;; backup
;; -------------------------------------------------------------------

(setq backup-directory-alist `((".*" . ,me-emacs-tmp)))
(setq auto-save-list-file-prefix
      (expand-file-name ".saves-" me-emacs-tmp))

(setq backup-by-copying    t
      delete-old-versions  t
      kept-new-versions    6
      kept-old-versions    2
      version-control      t)

(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (message "Deleting old backup files...")
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth
                                          (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

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
(add-to-list 'recentf-exclude me-emacs-data)
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
(add-hook 'text-mode-hook 'whitespace-mode)

;; -------------------------------------------------------------------
;; view mode
;; -------------------------------------------------------------------

(require 'view)

(defun me//view-mode-indicator ()
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
  :config (projectile-global-mode 1)
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

(defun me//diminish-flyspell()
  (diminish 'flyspell-mode))
(add-hook 'flyspell-mode-hook #'me//diminish-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(defun me//diminish-view ()
  (diminish 'view-mode))
(add-hook 'view-mode-hook #'me//diminish-view)

(defun me//diminish-auto-revert ()
  (diminish 'auto-revert-mode " "))
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
  :bind (("C-c g" . magit-status)
         ("C-c f h" . magit-log-buffer-file))
  :config
  (setq git-commit-major-mode 'org-mode))

;; -------------------------------------------------------------------
;; multiple cursors
;; -------------------------------------------------------------------

(use-package multiple-cursors
  :bind (("C-c m C-a" . mc/edit-beginnings-of-lines)
         ("C-c m C-e" . mc/edit-ends-of-lines)
         ("C-c m a"   . mc/mark-all-like-this-dwin)
         ("C-c m l" . mc/edit-lines)
         ("C-c m m" . mc/mc/mark-more-like-this-extended)
         ("C-c m r" . vr/mc-mark)))

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
        '(text-mode prog-mode dired-mode conf-mode
                    ein:notebook-multilang-mode Info-mode calendar-mode))
  (setq writeroom-mode-line t)
  (delete 'writeroom-set-menu-bar-lines writeroom-global-effects)
  (global-writeroom-mode))

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
  (add-hook 'prog-mode-hook #'fci-mode)
  (add-hook 'text-mode-hook #'fci-mode))

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
  (global-undo-tree-mode 1)
  (setq undo-tree-history-directory-alist `(("." . ,me-emacs-tmp)))
  (setq undo-tree-auto-save-history t))

;; -------------------------------------------------------------------
;; which key
;; -------------------------------------------------------------------

(use-package which-key
  :config (which-key-mode)
  :diminish which-key-mode)

;; -------------------------------------------------------------------
;; PDF tools
;; -------------------------------------------------------------------

(pdf-tools-install)

(defun me//pdf-view-next-few-lines ()
  (interactive)
  (pdf-view-next-line-or-next-page 10))

(defun me//pdf-view-prev-few-lines ()
  (interactive)
  (pdf-view-previous-line-or-previous-page 10))

;; copied directly from view-window-size
(defun me//window-size ()
  ;; Return the height of the current window, excluding the mode line.  Using
  ;; `window-line-height' accounts for variable-height fonts.
  (let ((h (window-line-height -1)))
    (if h
        (1+ (nth 1 h))
      ;; This should not happen, but if `window-line-height' returns
      ;; nil, fall back on `window-height'.
      (1- (window-height)))))

(defun me//pdf-view-scroll-half-forward ()
  (interactive)
  (pdf-view-next-line-or-next-page (/ (me//window-size) 2)))

(defun me//pdf-view-scroll-half-backward ()
  (interactive)
  (pdf-view-previous-line-or-previous-page (/ (me//window-size) 2)))

(bind-key (kbd "<delete>") #'pdf-view-scroll-up-or-next-page pdf-view-mode-map)
(bind-key (kbd "<down>") #'me//pdf-view-next-few-lines pdf-view-mode-map)
(bind-key (kbd "<up>") #'me//pdf-view-prev-few-lines pdf-view-mode-map)
(bind-key (kbd "b") #'helm-mini pdf-view-mode-map)
(bind-key (kbd "c") #'me//org-ref-open-citation pdf-view-mode-map)
(bind-key (kbd "d") #'me//pdf-view-scroll-half-forward pdf-view-mode-map)
(bind-key (kbd "e") #'me//pdf-view-scroll-half-backward pdf-view-mode-map)
(bind-key (kbd "j") #'me//pdf-view-scroll-half-forward pdf-view-mode-map)
(bind-key (kbd "n") #'me//org-ref-open-note pdf-view-mode-map)
(bind-key (kbd "g") #'pdf-view-goto-page pdf-view-mode-map)
(bind-key (kbd "k") #'me//pdf-view-scroll-half-backward pdf-view-mode-map)
(bind-key (kbd "z") #'delete-other-windows pdf-view-mode-map)

(bind-key (kbd "<right>") #'pdf-view-next-page-command pdf-view-mode-map)
(bind-key (kbd "<left>") #'pdf-view-previous-page-command pdf-view-mode-map)

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
  (TeX-fold-mode 1)
  (turn-on-auto-fill)
  (flyspell-mode)
  (whitespace-mode)
  (setq LaTeX-indent-level 1
        LaTeX-item-indent 0))
(add-hook 'LaTeX-mode-hook #'me//init-LaTeX)

;; -------------------------------------------------------------------
;; BBDB
;; -------------------------------------------------------------------

(use-package bbdb
  :config
  (bbdb-initialize 'gnus 'mail 'message 'anniv 'sc)

  (setq bbdb-complete-mail-allow-cycling t
        bbdb-allow-duplicates t
        bbdb-message-all-addresses t
        bbdb-file (expand-file-name "contacts.bbdb.gz" me-emacs-data))

  (add-hook 'message-setup-hook 'bbdb-mail-aliases))

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
            (or (mode . message-mode)))
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
                        (decode-char 'ucs #xF295))
                  (font-spec :family "FontAwesome" :size 16))

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font) charset (font-spec
                                        :family "Inziu Iosevka TC"
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

(require 'gnus)
(setq gnus-init-file (expand-file-name "gnus-conf.el" me-emacs))

(defun me//seconds-from-now (interval &optional wait)
  (let* ((m (mod (string-to-int (format-time-string "%M")) interval))
         (s (string-to-int (format-time-string "%S")))
         (elapsed (+ (* m 60) s))
         (w (or wait 30)))
    (if (< elapsed (- w 15))
        w
      (- (+ (* interval 60) w) elapsed))))

(defun me//start-gnus-bg ()
  (gnus)
  (switch-to-prev-buffer))
(run-at-time (me//seconds-from-now 5) nil #'me//start-gnus-bg)

;; -------------------------------------------------------------------
;; BibTeX
;; -------------------------------------------------------------------

(defvar me-bib (expand-file-name ".local/data/bibliography" me-home)
  "My bibliography collection path.")
(defvar me-bib-files
  `(,(expand-file-name "nn.bib" me-bib))
  "My bibliography files.")
(defvar me-bib-pdfs
  `(,(expand-file-name "nn-pdf" me-bib))
  "Paths containing my PDFs of the bibliography.")
(defvar me-bib-notes
  (expand-file-name "notes" me-bib)
  "Path to store my notes on each papers.")

(use-package helm-bibtex
  :bind ("C-c b" . helm-bibtex))

(use-package bibtex
  :config
  (defun me//init-bibtex ()
    (local-set-key [remap fill-paragraph] #'bibtex-fill-entry)
    (local-set-key [remap bibtex-clean-entry]
                   #'org-ref-clean-bibtex-entry)
    (local-set-key (kbd "C-c C-v") #'bibtex-validate)
    (local-set-key (kbd "C-c r p") #'org-ref-open-bibtex-pdf)
    (setq fill-column 140))
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
  (setq bibtex-completion-pdf-symbol ""
        bibtex-completion-notes-symbol ""))

;; -------------------------------------------------------------------
;; reftex
;; -------------------------------------------------------------------

(use-package reftex
  :diminish reftex-mode
  :config
  (add-hook 'tex-mode-hook #'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t
        reftex-ref-style-default-list '("Cleveref"
                                        "Hyperref"
                                        "Fancyref")
        reftex-default-bibliography me-bib-files))

;; -------------------------------------------------------------------
;; org-ref
;; -------------------------------------------------------------------

(use-package org-ref
  :init
  (setq org-ref-default-bibliography me-bib-files
        org-ref-pdf-directory me-bib-pdfs
        org-ref-notes-directory me-bib-notes)

  (setq org-ref-ref-color "goldenrod")
  (setq org-ref-cite-color "dark sea green")

  :config
  (defun me//org-ref-notes-function (thekey)
    (bibtex-completion-edit-notes
     (list (car (org-ref-get-bibtex-key-and-file thekey)))))
  (setq org-ref-notes-function #'me//org-ref-notes-function)
  (add-hook 'org-ref-clean-bibtex-entry-hook
            #'org-ref-downcase-bibtex-entry))

;; The following three functions jump among PDF, bibtex entry and note.  For
;; instance, me//org-ref-open-pdf opens the PDF file when your cursor is at the
;; bibtex entry or the note that is associated with this bibtex entry.

(defun me//org-ref-open-pdf ()
  (interactive)
  (if (string= (file-name-extension (buffer-file-name)) "pdf")
      nil
    (let* ((key
            (condition-case nil
                (org-ref-get-bibtex-key-under-cursor)
              ('error (file-name-base (buffer-file-name)))))
           (pdf-file (funcall org-ref-get-pdf-filename-function key)))
      (if (file-exists-p pdf-file)
          (org-open-file pdf-file)
        (message "no pdf found for %s" key)))))

(defun me//org-ref-open-citation ()
  "Open bibtex file to key with which the note associated."
  (interactive)
  (if (string= (file-name-extension (buffer-file-name)) "bib")
      nil
    (let* ((key
            (condition-case nil
                (org-ref-get-bibtex-key-under-cursor)
              ('error (file-name-base (buffer-file-name)))))
           (results (org-ref-get-bibtex-key-and-file key))
           (bibfile (cdr results)))
      (find-file bibfile)
      (bibtex-search-entry key))))

(defun me//org-ref-open-note ()
  (interactive)
  (let* ((key
         (condition-case nil
             (org-ref-get-bibtex-key-under-cursor)
           ('error (if (string= (file-name-extension (buffer-file-name)) "bib")
                       (save-excursion
                         (bibtex-beginning-of-entry)
                         (reftex-get-bib-field "=key=" (bibtex-parse-entry t)))
                     (file-name-base (buffer-file-name))))))
         (pdf-file (funcall org-ref-get-pdf-filename-function key)))
    (if (file-exists-p pdf-file)
        (org-ref-open-notes-at-point key)
      (message "no pdf found for %s" key))))

;; -------------------------------------------------------------------
;; Orgmode
;; -------------------------------------------------------------------

(use-package ox-beamer)
(use-package ox-bibtex)
(use-package ox-extra)
(use-package ox-gfm)

(defun me//getkey-orgref ()
  "Get the year part of org-ref citation."
  (save-excursion
    (if (re-search-forward org-ref-cite-re nil t)
        (let* ((bibkey (match-string 0))
               (YYYY-re "\\([0-9]\\{4\\}\\)"))
          (string-match YYYY-re bibkey)
          (match-string 0 bibkey))
      "")))

(defun me//org-sort-orgref-citation-list-by-year
    (&optional with-case sorting-type)
  "Sort the list of citations by year.

The list looks like:
- [X] cite:someone2017 dummy
- [ ] cite:others2013 dummy
- [ ] cite:hello2018 dummy

I want to sort the list by year instead of by
author (alphabetically)."
  (interactive)
  (org-sort-list with-case ?F #'me//getkey-orgref))

(use-package org
  :init
  (setq org-modules '(org-bbdb org-bibtex org-clock org-gnus
                               org-habit org-table))
  (setq org-export-backends '(ascii beamer html latex md))

  :config
  (setq org-directory me-emacs-data)

  (defun me//init-org ()
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

  (defun me//init-org-agenda ()
    (define-key org-agenda-mode-map (kbd "F") #'org-gcal-fetch))

  (add-hook 'org-mode-hook #'me//init-org)
  (add-hook 'org-agenda-mode-hook #'me//init-org-agenda)

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

  ;; Show events from diary
  (setq org-agenda-include-diary t)

  (setq org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)

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

  ;; Files to be included in Agenda view.
  (setq org-agenda-files
        (expand-file-name "orgfile" org-directory))

  (setq org-agenda-dim-blocked-tasks            t
        org-agenda-compact-blocks               nil
        org-agenda-repeating-timestamp-show-all t
        org-agenda-show-all-dates               t)

  (setq org-time-stamp-custom-formats
        '("<%m/%d/%y %a>" . "<%Y-%m-%d %a %R %z>"))

  (setq org-agenda-prefix-format
        '((agenda   . " %i %-12:c%?-12t% s")
          (timeline . "  % s")
          (todo     . " %i %-12:T")
          (tags     . " %i %-12:T")
          (search   . " %i %-12:T")))

  (setq org-agenda-tags-column         -100
        org-agenda-start-with-log-mode t)
  (setq org-habit-graph-column         50
        org-habit-preceding-days       28
        org-habit-following-days       1)

  (setq org-clock-history-length 32
        org-clock-in-resume t)
  (setq org-log-into-drawer   t
        org-clock-into-drawer t)

  (setq org-clock-persist t)
  (org-clock-persistence-insinuate)

  (setq org-use-fast-tag-selection 'auto)

  (setq org-capture-templates
        `(("m" "Save mail link" entry
           (file "todo.org")
           (file "capture/mail.org")
           :empty-lines 1
           :jump-to-captured t)

          ("r" "Insert related work reference")
          ("ra" "Adversarial attack/defense" checkitem
           (file+headline "notes/bibliography/related/adversarial.org" "Recent Work"))
          ("rc" "Compression and Quantization" checkitem
           (file+headline "notes/bibliography/related/compression.org" "Recent Work"))
          ("re" "Evolution" checkitem
           (file+headline "notes/bibliography/related/evolution.org" "Recent Work"))
          ("rl" "Reinforcement Learning" checkitem
           (file+headline "notes/bibliography/related/rl.org" "Recent Work"))
          ("rn" "Natural Language Processing" checkitem
           (file+headline "notes/bibliography/related/nlp.org" "Recent Work"))
          ("rr" "Randomness" checkitem
           (file+headline "notes/bibliography/related/random.org" "Recent Work"))
          ("rt" "Theory" checkitem
           (file+headline "notes/bibliography/related/theory.org" "Recent Work"))
          ("ru" "Miscellaneous" checkitem
           (file+headline "notes/bibliography/related/misc.org" "Recent Work"))

          ("t" "New TODO" entry
           (file "todo.org")
           (file "capture/todo.org")
           :empty-lines 1
           :jump-to-captured t)

          ("w" "New weekly summary" plain
           (file+olp+datetree "time-machine.org")
           "%?"
           :empty-lines 1
           :jump-to-captured t
           :tree-type week)))

  (setq org-latex-prefer-user-labels t)

  (setq org-latex-pdf-process
        (quote ("PDFLATEX=%latex texi2dvi --shell-escape --pdf --clean --verbose --batch %f")))

  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("activate={true,nocompatibility},final,tracking=true,kerning=true,spacing=basictext,factor=1100,stretch=10,shrink=10" "microtype"))
  (add-to-list 'org-latex-packages-alist '("" "geometry"))

  (add-to-list 'org-latex-classes
               '("ctexart"
                 "\\documentclass[11pt]{ctexart} [NO-DEFAULT-PACKAGES] [NO-PACKAGES] [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (ox-extras-activate '(ignore-headlines))
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
      urlcolor=MidnightBlue}\n")

  (add-to-list 'org-beamer-environments-extra
               '("only" "O" "\\only%a{" "}"))
  (add-to-list 'org-beamer-environments-extra
               '("action" "A" "\\action%a{" "}"))

  (setq org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil)

  (setq org-html-allow-name-attribute-in-anchors t)

  (setq org-html-htmlize-output-type 'css)

  ;; Postamble.
  (setq org-html-postamble t
        org-html-postamble-format
        '(("en" "<a class=\"author\"
           href=\"http://gongzhitaao.org\">%a</a> / <span
           class=\"date\">%T</span><span class=\"creator\">%c</span>")))

  (load-file (expand-file-name "my-org-misc.el" org-directory)))

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

(defun me//lua-send-current-line-and-next()
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
    #'me//lua-send-current-line-and-next)
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

(use-package org-gcal
  :config
  (setq org-gcal-client-id "1061693727479-1tf1621pclk4b31gunogg1psdbn3t5r1.apps.googleusercontent.com"
        org-gcal-client-secret "v8lcgsDYuCw5klOJMpO5o2eL"
        org-gcal-file-alist '(("zhitaao.gong@gmail.com" .  "~/Dropbox/dotfiles/emacs.d/data/gcal.org"))))

(require 'server)
(unless (server-running-p) (server-start))

;;; init.el ends here
