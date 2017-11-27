;;; init.el
;;; Time-stamp: <2017-11-27 10:19:27 gongzhitaao>

;; -------------------------------------------------------------------
;; Key binding
;; -------------------------------------------------------------------

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(global-set-key (kbd "C-z") #'delete-other-windows)

;; FN keys
;; --------------------------------------------------------------------

;; f3 -- sr-speedbar-toggle
(global-set-key (kbd "<f6>") #'calendar)
(global-set-key (kbd "<f7>") #'compile)
;; f8 -- deft
;; f10 -- menu
(global-set-key (kbd "<f11>") #'ispell)
;; f12 -- gnus-other-frame

;; Remaping
;; --------------------------------------------------------------------

(global-set-key [remap execute-extended-command] #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
;; (global-set-key [remap goto-line]        #'me-goto-line-with-linum)
(global-set-key [remap isearch-backward] #'isearch-backward-regexp)
(global-set-key [remap isearch-forward]  #'isearch-forward-regexp)
(global-set-key [remap list-buffers]     #'ibuffer)
(global-set-key [remap switch-to-buffer] #'helm-mini)
(global-set-key [remap yank-pop]         #'helm-show-kill-ring)

;; C-c key
;; --------------------------------------------------------------------

;; C-c b -- helm-bibtex
(global-set-key (kbd "C-c c") #'color-identifiers-mode)
;; C-c d -- drag-stuff-mode
(global-set-key (kbd "C-c e") #'me//sudo-edit)
(global-set-key (kbd "C-c j") #'ace-jump-mode)
;; C-c f -- frame operation
;; C-c g -- magit-status
;; C-c m --  multiple-cursor
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c r c") #'org-ref-open-citation-at-point)
(global-set-key (kbd "C-c r C") #'me//org-ref-jump-to-citation-from-note)
(global-set-key (kbd "C-c r p") #'org-ref-open-pdf-at-point)
(global-set-key (kbd "C-c r P") #'me//org-ref-jump-to-pdf-from-note)
(global-set-key (kbd "C-c r n") #'org-ref-open-notes-at-point)
;; C-c s -- smartparens
;; C-c u -- undo-tree
;; C-c w -- writeroom-mode
(global-set-key (kbd "C-c (") #'rainbow-delimiters-mode)
(global-set-key (kbd "C-c =") #'align-regexp)

(global-set-key (kbd "C-c C-q") #'bury-buffer)

;; M-key
;; --------------------------------------------------------------------

;; M-s search key
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
  "Operate on line or region."
  (if (region-active-p)
      args
    (let ((bol (+ (line-beginning-position) (current-indentation)))
          (eol (1- (line-beginning-position 2))))
      (push-mark bol)
      (goto-char eol)
      (list bol eol (nth 2 args)))))

(defun me--org-show-context-advice (&rest ignore)
  (org-show-context 'default))

(defun me-with-region-or-line (func &optional remove)
  "If not REMOVE, add advice to FUNC, i.e., when called with no
active region, call FUNC on current line.  Otherwise remove
advice."
  (if remove
      (advice-remove func #'me--ad-with-region-or-line)
    (advice-add func :filter-args #'me--ad-with-region-or-line)))

(defun me-goto-line-with-linum ()
  "Show line numbers temporarily, while prompting for the line
number input"
  (interactive)
  (unwind-protect
      (progn
        (nlinum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (nlinum-mode -1)))

(defun me-clear-shell ()
  "Clear shell window."
   (interactive)
   (let ((old-max comint-buffer-maximum-size))
     (setq comint-buffer-maximum-size 0)
     (comint-truncate-buffer)
     (setq comint-buffer-maximum-size old-max)))

(me-with-region-or-line #'kill-ring-save)

(if (file-exists-p me-local-conf)
    (load me-local-conf))

;; -------------------------------------------------------------------
;; Misc
;; -------------------------------------------------------------------

(setq display-time-24hr-format t
      display-time-day-and-date nil)
(display-time)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default tab-stop-list (number-sequence 2 120 2))
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

(global-visual-line-mode)

(setq-default fill-column 80)

(setq custom-file (expand-file-name "emacs-custom.el" me-emacs))
(load custom-file)

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

(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-listing-switches "-alh")

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
                              empty indentation space-after-tab space-before-tab))

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'text-mode-hook 'whitespace-mode)

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
;; (me//init)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; -------------------------------------------------------------------
;; Theme first
;; -------------------------------------------------------------------

(add-to-list 'default-frame-alist '(background-color . "gray20"))
(add-to-list 'default-frame-alist '(foreground-color . "gray90"))

(global-hl-line-mode)
(set-face-background 'hl-line "gray10")

;; -------------------------------------------------------------------
;; exec-path
;; -------------------------------------------------------------------

(use-package exec-path
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

  (setq helm-buffer-max-length nil)

  ;; rebind tab to run persistent action
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; make TAB works in terminal
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  ;; list actions using C-z
  (define-key helm-map (kbd "C-z")  #'helm-select-action)

  (use-package helm-files))

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
(diminish 'color-identifiers-mode)

(defun me//init-read-only-mode ()
  (diminish 'read-only-mode " "))
(add-hook 'read-only-mode-hook #'me//init-read-only-mode)

(setq view-read-only t)
(defun me//init-view-mode ()
  (diminish 'view-mode " "))
(add-hook 'view-mode-hook #'me//init-view-mode)

(defun me//init-flyspell-mode()
  (diminish 'flyspell-mode))
(add-hook 'flyspell-mode-hook #'me//init-flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(defun me//init-auto-revert-mode ()
  (diminish 'auto-revert-mode " "))
(add-hook 'auto-revert-mode-hook #'me//init-auto-revert-mode)

;; -------------------------------------------------------------------
;; dired+
;; -------------------------------------------------------------------

(use-package dired+)
(use-package mouse3)
(use-package bookmark+)

(use-package ls-lisp
  :config
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-insert-directory-program nil))

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
  :defines my-multiple-cursors-map
  :bind-keymap ("C-c m" . my-multiple-cursors-map)
  :config
  (defvar my-multiple-cursors-map
    (let ((map (make-sparse-keymap)))

      (define-key map (kbd "C-a") #'mc/edit-beginnings-of-lines)
      (define-key map (kbd "C-e") #'mc/edit-ends-of-lines)

      (define-key map (kbd "a")   #'mc/mark-all-like-this-dwim)
      (define-key map (kbd "l")   #'mc/edit-lines)
      (define-key map (kbd "m")   #'mc/mark-more-like-this-extended)
      (define-key map (kbd "r")   #'vr/mc-mark)

      map)))

;; -------------------------------------------------------------------
;; tramp
;; -------------------------------------------------------------------

(use-package tramp
  :config
  (setq tramp-default-method "ssh"
        tramp-persistency-file-name (expand-file-name "tramp"
                                                      me-emacs-tmp)))

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

;; -------------------------------------------------------------------
;; writeroom
;; -------------------------------------------------------------------

(use-package writeroom-mode
  :bind ("C-c w" . writeroom-mode)
  :config
  (setq writeroom-fullscreen-effect nil)
  (setq writeroom-maximize-window nil)
  (setq writeroom-width (+ fill-column 10))
  (setq writeroom-major-modes
        '(text-mode prog-mode dired-mode conf-mode
                    ein:notebook-multilang-mode))
  (setq writeroom-mode-line t)
  (delete 'writeroom-set-menu-bar-lines writeroom-global-effects))

(global-writeroom-mode)

;; -------------------------------------------------------------------
;; highlight-indent-guides
;; -------------------------------------------------------------------

(use-package highlight-indent-guides)
(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)

;; Whitespace-mode need to be called before highlight-indent-guides, otherwise
;; no guides are shown.
(add-hook 'prog-mode-hook #'whitespace-mode)

;; -------------------------------------------------------------------
;; transpose-frame
;; -------------------------------------------------------------------

(use-package transpose-frame
  :defines my-transpose-frame-map
  :bind-keymap ("C-c f" . my-transpose-frame-map)
  :config
  (defvar my-transpose-frame-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "a") #'rotate-frame-anticlockwise)
      (define-key map (kbd "c") #'rotate-frame-clockwise)
      (define-key map (kbd "r") #'rotate-frame)

      (define-key map (kbd "t") #'transpose-frame)

      (define-key map (kbd "x") #'flip-frame)
      (define-key map (kbd "y") #'flop-frame)

      map)))

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
;; anzu
;; -------------------------------------------------------------------

(use-package anzu
  :config (global-anzu-mode +1)
  :diminish anzu-mode)

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
  :bind ("M-s a" . helm-do-ag))

;; -------------------------------------------------------------------
;; helm-swoop
;; -------------------------------------------------------------------

(use-package helm-swoop
  :bind ("M-s s" . helm-swoop))

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
(use-package wgrep-helm)

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
;; sr speedbar
;; -------------------------------------------------------------------

(use-package sr-speedbar
  :bind ("<f3>" . sr-speedbar-toggle)
  :config
  (setq sr-speedbar-right-side nil)
  (setq speedbar-use-images nil))

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
    (sp-local-tag "i" "\"[" "\"]"))

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
  :config (global-undo-tree-mode 1))

;; -------------------------------------------------------------------
;; which key
;; -------------------------------------------------------------------

(use-package which-key
  :config (which-key-mode)
  :diminish which-key-mode)

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

(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator ""
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "_"
      bibtex-autokey-titlewords 1
      bibtex-autokey-titlewords-stretch 0
      bibtex-autokey-titleword-length nil)

(setq bibtex-maintain-sorted-entries t)

;; -------------------------------------------------------------------
;; helm-bibtex
;; -------------------------------------------------------------------

(define-key helm-command-map (kbd "b") #'helm-bibtex)

(use-package helm-bibtex
  :bind ("C-c b" . helm-bibtex)
  :config
  (setq bibtex-completion-bibliography me-bib-files
        bibtex-completion-library-path me-bib-pdfs
        bibtex-completion-notes-path me-bib-notes)

  (setq bibtex-completion-notes-extension ".org")
  ;; (setq bibtex-completion-pdf-open-function
  ;;       #'helm-open-file-with-default-tool)

  (setq bibtex-completion-pdf-symbol ""
        bibtex-completion-notes-symbol ""))

(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq bibtex-dialect 'biblatex)
(setq bibtex-align-at-equal-sign t)
(setq bibtex-text-indentation 20)

(defun me//init-bibtex ()
  (local-set-key [remap fill-paragraph] #'bibtex-fill-entry)
  (local-set-key [remap bibtex-clean-entry]
                 #'org-ref-clean-bibtex-entry)
  (local-set-key (kbd "C-c C-v") #'bibtex-validate)
  (setq fill-column 140))
(add-hook 'bibtex-mode-hook #'me//init-bibtex)

(require 'bibtex)
(add-to-list 'bibtex-entry-format 'unify-case)
(add-to-list 'bibtex-entry-format 'sort-fields)

;; use pdf-tools to open pdf in Emacs
(pdf-tools-install)

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
;; BBDB
;; -------------------------------------------------------------------

(use-package bbdb
  :config
  (bbdb-initialize 'gnus 'mail 'message 'anniv)

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
                (filename . ,(expand-file-name "org" me-emacs-data))
                (name . "\\.bbdb")
                (mode . bbdb-mode)
                (name . "^\\*Calendar\\*$")
                (name . "^diary$")))
           ("Dired" (mode . dired-mode))
           ("Web"
            (or (name . "\\.js")
                (name . "\\.s?css")
                (name . "\\.html")
                (name . "\\.php")
                (name . "\\.xml")
                (mode . yaml-mode)))
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
                (name 18 18 :left :elide) " "
                (size-h 9 -1 :right) " "
                (mode 16 16 :left :elide) " "
                filename-and-process))))

;; -------------------------------------------------------------------
;; Calendar
;; -------------------------------------------------------------------

(setq diary-file (expand-file-name "diary" me-emacs-data))
(setq calendar-view-diary-initially-flag t)
(setq calendar-latitude 32.6)
(setq calendar-longitude -85.5)

;; -------------------------------------------------------------------
;; Appt
;; -------------------------------------------------------------------

(appt-activate 1)
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; -------------------------------------------------------------------
;; Font
;; -------------------------------------------------------------------

(set-face-attribute 'default nil
                    :family "Ubuntu Mono"
                    :height 140)

(set-fontset-font "fontset-default"
                  (cons (decode-char 'ucs #xF000)
                        (decode-char 'ucs #xF295))
                  (font-spec :family "FontAwesome" :size 16))

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset (font-spec :family "Noto Sans Mono CJK TC"
                      :size 20)))

(set-face-attribute 'fixed-pitch nil :height 130)

(setq-default line-spacing 5)

;; -------------------------------------------------------------------
;; Key logger
;; -------------------------------------------------------------------

(open-dribble-file
 (expand-file-name
  (format-time-string "key-%FT%H%M%S.log") me-keylog))

;; -------------------------------------------------------------------
;; Gnus
;; -------------------------------------------------------------------

(require 'gnus)
(global-set-key (kbd "<f12>") #'gnus-other-frame)
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
;; org-ref
;; -------------------------------------------------------------------

(use-package org-ref
  :init
  (setq org-ref-default-bibliography me-bib-files
        org-ref-pdf-directory me-bib-pdfs
        org-ref-notes-directory me-bib-notes)
  :config
  (defun me//org-ref-notes-function (thekey)
    (bibtex-completion-edit-notes
     (list (car (org-ref-get-bibtex-key-and-file thekey)))))
  (setq org-ref-notes-function #'me//org-ref-notes-function)
  (add-hook 'org-ref-clean-bibtex-entry-hook
            #'org-ref-downcase-bibtex-entry))

(defun me//org-ref-jump-to-pdf-from-note ()
  "Jump to the pdf with which the current note associated."
  (interactive)
  (let* ((key (file-name-base (buffer-file-name)))
         (pdf-file (funcall org-ref-get-pdf-filename-function key)))
    (if (file-exists-p pdf-file)
        (org-open-file pdf-file)
      (message "no pdf found for %s" key))))

(defun me//org-ref-jump-to-citation-from-note ()
  "Open bibtex file to key with which the note associated."
  (interactive)
  (let* ((key (file-name-base (buffer-file-name)))
         (results (org-ref-get-bibtex-key-and-file key))
         (bibfile (cdr results)))
    (find-file bibfile)
    (bibtex-search-entry key)))

;; -------------------------------------------------------------------
;; Org
;; -------------------------------------------------------------------

(use-package org
  :init
  (setq org-modules
        '(ox-beamer
          ox-bibtex
          ox-latex
          ox-md
          org-table
          org-habit
          org-clock
          org-bbdb
          org-docview
          org-gnus
          org-info))
  :config
  (defun me//init-org ()
    (turn-on-auto-fill)
    (flyspell-mode)

    (advice-add 'ispell-highlight-spelling-error :before
                'me--org-show-context-advice)

    (make-local-variable 'ispell-skip-region-alist)
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

  (add-hook 'org-mode-hook #'me//init-org)

  (setq org-adapt-indentation nil)
  (setq org-list-description-max-indent 0)
  (setq org-support-shift-select t)

  ;; (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")

  (add-to-list 'org-structure-template-alist
               '("b" "#+BEGIN_abstract\n?\n#+END_abstract" ""))
  (add-to-list 'org-structure-template-alist '("B" "#+BIBLIOGRAPHY: ?" ""))
  (add-to-list 'org-structure-template-alist '("C" "#+CAPTION: ?" ""))
  (add-to-list 'org-structure-template-alist '("D" "#+DESCRIPTION: ?" ""))
  (add-to-list 'org-structure-template-alist '("K" "#+KEYWORDS: ?" ""))
  (add-to-list 'org-structure-template-alist '("M" "#+MACRO: ?" ""))
  (add-to-list 'org-structure-template-alist '("N" "#+NAME: ?" ""))
  (add-to-list 'org-structure-template-alist '("O" "#+OPTIONS: ?" ""))
  (add-to-list 'org-structure-template-alist '("R" "#+ATTR_BEAMER: ?" ""))
  (add-to-list 'org-structure-template-alist '("S" "#+SETUPFILE: ?" ""))
  (add-to-list 'org-structure-template-alist '("T" "#+TITLE: ?" ""))
  (add-to-list 'org-structure-template-alist '("X" "#+ATTR_LaTeX: ?" ""))

  (define-key org-mode-map [remap fill-paragraph] #'org-fill-paragraph)
  (define-key org-mode-map (kbd "C-c [") nil)
  ;; (define-key org-mode-map (kbd "C-c ]") nil)

  (setq org-directory (expand-file-name "org" me-emacs-data))

  ;; Recursive update todo statistics
  (setq org-provide-todo-statistics      t
        org-hierarchical-todo-statistics nil)

  ;; Show events from diary
  (setq org-agenda-include-diary t)

  (setq org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)

  ;; (defun me//org-agenda-goto-narrow (&rest args)
  ;;   (org-narrow-to-subtree))
  ;; (advice-add 'org-agenda-goto :after #'me//org-agenda-goto-narrow)

  ;; Resolve open clocks if the user if idle more than 10 minutes.
  (setq org-clock-idle-time 10)

  ;; Sublevels inherit property from parents
  (setq org-use-property-inheritance t)

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

  (setq org-use-fast-tag-selection nil)

  (setq org-capture-templates
        `(("t" "New TODO" entry
           (file "todo.org.gz")
           (file "capture/todo.org")
           :empty-lines 1
           :jump-to-captured t)
          ("m" "Save mail link" entry
           (file "todo.org.gz")
           (file "capture/mail.org")
           :empty-lines 1
           :jump-to-captured t)))

  ;; (require 'ox-latex)

  (setq org-latex-prefer-user-labels t)

  (setq org-latex-pdf-process
        (quote ("PDFLATEX=%latex texi2dvi --shell-escape --pdf --clean --verbose --batch %f")))

  (setq org-latex-listings 'minted)
  ;; (add-to-list 'org-latex-packages-alist '("dvipsnames" "xcolor"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("activate={true,nocompatibility},final,tracking=true,kerning=true,spacing=nonfrench,factor=1100,stretch=10,shrink=10" "microtype"))
  (add-to-list 'org-latex-packages-alist '("" "geometry"))

  (add-to-list 'org-latex-classes
               '("ctexart"
                 "\\documentclass[11pt]{ctexart}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; (defun org-latex-ref-to-cref (text backend info)
  ;;   "Use \\cref instead of \\ref in latex export."
  ;;   (when (org-export-derived-backend-p backend 'latex)
  ;;     (replace-regexp-in-string "\\\\ref{" "\\\\Cref{" text)))

  ;; (add-to-list 'org-export-filter-final-output-functions
  ;;              'org-latex-ref-to-cref)

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

  (require 'ox-beamer)

  (add-to-list 'org-beamer-environments-extra
               '("only" "O" "\\only%a{" "}"))

  ;; (require 'ox-html)

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

  ;; ditaa
  ;; (setq org-ditaa-jar-path "/usr/bin/ditaa")

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

(defun me//init-python()
  (local-set-key (kbd "M-<left>") #'decrease-left-margin)
  (local-set-key (kbd "M-<right>") #'increase-left-margin)
  (python-docstring-mode)
  (diminish 'python-docstring-mode)
  (setq fill-column 78)
  (setq python-check-command "flake8"))
(add-hook 'python-mode-hook #'me//init-python)

(use-package ein
  :config
  (setq ein:jupyter-default-server-command
        "/home/gongzhitaao/.local/env/py3/bin/jupyter"))

;; -------------------------------------------------------------------
;; shell script
;; -------------------------------------------------------------------

(defun me//init-sh()
  (local-set-key (kbd "M-<left>") #'decrease-left-margin)
  (local-set-key (kbd "M-<right>") #'increase-left-margin))
(add-hook 'sh-mode-hook #'me//init-sh)

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

(require 'server)
(unless (server-running-p) (server-start))

;;; init.el ends here
