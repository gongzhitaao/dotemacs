;; -------------------------------------------------------------------
;; Key binding
;; -------------------------------------------------------------------

(global-set-key (kbd "C-z") #'delete-other-windows)

;; FN keys

(global-set-key (kbd "<f6>") #'calendar)
(global-set-key (kbd "<f7>") #'compile)
;; f8 -- deft
;; f10 -- menu
(global-set-key (kbd "<f11>") #'ispell)
;; f12 -- gnus-other-frame

;; Remaping

(global-set-key [remap execute-extended-command] #'helm-M-x)

(global-set-key (kbd "C-x C-f") #'helm-find-files)

(global-set-key [remap goto-line]        #'me-goto-line-with-linum)
(global-set-key [remap isearch-backward] #'isearch-backward-regexp)
(global-set-key [remap isearch-forward]  #'isearch-forward-regexp)
(global-set-key [remap list-buffers]     #'ibuffer)
(global-set-key [remap switch-to-buffer] #'helm-mini)
(global-set-key [remap yank-pop]         #'helm-show-kill-ring)

;; C-c user-key

;; C-c b -- helm-bibtex
;; C-c d -- drag-stuff-mode
(global-set-key (kbd "C-c j") #'ace-jump-mode)
;; C-c g -- helm-gtags-mode
(global-set-key (kbd "C-c m") #'magit-status)
;; C-c M -- multiple-cursor
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c q") #'auto-fill-mode)
(global-set-key (kbd "C-c r c") #'org-ref-open-citation-at-point)
(global-set-key (kbd "C-c r p") #'org-ref-open-pdf-at-point)
(global-set-key (kbd "C-c r n") #'org-ref-open-notes-at-point)
;; C-c u -- undo-tree
;; C-c w -- writeroom-mode
(global-set-key (kbd "C-c (") #'rainbow-delimiters-mode)
(global-set-key (kbd "C-c =") #'align-regexp)

;; M-s search key

;; M-s a helm-do-ag
(global-set-key (kbd "M-s g") #'helm-do-grep-ag)
;; M-s s helm-swoop

;; -------------------------------------------------------------------
;; Variable
;; -------------------------------------------------------------------

(defconst me-home "~" "My home directory.")
(defconst me-dropbox (expand-file-name "Dropbox" me-home))
(defconst me-emacs (expand-file-name "dotfiles/emacs.d" me-dropbox))
(defconst me-emacs-data (expand-file-name "data" me-emacs))

(defconst me-emacs-tmp (expand-file-name "tmp" user-emacs-directory))
(unless (file-exists-p me-emacs-tmp)
  (mkdir me-emacs-tmp))

(defconst me-keylog (expand-file-name "keylog" user-emacs-directory))
(unless (file-exists-p me-keylog)
  (mkdir me-keylog))

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
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun me-clear-shell ()
  "Clear shell window."
   (interactive)
   (let ((old-max comint-buffer-maximum-size))
     (setq comint-buffer-maximum-size 0)
     (comint-truncate-buffer)
     (setq comint-buffer-maximum-size old-max)))

(me-with-region-or-line #'kill-ring-save)

;; -------------------------------------------------------------------
;; Misc
;; -------------------------------------------------------------------

(setq display-time-24hr-format t
      display-time-day-and-date nil)
(display-time)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq-default tab-stop-list (number-sequence 2 120 2))
(setq tab-always-indent 'complete)

(delete-selection-mode t)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-pattern nil)

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

(defun me//init-read-only-mode ()
  (diminish 'read-only-mode " "))
(add-hook 'read-only-mode-hook #'me//init-read-only-mode)

(setq view-read-only t)
(defun me//init-view-mode ()
  (diminish 'view-mode " "))
(add-hook 'view-mode-hook #'me//init-view-mode)

(setq delete-by-moving-to-trash t)

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
               (> (- current (float-time (fifth (file-attributes file))))
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
(add-to-list 'recentf-exclude (expand-file-name ".*" me-emacs-tmp))
(add-to-list 'recentf-exclude (expand-file-name "~/.newsrc*"))
(add-to-list 'recentf-keep 'file-remote-p)
(recentf-mode 1)

;; -------------------------------------------------------------------
;; savehist
;; -------------------------------------------------------------------

(require 'savehist)
(savehist-mode 1)
(setq savehist-additional-variables '(search ring regexp-search-ring)
      savehist-file (expand-file-name "savehist" me-emacs-tmp))

;; -------------------------------------------------------------------
;; saveplace
;; -------------------------------------------------------------------

(setq-default save-place t)
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" me-emacs-tmp))

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

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; -------------------------------------------------------------------
;; Theme first
;; -------------------------------------------------------------------

(add-to-list 'default-frame-alist '(background-color . "gray20"))
(add-to-list 'default-frame-alist '(foreground-color . "gray90"))

(global-hl-line-mode +1)
(set-face-background 'hl-line "gray15")

;; -------------------------------------------------------------------
;; Helm
;; -------------------------------------------------------------------

(use-package helm-config
  :diminish helm-mode
  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  (helm-adaptive-mode 1)
  (helm-push-mark-mode 1)

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
;; yasnippet
;; -------------------------------------------------------------------

;; (use-package yasnippet
;;   :init
;;   (setq yas-snippet-dirs
;;         `(,(expand-file-name "snippets" me-emacs)
;;           yas-installed-snippets-dir))
;;   :config
;;   (yas-global-mode))

;; -------------------------------------------------------------------
;; diminish
;; -------------------------------------------------------------------

(diminish 'auto-fill-function " ")
(diminish 'isearch-mode " ")

(defun me//init-flyspell-mode()
  (diminish 'flyspell-mode))
(add-hook 'flyspell-mode-hook #'me//init-flyspell-mode)

(defun me//init-auto-revert-mode ()
  (diminish 'auto-revert-mode " "))
(add-hook 'auto-revert-mode-hook #'me//init-auto-revert-mode)

;; -------------------------------------------------------------------
;; Async
;; -------------------------------------------------------------------

(use-package async
  :commands (dired-async-mode)
  :init (dired-async-mode 1))

;; -------------------------------------------------------------------
;; multiple cursors
;; -------------------------------------------------------------------

(use-package multiple-cursors
  :defines my-multiple-cursors-map
  :bind-keymap ("C-c M" . my-multiple-cursors-map)
  :config
  (defvar my-multiple-cursors-map
    (let ((map (make-sparse-keymap)))

      (define-key map (kbd "C-a") #'mc/edit-beginnings-of-lines)
      (define-key map (kbd "C-e") #'mc/edit-ends-of-lines)
      (define-key map (kbd "C-s") #'mc/mark-all-in-region)

      (define-key map (kbd "a")   #'mc/mark-all-like-this-dwim)
      (define-key map (kbd "l")   #'mc/edit-lines)
      (define-key map (kbd "m")   #'mc/mark-more-like-this-extended)
      (define-key map (kbd "n")   #'mc/mark-next-like-this)
      (define-key map (kbd "p")   #'mc/mark-previous-like-this)
      (define-key map (kbd "r")   #'mc/mark-all-in-region-regexp)

      map)))

;; -------------------------------------------------------------------
;; tramp
;; -------------------------------------------------------------------

(use-package tramp
  :config
  (setq tramp-default-method "ssh"
        tramp-persistency-file-name (expand-file-name "tramp" me-emacs-tmp)))

;; -------------------------------------------------------------------
;; writeroom
;; -------------------------------------------------------------------

(use-package writeroom-mode
  :bind ("C-c w" . writeroom-mode)
  :config (setq writeroom-width (+ fill-column 10)))

;; -------------------------------------------------------------------
;; anzu
;; -------------------------------------------------------------------

(use-package anzu
  :config (global-anzu-mode +1)
  :diminish anzu-mode)

;; -------------------------------------------------------------------
;; helm-gtags
;; -------------------------------------------------------------------

(use-package helm-gtags
  :diminish (helm-gtags-mode . " ")
  :config
  (setq helm-gtags-prefix-key "\C-cg")
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
;; wgrep
;; -------------------------------------------------------------------
(use-package wgrep
  :config (setq wgrep-auto-save-buffer t))
(use-package wgrep-helm)

;; -------------------------------------------------------------------
;; Drag
;; -------------------------------------------------------------------

(use-package drag-stuff
  :bind ("C-c d" . drag-stuff-mode)
  :diminish (drag-stuff-mode . " "))

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
;; fancy narrow
;; -------------------------------------------------------------------

(use-package fancy-narrow
  :config
  (setq fancy-narrow-lighter "")
  (fancy-narrow-mode 1))

;; -------------------------------------------------------------------
;; smartparens
;; -------------------------------------------------------------------

(use-package smartparens-config
  :diminish smartparens-mode
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode 1)

  (sp-with-modes
      '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
    (sp-local-tag "i" "\"<" "\">")
    (sp-local-tag "i" "\"[" "\"]"))

  ;; (set-face-attribute 'sp-pair-overlay-face nil :background "#443152")
  ;; (set-face-attribute 'sp-show-pair-match-face nil :background "#A16946")

  ;; Still dont like this

  ;; (sp-with-modes 'org-mode
  ;;   (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
  ;;   (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
  ;;   (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  ;;   (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  ;;   (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  ;;   (sp-local-pair "«" "»"))

  (sp-local-pair '(emacs-lisp-mode lisp-mode lisp-interaction-mode) "`" "'")
  (sp-local-pair '(emacs-lisp-mode lisp-mode lisp-interaction-mode) "`"
                 nil :when '(sp-in-string-p))
  (sp-local-pair '(emacs-lisp-mode lisp-mode lisp-interaction-mode) "'"
                 nil :actions nil)

  (setq sp-cancel-autoskip-on-backward-movement nil)
  (setq sp-navigate-consider-stringlike-sexp
        '(lisp-mode emacs-lisp-mode latex-mode LaTeX-mode TeX-mode))

  (define-key smartparens-mode-map (kbd "C-c s f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-c s b") 'sp-backward-sexp)

  (define-key smartparens-mode-map (kbd "C-c s d") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-c s D") 'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "C-c s a") 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "C-c s e") 'sp-end-of-sexp)

  (define-key smartparens-mode-map (kbd "C-c s u") 'sp-up-sexp)
  (define-key smartparens-mode-map (kbd "C-c s U") 'sp-backward-up-sexp)
  (define-key smartparens-mode-map (kbd "C-c s t") 'sp-transpose-sexp)

  (define-key smartparens-mode-map (kbd "C-c s n") 'sp-next-sexp)
  (define-key smartparens-mode-map (kbd "C-c s p") 'sp-previous-sexp)

  (define-key smartparens-mode-map (kbd "C-c s k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-c s w") 'sp-copy-sexp)

  (define-key smartparens-mode-map (kbd "C-c s s") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c s r") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-c s S") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c s R") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-c s F") 'sp-forward-symbol)
  (define-key smartparens-mode-map (kbd "C-c s B") 'sp-backward-symbol)

  (define-key smartparens-mode-map (kbd "C-c s [") 'sp-select-previous-thing)
  (define-key smartparens-mode-map (kbd "C-c s ]") 'sp-select-next-thing)

  (define-key smartparens-mode-map (kbd "C-c s C-i") 'sp-splice-sexp)
  (define-key smartparens-mode-map (kbd "C-c s <delete>") 'sp-splice-sexp-killing-forward)
  (define-key smartparens-mode-map (kbd "C-c s <backspace>") 'sp-splice-sexp-killing-backward)
  (define-key smartparens-mode-map (kbd "C-c s C-<backspace>") 'sp-splice-sexp-killing-around)

  (define-key smartparens-mode-map (kbd "C-c s C-w") 'sp-wrap)
  (define-key smartparens-mode-map (kbd "C-c s C-u") 'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-b") 'sp-backward-unwrap-sexp)

  (define-key smartparens-mode-map (kbd "C-c s C-t") 'sp-prefix-tag-object)
  (define-key smartparens-mode-map (kbd "C-c s C-p") 'sp-prefix-pair-object)
  (define-key smartparens-mode-map (kbd "C-c s C-c") 'sp-convolute-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-a") 'sp-absorb-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-e") 'sp-emit-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-p") 'sp-add-to-previous-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-n") 'sp-add-to-next-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-j") 'sp-join-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-s") 'sp-split-sexp)
  (define-key smartparens-mode-map (kbd "C-c s C-r") 'sp-raise-sexp))

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
  :diminish which-key-mode
  :config (which-key-mode))

;; -------------------------------------------------------------------
;; BibTeX
;; -------------------------------------------------------------------

(defvar me-bib (expand-file-name "bibliography" me-dropbox)
  "My bibliography collection path.")
(defvar me-bib-file (expand-file-name "nn.bib" me-bib)
  "My bibliography files.")
(defvar me-bib-pdf-path (expand-file-name "pdf" me-bib)
  "Paths containing my PDFs of the bibliography.")
(defvar me-bib-notes-path
  (expand-file-name "notes" me-emacs-data)
  ;; (expand-file-name "notes" me-bib)
  "Path to store my notes on each papers.")

(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator ""
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "_"
      bibtex-autokey-titlewords 1
      bibtex-autokey-titlewords-stretch 0
      bibtex-autokey-titleword-length nil)

;; -------------------------------------------------------------------
;; helm-bibtex
;; -------------------------------------------------------------------

(define-key helm-command-map (kbd "b") #'helm-bibtex)

(use-package helm-bibtex
  :bind ("C-c b" . helm-bibtex)
  :config
  (setq bibtex-completion-bibliography me-bib-file
        bibtex-completion-library-path me-bib-pdf-path
        bibtex-completion-notes-path me-bib-notes-path)

  (setq bibtex-completion-notes-extension ".org")
  (setq bibtex-completion-pdf-open-function #'helm-open-file-with-default-tool)

  (setq bibtex-completion-pdf-symbol ""
        bibtex-completion-notes-symbol ""))

(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq bibtex-dialect 'biblatex)
(setq bibtex-align-at-equal-sign t)
(setq bibtex-text-indentation 20)

(defun me//init-bibtex ()
  (local-set-key [remap fill-paragraph] #'bibtex-fill-entry)
  (local-set-key [remap bibtex-clean-entry] #'org-ref-clean-bibtex-entry)
  (local-set-key (kbd "C-c C-v") #'bibtex-validate)
  (setq fill-column 140))
(add-hook 'bibtex-mode-hook #'me//init-bibtex)

(add-hook 'latex-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

;; -------------------------------------------------------------------
;; org-ref
;; -------------------------------------------------------------------

(use-package org-ref
  :init
  (setq org-ref-default-bibliography (list me-bib-file)
        org-ref-pdf-directory me-bib-pdf-path
        org-ref-notes-directory me-bib-notes-path)
  (defun me//org-ref-notes-function (thekey)
    (bibtex-completion-edit-notes
     (car (org-ref-get-bibtex-key-and-file thekey))))
  (setq org-ref-notes-function #'me//org-ref-notes-function)
  :after (org))

;; -------------------------------------------------------------------
;; reftex
;; -------------------------------------------------------------------

(use-package reftex
  :diminish reftex-mode
  :config
  (add-hook 'latex-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t
        reftex-ref-style-default-list '("Cleveref" "Hyperref" "Fancyref")
        reftex-default-bibliography (list me-bib-file)))

;; -------------------------------------------------------------------
;; BBDB
;; -------------------------------------------------------------------

(use-package bbdb
  :config
  (bbdb-initialize 'gnus 'mail 'message 'anniv)

  (setq bbdb-complete-mail-allow-cycling t
        bbdb-allow-duplicates t
        bbdb-message-all-addresses t
        bbdb-file
        (expand-file-name "contacts.bbdb.gz" me-emacs-data))

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
                (name . "\\.css")
                (name . "\\.html")
                (name . "\\.php")
                (name . "\\.xml")
                (mode . yaml-mode)))
           ("Text"
            (or (name . "\\.\\(tex\\|bib\\|csv\\)")
                (mode . org-mode)
                (mode . markdown-mode)
                (mode . text-mode)))
           ("Data"
            (or (mode . gnuplot-mode)
                (mode . octave-mode)
                (mode . R-mode)))
           ("Coding"
            (or (mode . shell-script-mode)
                (mode . sh-mode)
                (mode . emacs-lisp-mode)
                (name . "\\.[ch]\\(pp\\|xx\\|\\+\\+\\)?")
                (mode . python-mode)
                (name . "\\.ya?ml")
                (name . "\\.R")
                (name . "\\.lua")))
           ("Mail"
            (or (mode . message-mode)
                (mode . mail-mode)
                (mode . gnus-group-mode)
                (mode . gnus-summary-mode)
                (mode . gnus-article-mode)
                (mode . gnus-server-mode)
                (mode . gnus-browse-mode)
                (name . "^\\.newsrc-dribble")))
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
;; Lilypond
;; -------------------------------------------------------------------

(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))

(defun me//init-lilypond ()
  (turn-on-font-lock))

(add-hook 'LilyPond-mode-hook #'me//init-lilypond)

;; -------------------------------------------------------------------
;; Appt
;; -------------------------------------------------------------------

(setq diary-file (expand-file-name "diary" me-emacs-data))
(setq calendar-view-diary-initially-flag t)
(appt-activate 1)
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; -------------------------------------------------------------------
;; Font
;; -------------------------------------------------------------------

(set-face-attribute 'default nil
                    :family "Ubuntu Mono"
                    :height 120)

(set-fontset-font "fontset-default"
                  (cons (decode-char 'ucs #xF000)
                        (decode-char 'ucs #xF295))
                  (font-spec :family "FontAwesome" :size 28))

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset (font-spec :family "Noto Sans Mono CJK TC"
                      :size 32)))

(set-face-attribute 'fixed-pitch nil :height 105)

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

;; -------------------------------------------------------------------
;; Org
;; -------------------------------------------------------------------

(use-package org
  :init
  (setq org-modules
        '(ox-beamer
          ox-bibtex
          ox-latex
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

  (setq org-list-description-max-indent 5)
  (setq org-src-preserve-indentation t)
  (setq org-support-shift-select t)

  (add-to-list 'org-structure-template-alist
               '("b" "#+BEGIN_abstract\n?\n#+END_abstract" ""))
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

  (setq org-directory (expand-file-name "org" me-emacs-data))

  ;; never use [1]-styled footnote.
  (setq org-footnote-re
        (concat "\\[\\(?:"
                ;; Match inline footnotes.
                (org-re "fn:\\([-_[:word:]]+\\)?:\\|")
                ;; Match other footnotes.
                ;; "\\(?:\\([0-9]+\\)\\]\\)\\|"
                (org-re "\\(fn:[-_[:word:]]+\\)")
                "\\)")

        org-footnote-definition-re
        (org-re "^\\[\\(fn:[-_[:word:]]+\\)\\]"))

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
        org-src-preserve-indentation nil)

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
           (file+headline "todo.org.gz" "Tasks")
           (file "capture/todo.org")
           :empty-lines 1)
          ("l" "Weekly log" item
           (file+datetree ,(expand-file-name "notes/log.org"
                                             me-emacs-data))
           (file "capture/weekly.org"))
          ("m" "Save mail link" entry
           (file+headline "todo.org.gz" "Mail")
           (file "capture/mail.org")
           :empty-lines 1)))

  ;; (require 'ox-latex)

  (setq org-latex-prefer-user-labels t)

  (setq org-latex-pdf-process
        (quote ("texi2dvi --pdf --clean --verbose --batch %f")))

  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("activate={true,nocompatibility},final,tracking=true,kerning=true,spacing=nonfrench,factor=1100,stretch=10,shrink=10" "microtype"))
  (add-to-list 'org-latex-packages-alist '("" "geometry"))

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
      linkcolor=red,
      citecolor=ForestGreen,
      filecolor=magenta,
      urlcolor=cyan}\n")

  (require 'ox-beamer)
  (add-to-list 'org-beamer-environments-extra
               '("only" "O" "\\only%a{" "}"))

  ;; (require 'ox-html)

  (setq org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil)

  ;; Postamble.
  (setq org-html-postamble t
        org-html-postamble-format
        '(("en" "<a class=\"author\"
           href=\"http://gongzhitaao.org\">%a</a> / <span
           class=\"date\">%T</span><span class=\"creator\">%c</span>")))

  ;; ditaa
  (setq org-ditaa-jar-path "/usr/bin/ditaa")

  (load-file (expand-file-name "my-org-misc.el" org-directory)))

;; -------------------------------------------------------------------
;; C/C++
;; -------------------------------------------------------------------

(add-hook 'c-mode-common-hook #'google-set-c-style)

(defun me//turn-on-hs-minor-mode()
  (interactive)
  (hs-minor-mode 1))

(add-hook 'c-mode-common-hook #'me//turn-on-hs-minor-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

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
  :defer t
  :config
  (add-hook 'web-mode-hook #'me//init-web-mode))

;; -------------------------------------------------------------------
;; Python
;; -------------------------------------------------------------------

(defun me//init-python()
  (local-set-key (kbd "M-<left>") #'decrease-left-margin)
  (local-set-key (kbd "M-<right>") #'increase-left-margin))
(add-hook 'python-mode-hook #'me//init-python)

;; -------------------------------------------------------------------
;; Lua
;; -------------------------------------------------------------------

(defun me//lua-send-current-line-and-next()
  (lua-send-current-line)
  (next-line))

(use-package lua-mode
  :mode "\\.lua\\'"
  :config
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

(setq confirm-kill-emacs 'yes-or-no-p)

(require 'server)
(unless (server-running-p) (server-start))
