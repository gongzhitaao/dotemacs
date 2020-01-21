;;; init.el --- Yet another Emacs config  -*- lexical-binding: t; -*-
;; Time-stamp: <2020-01-21 14:04:00 gongzhitaao>

;;; Commentary:
;; me/xxx: mostly interactive functions, may be executed with M-x or keys
;; me//xxx: internal helper functions, not called directly by user
;; me-xxx: custom variables

;;; Code:

;; copied from spacemacs
(defun me//remove-gui-elements ()
  "Remove the menu bar, tool bar and scroll bars."
  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
    (menu-bar-mode -1))
  (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1)))

(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(defvar file-name-handler-alist-original file-name-handler-alist)
(defun me//pre-emacs-startup ()
  "Adjust some settings to make the startup more smooth."

  ;; set to 100Mib for startup
  (setq gc-cons-threshold (* 100 1024 1024))

  (setq file-name-handler-alist nil)

  (me//remove-gui-elements)
  (hidden-mode-line-mode))

(me//pre-emacs-startup)

(defun me//post-emacs-startup ()
  "Cleanup after EMACS startup."

  ;; reset to 150MiB
  (setq gc-cons-threshold (* 150 1024 1024))

  (setq file-name-handler-alist file-name-handler-alist-original)
  (makunbound 'file-name-handler-alist-original)
  (menu-bar-mode 1))

(add-hook 'emacs-startup-hook #'me//post-emacs-startup)

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

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode)

(eval-when-compile
  (require 'use-package))
(require 'delight)
(require 'bind-key)

(setq load-prefer-newer t)

;; The ~/.emacs.d/site-lisp contains some configs that don't fit in here,
;; because it is site-specific or it contains sensitive information.  These
;; configs will not go into the public git repo.
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; I'm always skeptical about cask and use-package.  If they fail mysteriously,
;; I could not start my Emacs.  I guess a better way is to backup the packages
;; every day just in case.  However, use-package, bind-keys and cask are so much
;; more convenient, I surrender.

;; =============================================================================
;; Key binding
;; =============================================================================

(global-set-key (kbd "C-z") #'delete-other-windows)
(global-set-key (kbd "<backtab>") #'decrease-left-margin)

(global-set-key (kbd "C-t") #'transpose-sexps)
(global-set-key (kbd "C-+") #'me/join-next-line)
(global-set-key (kbd "C->") #'mc/mark-next-like-this-word)
(global-set-key (kbd "C-<") #'mc/mark-previous-like-this-word)
(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)

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
;; C-c c -- helm-flycheck

(use-package drag-stuff
  :bind ("C-c d" . drag-stuff-mode)
  :delight (drag-stuff-mode " ")
  :config (drag-stuff-define-keys))

(bind-keys :prefix-map me-editing-command-map
           :prefix "C-c e"
           ("2"   . me/double-space-after-dot)
           ("c"   . set-buffer-file-coding-system)
           ("d"   . delete-duplicate-lines)
           ;; g - me/grab-google3-python-imports
           ("l"   . magit-log-buffer-file)
           ("M-w" . me/copy-region-escaped)
           ("s l" . sort-lines)
           ("s L" . sort-fields)
           ("s s" . me/sort-symbols)
           ("s w" . me/sort-words)
           ("S"   . me/sudo-edit)
           ("t"   . me/gnome-terminal)
           ("v"   . add-file-local-variable)
           ("V"   . add-file-local-variable-prop-line))

(use-package magit
  :bind ("C-c g" . magit-status)
  :config (setq git-commit-major-mode 'org-mode))

(use-package multiple-cursors
  :config
  (bind-keys :prefix-map me-multiple-cursors-command-map
             :prefix "C-c m"
             ("C-a" . mc/edit-beginnings-of-lines)
             ("C-e" . mc/edit-ends-of-lines)
             ("a"   . mc/mark-all-like-this-dwim)
             ("l"   . mc/edit-lines)
             ("i n" . mc/insert-numbers)
             ("i l" . mc/insert-letters))
  (setq mc/mode-line
        `(" mc:" (:eval (format ,(propertize "%d" 'face 'custom-rogue)
                                (mc/num-cursors))))))

(bind-keys :prefix-map me-org-command-map
           :prefix "C-c o"
           ("a"   . org-agenda)
           ("c"   . org-capture)
           ("e"   . me/org-ref-open-entry)
           ("h"   . me/org-custom-id-get-create-hash)
           ("H"   . me/org-custom-id-get-create-hash-all)
           ("i"   . me/org-custom-id-get-create)
           ("I"   . me/org-custom-id-get-create-all)
           ("l b" . org-ref-extract-bibtex-entries)
           ("l f" . org-ref-list-of-figures)
           ("l t" . org-ref-list-of-tables)
           ("n"   . me/org-ref-open-note)
           ("p"   . me/org-ref-open-pdf)
           ("s"   . me/org-sort-orgref-citation-list-by-year))

;; C-c s -- smartparens
;; C-c u -- undo-tree
(global-set-key (kbd "C-c =") #'align-regexp)
(global-set-key (kbd "C-c C-q") #'bury-buffer)

;; C-c | -- fci-mode

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
(global-set-key (kbd "M-Q") #'me/unfill-paragraph)
;; M-r move-to-window-line-top-bottom
;; M-s search
;; M-t transpose-word
;; M-u upcase-word
;; M-v scroll-down
;; M-w kill-ring-save
;; M-x helm-M-x execute command
;; M-y helm-show-kill-ring
;; M-z zap-to-char

(global-set-key (kbd "M-<right>") #'sp-forward-sexp)
(global-set-key (kbd "M-<down>") #'sp-down-sexp)
(global-set-key (kbd "M-<left>") #'sp-backward-sexp)
(global-set-key (kbd "M-<up>") #'sp-up-sexp)

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

(defconst me-emacs-data-private
  (expand-file-name "data/private" user-emacs-directory)
  "Private EMACS data synced to a private repo.")
(defconst me-emacs-data-public
  (expand-file-name "data/public" user-emacs-directory)
  "Public EMACS data synced to a public repo.")

(defconst me-emacs-tmp (expand-file-name "tmp" user-emacs-directory)
  "Directory for temporary files.")
(unless (file-exists-p me-emacs-tmp)
  (mkdir me-emacs-tmp))

(defconst me-keylog (expand-file-name "keylog" me-emacs-data-private)
  "The file path that Logs every key stroke in my EMACS.")
(unless (file-exists-p me-keylog)
  (mkdir me-keylog))

(defconst me-local-conf (expand-file-name "local.el" user-emacs-directory)
  "Local configuration not shared around machines.")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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

(defun me/double-space-after-dot (beg end)
  "Exactly two spaces after dot in region (BEG, END).

I use `sentence-end-double-space', thus I need a way to replace
all '.<space>' with '.<space><space>'."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((old "\\.[[:blank:]]+")
            (new ".  "))
        (while (re-search-forward old nil t)
          (replace-match new))))))

(defun me/unfill-paragraph (&optional region)
  "Unfill paragraph, in the REGION if provided."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun me/copy-region-escaped (beg end)
  "Copy buffer between BEG and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((str (buffer-substring-no-properties beg end))
            (print-escape-newlines t))
        (kill-new (prin1-to-string str)))))
  (setq deactivate-mark t)
  nil)

(defun me/gnome-terminal ()
  "Open gnome-terminal."
  (interactive)
  (start-process "gnome-terminal" nil "gnome-terminal"))

;; =============================================================================
;; Appearance
;; =============================================================================

(blink-cursor-mode 0)
(mouse-avoidance-mode 'animate)

(defun me//init-prog-mode ()
  (setq-local display-line-numbers t))

(add-hook 'prog-mode-hook #'me//init-prog-mode)

(set-face-foreground 'line-number-current-line "Lime")

(scroll-bar-mode 0)
(setq scroll-margin 0
      scroll-preserve-screen-position 1)

(setq visible-bell t)
(setq inhibit-startup-message t
      resize-mini-windows t)

(defun me//set-title-bar()
  "Update title bar."
  (let* ((name (or buffer-file-name dired-directory (buffer-name)))
         (hostname (or (file-remote-p name 'host) "localhost")))
    (concat "@" hostname "     " name)))
(setq frame-title-format '((:eval (me//set-title-bar))))

;; (global-hl-line-mode)
;; (set-face-background 'hl-line "black")

;; Show the search result count.
(use-package anzu
  :delight
  :config
  (global-anzu-mode +1)
  (setq anzu-search-threshold 1000))

;; Show a dimmed delimiter at fill-column.
(use-package fill-column-indicator
  :bind ("C-c |" . fci-mode)
  :config
  (setq fci-rule-color "gray50")
  ;; (let ((hooks '(prog-mode-hook org-mode-hook markdown-mode-hook
  ;;                               tex-mode-hook message-mode-hook)))
  ;;   (mapc (lambda (x) (add-hook x #'turn-on-fci-mode)) hooks))
  )
;; (use-package fill-column-indicator
;;   :config
;;   (setq fci-rule-color "gray50")
;;   (let ((hooks '(c++-mode-hook)))
;;     (mapc (lambda (x) (add-hook x #'turn-on-fci-mode)) hooks)))

;; Center the editing content.
(use-package writeroom-mode
  :config
  (setq writeroom-fullscreen-effect nil
        writeroom-maximize-window nil
        writeroom-mode-line t
        writeroom-use-derived-modes t
        writeroom-width 100)
  (setq writeroom-major-modes
        '(prog-mode dired-mode Info-mode calendar-mode text-mode))
  (setq writeroom-major-modes-exceptions
        '(web-mode))
  (delete 'writeroom-set-menu-bar-lines writeroom-global-effects)
  (global-writeroom-mode))

;; Highlight the cursor when jumping around.
;; (use-package beacon
;;   :delight
;;   :config (beacon-mode))

;; Select interesting regions
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Working with parenthesis
(use-package smartparens-config
  :delight smartparens-mode
  :config
  (bind-keys ("M-<backspace>" . sp-backward-unwrap-sexp)
             ("M-<delete>"    . sp-unwrap-sexp)
             :prefix-map smartparens-mode-map
             :prefix "C-c s"
             ("<backspace>" . sp-splice-sexp-killing-backward)
             ("<delete>"    . sp-splice-sexp-killing-forward)
             ("<left>"      . sp-backward-slurp-sexp)
             ("<right>"     . sp-forward-slurp-sexp)
             ("a"           . sp-beginning-of-sexp)
             ("b"           . sp-backward-sexp)
             ("C-<delete>"  . sp-splice-sexp-killing-around)
             ("C-<left>"    . sp-forward-barf-sexp)
             ("C-<right>"   . sp-backward-barf-sexp)
             ("D"           . sp-backward-down-sexp)
             ("d"           . sp-down-sexp)
             ("e"           . sp-end-of-sexp)
             ("E"           . sp-up-sexp)
             ("f"           . sp-forward-sexp)
             ("k"           . sp-kill-sexp)
             ("n"           . sp-next-sexp)
             ("p"           . sp-previous-sexp)
             ("r"           . sp-rewrap-sexp)
             ("s"           . sp-split-sexp)
             ("t"           . sp-transpose-sexp)
             ("w"           . sp-copy-sexp))

  (smartparens-global-mode)
  (show-smartparens-global-mode)

  (setq sp-highlight-pair-overlay nil
        sp-show-pair-from-inside nil
        sp-cancel-autoskip-on-backward-movement nil)

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
                 nil :actions nil))

;; (use-package hl-todo
;;   :config
;;   (let ((key-faces '(("TODO" . "#ff0000")
;;                      ("FIXME" . "#ff0000"))))
;;     (dolist (elm key-faces)
;;       (setcdr (assoc "TODO" hl-todo-keyword-faces) (cdr elm))))
;;   (global-hl-todo-mode))

;; fonts
;; -----------------------------------------------------------------------------

(set-face-attribute 'default nil
                    :family "Iosevka SS09"
                    :height 140)

(set-fontset-font "fontset-default"
                  (cons (decode-char 'ucs #xF000)
                        (decode-char 'ucs #xF4E3))
                  (font-spec :family "Font Awesome 5 Free" :size 16))

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font) charset (font-spec
                                        :family "Sarasa Mono TC"
                                        :size 20)))

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

;; =============================================================================
;; General editing
;; =============================================================================

(setq confirm-kill-emacs 'yes-or-no-p
      delete-by-moving-to-trash t
      select-enable-clipboard t
      tab-always-indent 'complete
      view-read-only t)

(setq-default fill-column 80
              indent-tabs-mode nil
              sort-fold-case t
              standard-indent 2
              tab-stop-list (number-sequence 2 120 2)
              tab-width 4
              truncate-lines t)

;; (use-package aggressive-indent
;;   :delight
;;   :config
;;   (global-aggressive-indent-mode)
;;   (dolist (m '(html-mode sh-mode))
;;     (add-to-list 'aggressive-indent-excluded-modes m)))

(delete-selection-mode)
(add-hook 'before-save-hook 'time-stamp)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)

(global-subword-mode 1)

;; This is slow if enabled.
(setq blink-matching-paren nil)

;; Whitespace-mode need to be called before highlight-indent-guides, otherwise
;; no guides are shown.
(use-package highlight-indent-guides
  :config (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
  :delight)

(use-package whitespace
  :after highlight-indent-guides
  :delight global-whitespace-mode
  :config
  (setq whitespace-line-column fill-column)
  (setq whitespace-style '(empty face indentation lines-tail
                                 space-after-tab space-before-tab
                                 spaces tabs trailing))
  (setq whitespace-global-modes t)
  (dolist (hook '(prog-mode-hook))
    (add-hook hook #'whitespace-mode))
  ;; (global-whitespace-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup))

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
  :config
  (setq avy-background t)
  (setq avy-case-fold-search nil))

(use-package golden-ratio-scroll-screen
  :config
  (setq golden-ratio-scroll-highlight-flag nil)
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

(use-package visual-regex
  :bind ("M-s q" . vr/query-replace))

;; Searching
;; -----------------------------------------------------------------------------

(use-package helm-ag
  :after helm
  :bind ("M-s a" . helm-do-ag))

(use-package helm-swoop
  :after helm
  :bind (("M-s s" . helm-swoop)
         :map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch))
  :config
  (setq helm-multi-swoop-edit-save t))

;; Completion
;; -----------------------------------------------------------------------------

(setq abbrev-file-name (expand-file-name "abbrev_defs" me-emacs-data-private))

(use-package yasnippet
  :delight yas-minor-mode
  :config
  (setq yas-snippet-dirs `(,(expand-file-name "snippets" me-emacs-data-public)))
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
                   ,(expand-file-name ".cask/.*" user-emacs-directory))))
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
  :config
  (bind-keys ("M-\"" . flycheck-keymap-prefix))
  (define-prefix-command 'flycheck-keymap-prefix)
  (fset 'flycheck-keymap-prefix flycheck-command-map)
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-global-modes '(not org-mode)
        flycheck-keymap-prefix flycheck-command-map
        flycheck-python-flake8-executable "flake8")
  (global-flycheck-mode))

(use-package helm-flycheck
  :bind ("C-c c" . helm-flycheck))

;; Edit remote files
(use-package tramp
  :config (setq tramp-default-method "ssh"))

(use-package tramp-cache
  :config
  (setq tramp-persistency-file-name
        (expand-file-name "tramp" me-emacs-tmp)))

(use-package bookmark
  :config
  (setq bookmark-default-file
        (expand-file-name "bookmarks" me-emacs-data-private)))

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

(use-package ffap)

(use-package dired
  :bind (:map dired-mode-map
         ("b" . helm-mini)
         ("f" . find-file-literally-at-point))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-listing-switches "-alh")

  (defface me-dired-dim-0 '((t (:foreground "gray70")))
    "Dimmed face."
    :group 'me-dired)

  (defface me-dired-dim-1 '((t (:foreground "gray50")))
    "Dimmed face."
    :group 'me-dired)

  (defface me-dired-executable '((t (:foreground "green")))
    "face for executables"
    :group 'me-dired)

  (let* ((user-group-anchor (concat "^..[-dl][-rwxsS]\\{9\\}[ ]*"
                                    "\\(?:[0-9]*?\\)[ ]+"
                                    "\\(.*?\\)[ ]+"
                                    "\\(.*?\\)[ ]+"
                                    "\\(?:.*?\\)[ ]+"))
         (date-0 "\\([0-9][0-9]-[0-9][0-9][ ]+[0-9][0-9]:[0-9][0-9]\\)")
         (date-1 "\\([0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]\\)")
         (executable (concat "^[ ].-\\(?:.*x.*?\\)[ ]"
                             "\\(?:.*?\\(?:[0-9][0-9]:[0-9][0-9]\\)\\|"
                             "\\(?:[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]\\)\\)[ ]+"
                             "\\(.*$\\)")))
    (font-lock-add-keywords 'dired-mode
                            `((,user-group-anchor
                               (1 'me-dired-dim-1)
                               (2 'me-dired-dim-1)
                               (,date-0 nil nil (0 'me-dired-dim-0))
                               (,date-1 nil nil (0 'me-dired-dim-1)))
                              (,executable
                               (1 'me-dired-executable))))
    (font-lock-add-keywords 'wdired-mode
                            `((,user-group-anchor
                               (1 'me-dired-dim-1)
                               (2 'me-dired-dim-1)
                               (,date-0 nil nil (0 'me-dired-dim-0))
                               (,date-1 nil nil (0 'me-dired-dim-1)))
                              (,executable
                               (1 'me-dired-executable))))))

(use-package dired-x)

(use-package async
  :delight dired-async-mode
  :config (dired-async-mode))

(use-package ls-lisp
  :config
  (setq ls-lisp-dirs-first t
        ls-lisp-use-insert-directory-program nil))

;; Calendar
;; -----------------------------------------------------------------------------

(use-package solar
  :config
  (setq calendar-latitude 32.6)
  (setq calendar-longitude -85.5))

(use-package calendar
  :config
  (setq calendar-week-start-day 1
        calendar-chinese-all-holidays-flag t
        diary-file (expand-file-name "org/time-machine/diary"
                                     me-emacs-data-private))
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
  (set-face-background 'cal-china-x-general-holiday-face "forest green")

  (setq calendar-date-display-form calendar-iso-date-display-form))

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
                (name . "work.org")
                (name . "time-machine.txt")
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

;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L12-L94
;; Fix the silly property list indentation problem.
(use-package lisp-mode
  :defer
  :config
  (defun me//lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (let ((normal-indent (current-column))
          (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond
       ;; car of form doesn't seem to be a symbol, or is a keyword
       ((and (elt state 2)
             (or (not (looking-at "\\sw\\|\\s_"))
                 (looking-at ":")))
        (unless (> (save-excursion (forward-line 1) (point))
                   calculate-lisp-indent-last-sexp)
          (goto-char calculate-lisp-indent-last-sexp)
          (beginning-of-line)
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
        ;; Indent under the list or under the first sexp on the same
        ;; line as calculate-lisp-indent-last-sexp.  Note that first
        ;; thing on that line has to be complete sexp since we are
        ;; inside the innermost containing sexp.
        (backward-prefix-chars)
        (current-column))
       (t
        (let ((function (buffer-substring (point)
                                          (progn (forward-sexp 1) (point))))
              method)
          (setq method (or (function-get (intern-soft function)
                                         'lisp-indent-function)
                           (get (intern-soft function) 'lisp-indent-hook)))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (> (length function) 3)
                          (string-match "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((integerp method)
                 (lisp-indent-specform method state
                                       indent-point normal-indent))
                (method
                 (funcall method indent-point state))))))))

  (defun me//init-emacs-lisp()
    (setq-local lisp-indent-function #'me//lisp-indent-function))
  (add-hook 'emacs-lisp-mode-hook 'me//init-emacs-lisp))

(use-package cc-mode
  :config
  (add-hook 'c-mode-common-hook #'google-set-c-style)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))

(use-package clang-format
  :bind (:map c++-mode-map
         ("C-!" . clang-format-region)))

(use-package web-mode
  :mode ("\\.\\(html\\|htm\\)\\'" "\\.php\\'")
  :config
  (defun me//init-web-mode()
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-enable-current-element-highlight t))
  (add-hook 'web-mode-hook #'me//init-web-mode))

(defun me//init-python()
  "Init python model."
  (python-docstring-mode)
  (sphinx-doc-mode)
  (set (make-local-variable 'comment-inline-offset) 2)
  (set (make-local-variable 'yas-indent-line) 'fixed)
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
         ("H"   . image-transform-fit-to-height)
         ("q"   . quit-window)
         ("Q"   . kill-this-buffer)
         ("r"   . image-transform-set-rotation)
         ("W"   . image-transform-fit-to-width)
         ("SPC" . image-transform-reset)))

(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'"     . ssh-config-mode)
         ("/sshd?_config\\'"      . ssh-config-mode)
         ("/known_hosts\\'"       . ssh-known-hosts-mode)
         ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

(use-package sh-script
  :config
  (set-face-attribute 'sh-heredoc nil
                      :weight 'normal
                      :foreground "yellow2"))

(use-package octave-mode
  :mode "\\.m\\'"
  :init
  (setq octave-comment-char ?%))

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook #'turn-on-flyspell)

  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '("`" "`"))
  (add-to-list 'ispell-skip-region-alist '("^```" . "^```")))

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
  (setq org-modules '(org-bbdb org-bibtex org-clock org-gnus org-tempo
                               org-habit org-table))
  (setq org-export-backends '(ascii beamer html latex md))

  :config
  (setq org-directory (expand-file-name "org" me-emacs-data-private))

  (add-hook 'org-mode-hook #'me//init-org)

  (define-key org-mode-map [remap fill-paragraph] #'org-fill-paragraph)
  (define-key org-mode-map (kbd "C-c [") nil)

  (setq org-adapt-indentation nil
        org-catch-invisible-edits 'smart
        org-hide-emphasis-markers t
        org-hide-macro-markers t
        org-hierarchical-todo-statistics nil
        org-list-description-max-indent 0
        org-provide-todo-statistics      t
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-support-shift-select t
        org-treat-S-cursor-todo-selection-as-state-change nil
        org-use-fast-todo-selection t
        org-use-fast-tag-selection 'auto
        org-image-actual-width nil)

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
  (setq org-habit-graph-column   50
        org-habit-preceding-days 28
        org-habit-following-days 1))

(use-package org-clock
  :config
  (setq org-clock-history-length 32
        org-clock-idle-time 10
        org-clock-in-resume t
        org-clock-into-drawer t
        org-clock-persist t
        org-log-into-drawer t)
  (org-clock-persistence-insinuate))

(use-package org-agenda
  :bind (:map org-agenda-mode-map
         ("F" . org-gcal-fetch))
  :config
  (setq org-agenda-compact-blocks nil
        org-agenda-dim-blocked-tasks t
        org-agenda-files (expand-file-name "orgfile" org-directory)
        org-agenda-include-diary t
        org-agenda-show-all-dates t
        org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
        org-agenda-start-with-log-mode t
        org-agenda-tags-column -130)

  (setq org-agenda-prefix-format
        '((agenda   . " %i %-20:c%?-20t% s")
          (timeline . "  % s")
          (todo     . " %i %-20:T")
          (tags     . " %i %-20:T")
          (search   . " %i %-20:T")))

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

          ("t" "TODO" entry
           (file "todo.org")
           (file "capture/todo.org")
           :empty-lines 1
           :jump-to-captured t)

          ("l" "Log daily" plain
           (file+olp+datetree "time-machine/time-machine.txt")
           "%?"
           :empty-lines 1
           :jump-to-captured t
           :tree-type week)

          ("w" "Work TODO" entry
           (file "work.org")
           (file "capture/todo.org")
           :empty-lines 1
           :jump-to-captured t))))

(use-package ox
  :config
  (setq org-export-global-macros
        '(("tex" . "@@latex:$1@@")
          ("html" . "@@html:$1@@"))))

(use-package ox-extra
  :config
  (ox-extras-activate '(ignore-headlines)))

(use-package ox-html
  :config
  (setq org-html-allow-name-attribute-in-anchors t
        org-html-doctype "html5"
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        org-html-html5-fancy t
        org-html-htmlize-output-type 'css)

  ;; Postamble.
  (setq org-html-postamble t
        org-html-postamble-format
        '(("en" "<a class=\"author\"
           href=\"http://gongzhitaao.org\">%a</a> / <span
           class=\"date\">%T</span><span class=\"creator\">%c</span>"))))

(use-package ox-bibtex)

(use-package ox-latex
  :config
  (setq org-latex-prefer-user-labels t
        org-latex-caption-above nil)
  (setq org-latex-pdf-process
        `(,(concat "PDFLATEX=%latex texi2dvi"
                   " --shell-escape --pdf --tidy --verbose --batch %f")))

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

  (add-to-list 'org-latex-classes
               '("authesis"
                 "\\documentclass{report}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

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
  (add-to-list 'org-beamer-environments-extra '("only" "o" "\\only%a{" "}"))
  (add-to-list 'org-beamer-environments-extra '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))
  (add-to-list 'org-beamer-environments-extra '("action" "A" "\\action%a{" "}")))

;; https://www.reddit.com/r/orgmode/comments/7u2n0h/tip_for_defining_latex_macros_for_use_in_both/
(use-package org-src
  :config
  (add-to-list 'org-src-lang-modes '("latex-macro" . LaTeX))

  (defvar org-babel-default-header-args:latex-macro
    '((:results . "raw")
      (:exports . "results")))

  (defun me//prefix-all-lines (pre body)
    (with-temp-buffer
      (insert body)
      (string-insert-rectangle (point-min) (point-max) pre)
      (buffer-string)))

  (defun org-babel-execute:latex-macro (body _params)
    (concat
     (me//prefix-all-lines "#+LATEX_HEADER: " body)
     "\n#+HTML_HEAD_EXTRA: <div style=\"display: none\"> \\(\n"
     (me//prefix-all-lines "#+HTML_HEAD_EXTRA: " body)
     "\n#+HTML_HEAD_EXTRA: \\)</div>\n")))

;; Helper functions
;; -----------------------------------------------------------------------------

(use-package org-id)

(defun me//clean-up-heading (heading)
  "Clean up HEADING text."
  (replace-regexp-in-string "[^[:alpha:][:digit:][:space:]_-]" ""
                            (downcase heading)))

(defun me//preprocess-heading (heading sep)
  "Preprocessing on HEADING with SEP as separator."
  (let* ((words (split-string (me//clean-up-heading heading)))
         (words2 (subseq words 0 (min 2 (length words)))))
    (replace-regexp-in-string "\\s-+" sep (mapconcat 'identity words2 " "))))

(defun me//org-id-from-heading (heading &optional level sep uniq)
  "Format HEADING to use as custom ID and return it.

LEVEL is provided, the level of heading is prefixed.  This
function normalize HEADING by replacing spaces with SEP.  SEP is
the separator used to glue different parts.  if UNIQ, append a 5
digit hash ID."
  (let ((sep (or sep "-")))
    (concat (when org-id-prefix (format "%s%s" org-id-prefix sep))
            (when level (format "h%d%s" level sep))
            (me//preprocess-heading heading sep)
            (when uniq (concat sep (substring (org-id-new) 0 5))))))

(defun me//org-id-hash (s &optional length)
  "Generate a unique string hash for S, truncated at LENGTH."
  (let ((length (or length 5))
        (hash (secure-hash 'sha1 s)))
    (substring-no-properties hash nil length)))

(defun me/org-custom-id-get (&optional pom create uniq)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.

If POM is nil, refer to the entry at point.  If the entry does
not have an CUSTOM_ID, the function returns nil.  However, when
CREATE is non nil, create a CUSTOM_ID if none is present already.
PREFIX will be passed through to `org-id-new'.  Append a 5-digit
hash if UNIQ is non nil.  In any case, the CUSTOM_ID of the entry
is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (if create nil
                (org-entry-get nil "CUSTOM_ID"))))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (t
        (setq id (me//org-id-from-heading (org-get-heading t t t t)
                                          (org-current-level)
                                          "-"
                                          uniq))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

(defun me/org-custom-id-get-create (&optional force)
  "Create an ID w/o a suffix for the current entry and return it.

If the entry already has an ID, just return it.  With optional
argument FORCE, force the creation of a new ID."
  (interactive "P")
  (when (derived-mode-p 'org-mode)
    (me/org-custom-id-get (point) force)))

(defun me/org-custom-id-get-create-hash (&optional force)
  "Create an ID w/o a suffix for the current entry and return it.

If the entry already has an ID, just return it.  With optional
argument FORCE, force the creation of a new ID."
  (interactive "P")
  (when (derived-mode-p 'org-mode)
    (me/org-custom-id-get (point) force 'uniq)))

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

(defun me/org-custom-id-get-create-hash-all (&optional force)
  "Create custom ID for every heading.  Overwrite current if FORCE."
  (interactive "P")
  (when (derived-mode-p 'org-mode)
    (when force
      (org-entry-put (point) "CUSTOM_ID" nil))
    (let ((me//org-custom-id-get-wrapper
           (lambda () (me/org-custom-id-get (point) force 'uniq))))
      (org-map-entries me//org-custom-id-get-wrapper))))

;; The gcal contains some senstive information, thus in a separate file.
;; -----------------------------------------------------------------------------

;(require 'gcal-conf)

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
  (bind-keys ("M-x"     . helm-M-x)
             ("C-x C-f" . helm-find-files)
             ("M-s a"   . helm-do-ag)
             ("M-s g"   . helm-do-grep-ag)
             ("C-b"     . helm-mini)
             ("M-/"     . helm-dabbrev)
             :map helm-map
             ("<tab>"   . helm-execute-persistent-action)
             ("C-i"     . helm-execute-persistent-action)
             ("C-z"     . helm-select-action)
             ;; :map helm-moccur-map
             ;; ("C-s"     . me//isearch-from-helm-occur)
             )

  (helm-autoresize-mode t)
  (helm-adaptive-mode 1)

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
        helm-buffers-fuzzy-matching t)
  (set-face-attribute 'helm-buffer-directory nil :inherit 'dired-directory))

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

(use-package helm-flyspell)
(use-package flyspell
  :after helm-flyspell
  :bind (:map flyspell-mode-map
         ("C-;" . helm-flyspell-correct)))

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
   (whitespace-mode nil whitespace)
   (eldoc-mode nil t)))

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

;; ace-window
;; -----------------------------------------------------------------------------

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
  (setq deft-auto-save-interval 0
        deft-default-extension "org"
        deft-directory (expand-file-name "notes" me-emacs-data-private)
        deft-recursive t
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t)
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

;; Temporarily disable since msmtp does not support XOAUTH2.  Reading mail is
;; fine, but replying mail is not working.
;(use-package mail-conf)

;; Contacts
;; -----------------------------------------------------------------------------

(use-package bbdb
  :config
  (bbdb-initialize 'message 'anniv 'mu4e)
  (setq bbdb-allow-duplicates t
        bbdb-complete-mail-allow-cycling t
        bbdb-file (expand-file-name "contacts.bbdb.gz" me-emacs-data-private)
        bbdb-mail-user-agent 'message-user-agent
        bbdb-mua-pop-up nil
        bbdb-message-all-addresses t)
  (add-hook 'message-setup-hook 'bbdb-mail-aliases))

;; =============================================================================
;; Bibliography manager
;; =============================================================================

(defvar me-bib (expand-file-name "bibliography" me-emacs-data-public)
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
         ([remap fill-paragraph]     . bibtex-fill-entry)
         ([remap bibtex-clean-entry] . org-ref-clean-bibtex-entry)
         ("C-c C-v"                  . bibtex-validate)
         ("<backtab>"                . me/bibtex-find-text-begin)
         ("M-<down>"                 . bibtex-end-of-entry)
         ("M-<up>"                   . bibtex-beginning-of-entry))
  :config
  (add-hook 'bibtex-mode-hook #'me//init-bibtex)

  (add-to-list 'bibtex-entry-format 'unify-case)
  (add-to-list 'bibtex-entry-format 'sort-fields)
  (add-to-list 'bibtex-entry-format 'whitespace)
  (add-to-list 'bibtex-entry-format 'last-comma)

  (setq bibtex-align-at-equal-sign t
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-titleword-length nil
        bibtex-autokey-titleword-separator "_"
        bibtex-autokey-titlewords 1
        bibtex-autokey-titlewords-stretch 0
        bibtex-autokey-year-length 4
        bibtex-autokey-year-title-separator "-"
        bibtex-completion-bibliography me-bib-files
        bibtex-completion-library-path me-bib-pdfs
        bibtex-completion-notes-extension ".org"
        bibtex-completion-notes-path me-bib-notes
        bibtex-completion-notes-symbol "✔"
        bibtex-completion-pdf-symbol "⚐"
        bibtex-dialect 'biblatex
        bibtex-maintain-sorted-entries t
        bibtex-text-indentation 20))

;; reftex
;; -----------------------------------------------------------------------------

(use-package reftex
  :delight
  :config
  (add-hook 'TeX-mode-hook #'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t
        reftex-ref-style-default-list '("Cleveref" "Hyperref" "Fancyref")
        reftex-default-bibliography me-bib-files))

;; turn on flyspell on latex
;; -----------------------------------------------------------------------------

(defun me//init-tex()
  "Init tex mode."
  (flyspell-mode))

(use-package tex-mode
  :config
  (add-hook 'TeX-mode-hook #'me//init-tex))

;; org-ref
;; -----------------------------------------------------------------------------

(defun me//org-ref-notes-function (thekey)
  "Return the name of the note file by THEKEY."
  (bibtex-completion-edit-notes
   (list (car (org-ref-get-bibtex-key-and-file thekey)))))

(defun me//org-ref-add-timestamp ()
  "Add a timestamp field to a bibtex entry."
  (interactive)
  (bibtex-beginning-of-entry)
  (bibtex-set-field "timestamp" (format-time-string "%Y%m%dT%H%M")))

(use-package org-ref
  :demand
  :bind (:map org-mode-map
         ("C-c ]" . org-ref-insert-ref-link))
  :init
  (setq org-ref-cite-color (me//colir-blend "dark sea green" "grey90" 0.4)
        org-ref-default-bibliography me-bib-files
        org-ref-notes-directory me-bib-notes
        org-ref-pdf-directory me-bib-pdfs
        org-ref-ref-color (me//colir-blend "goldenrod" "grey90" 0.4)
        org-ref-show-citation-on-enter nil)
  :config
  (setq org-ref-notes-function #'me//org-ref-notes-function)

  (dolist (func '(org-ref-downcase-bibtex-entry me//org-ref-add-timestamp))
    (add-hook 'org-ref-clean-bibtex-entry-hook func))

  (define-key org-ref-cite-keymap (kbd "M-<right>") #'org-ref-next-key)
  (define-key org-ref-cite-keymap (kbd "M-<left>") #'org-ref-previous-key)
  (define-key org-ref-cite-keymap (kbd "C-<left>") nil)
  (define-key org-ref-cite-keymap (kbd "C-<right>") nil)

  (setq bibtex-completion-dispvlay-formats
        `((article       . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${journal:20}  ${keywords:40}")
          (inbook        . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${chapter:20}  ${keywords:40}")
          (incollection  . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${booktitle:20}  ${keywords:40}")
          (inproceedings . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${booktitle:20}  ${keywords:40}")
          (t             . ,(format "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  %s  ${keywords:40}" (make-string 20 ? )))))
  (setq bibtex-completion-additional-search-fields '(keywords journal booktitle)))

(defun me/cleanup-bibtex-file (beg end)
  "Cleanup entries in region BEG, END."
  (interactive "r")
  (bibtex-map-entries (lambda (_key _beg _end)
                        (org-ref-clean-bibtex-entry))))

;; =============================================================================
;; Working with PDF
;; =============================================================================

(defun me//init-pdf-view-mode ()
  "Initailize pdf view mode."
  (setq-local display-line-numbers nil))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)

  (defun me/pdf-set-last-viewed-bookmark ()
    (interactive)
    (when (eq major-mode 'pdf-view-mode)
      (bookmark-set (me//pdf-get-bookmark-name))))

  (defun me//pdf-jump-last-viewed-bookmark ()
    (bookmark-set "fake")
    (let ((bmk (me//pdf-get-bookmark-name)))
      (when (me//pdf-has-last-viewed-bookmark bmk)
        (bookmark-jump bmk))))

  (defun me//pdf-has-last-viewed-bookmark (bmk)
    (assoc bmk bookmark-alist))

  (defun me//pdf-get-bookmark-name ()
    (concat "PDF-last-viewed: " (buffer-file-name)))

  (defun me//pdf-set-all-last-viewed-bookmarks ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf (me/pdf-set-last-viewed-bookmark))))

  (add-hook 'kill-buffer-hook 'me/pdf-set-last-viewed-bookmark)
  (add-hook 'pdf-view-mode-hook 'me//pdf-jump-last-viewed-bookmark)
  (add-hook 'pdf-view-mode-hook 'me//init-pdf-view-mode)

  ;; As `save-place-mode' does
  (unless noninteractive
    (add-hook 'kill-emacs-hook #'me//pdf-set-all-last-viewed-bookmarks)))

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
         ("<delete>"   . pdf-view-scroll-up-or-next-page)
         ("<down>"     . me/pdf-view-next-few-lines)
         ("<left>"     . pdf-view-previous-page-command)
         ("<right>"    . pdf-view-next-page-command)
         ("<up>"       . me/pdf-view-prev-few-lines)
         ("<PageUp>"   . pdf-view-scroll-down-or-previous-page)
         ("<PageDown>" . pdf-view-scroll-up-or-next-page)
         ("b"          . helm-mini)
         ("c"          . me/org-ref-open-entry)
         ("d"          . pdf-view-scroll-up-or-next-page)
         ("e"          . pdf-view-scroll-down-or-previous-page)
         ("g"          . pdf-view-goto-page)
         ("j"          . pdf-view-scroll-up-or-next-page)
         ("k"          . pdf-view-scroll-down-or-previous-page)
         ("n"          . me/org-ref-open-note)
         ("z"          . delete-other-windows))
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

;; (define-pdf-cache-function pagelabels)
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
  :delight
  :bind ("C-c u" . undo-tree-visualize)
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,me-emacs-tmp)))

  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz")))

  (global-undo-tree-mode))

;; Evil
;; -----------------------------------------------------------------------------

(use-package view)
(use-package evil
  :config
  (evil-mode 1)

  (mapc (lambda (x) (evil-set-initial-state x 'emacs))
        '(dired-mode image-dired-thumbnail-mode image-mode
                     diary-mode org-src-mode mu4e-compose-mode calendar-mode
                     org-capture-mode))

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
             ("<down>"      . evil-next-visual-line)
             ("<up>"        . evil-previous-visual-line)
             ("C-b"         . helm-mini)
             ("C-e"         . move-end-of-line)
             ("C-z"         . delete-other-windows)
             ("C-v"         . golden-ratio-scroll-screen-up))

  (setq cursor-type 'box)

  (defun me//propertize-evil-tag (str fg bg)
    "Make the evil state notifier pertty.

Propertize STR with foreground FG and background BG color."
    (propertize str 'face
                `((:box (:line-width 6 :color ,bg)
                   :background ,bg
                   :foreground ,fg))))

  (let ((state-color-list '((:state "insert" :color "chartreuse3" :tag " <I> ")
                            (:state "emacs" :color "SkyBlue2" :tag " <E> ")
                            (:state "normal" :color "DarkGoldenrod2" :tag " <N> ")
                            (:state "visual" :color "gray" :tag " <V> ")
                            (:state "motion" :color "plum3" :tag " <M> ")))
        (dim 0.5))
    (dolist (elm state-color-list)
      (set (intern-soft (concat "evil-" (plist-get elm :state) "-state-cursor"))
           (plist-get elm :color))
      (set (intern-soft (concat "evil-" (plist-get elm :state) "-state-tag"))
           (me//propertize-evil-tag (plist-get elm :tag)
                                    "gray80"
                                    (me//colir-blend (plist-get elm :color)
                                                     "#21252B" dim)))))

  (setq evil-move-beyond-eol t)

  (evil-make-overriding-map help-mode-map 'motion)

  ;; Use emacs state instead of insert mode
  (defalias 'evil-insert-state 'evil-emacs-state)
  (setq-default evil-shift-width 2))

;; =============================================================================
;; Other stuff
;; =============================================================================

(use-package dm)

;; =============================================================================
;; Theme
;; =============================================================================

;; (use-package adhoc-theme)
;; (load-theme 'adhoc t)

(use-package nord-theme)
(load-theme 'nord t)

;; =============================================================================
;; Now start the server
;; =============================================================================

(use-package server
  :config
  (unless (server-running-p) (server-start)))

;;; init.el ends here
