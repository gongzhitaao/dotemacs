;;; init.el --- Yet another Emacs config  -*- lexical-binding: t; -*-
;; Time-stamp: <2024-11-12 16:34:47 gongzhitaao>

;;; Commentary:
;; me/xxx: mostly interactive functions, may be executed with M-x or keys
;; me//xxx: internal helper functions, not called directly by user
;; me-xxx: custom variables

;;; Code:

;; =============================================================================
;; Variables
;; =============================================================================

(defconst me-emacs-config-dir "~/.config/emacs" "My Emacs directory.")
(defconst me-emacs-data-dir "~/.local/share/emacs" "Emacs data directory.")
(defconst me-emacs-cache-dir "~/.cache/emacs" "Directory for temporary files.")
(unless (file-exists-p me-emacs-cache-dir)
  (mkdir me-emacs-cache-dir))

(setq user-emacs-directory (file-name-as-directory me-emacs-cache-dir))

(defconst me-keylog (file-name-concat me-emacs-data-dir "keylog")
  "The file path that Logs every key stroke in my EMACS.")
(unless (file-exists-p me-keylog)
  (mkdir me-keylog))

(setq custom-file (file-name-concat me-emacs-config-dir "custom.el"))

;; (if (file-exists-p custom-file)
;;     (load custom-file))

;; The ~/.emacs.d/site-lisp contains some configs that don't fit in here,
;; because it is site-specific or it contains sensitive information.  These
;; configs will not go into the public git repo.
(add-to-list 'load-path (file-name-concat me-emacs-config-dir "site-lisp"))

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
;;
;;(package-initialize)

(setq straight-base-dir me-emacs-cache-dir)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package
  :config
  (setq use-package-compute-statistics t))

(use-package straight
  :custom
  (straight-use-package-by-default t)
  (straight-built-in-pseudo-packages
   '(
     ;; keep-sorted begin
     abbrev
     comp
     composite
     dired
     emacs
     esh-mode
     files
     google-coding-style
     google-java-format
     google-pyformat
     google3-build-mode
     google3-eglot
     helm-adaptive
     helm-bookmark
     helm-buffers
     helm-config
     helm-elisp
     helm-files
     helm-for-files
     helm-locate
     helm-mode
     ibuf-ext
     image-mode
     indent
     lisp-mode
     ls-lisp
     mail-conf
     message
     oc
     org-agenda
     org-capture
     org-clock
     org-habit
     org-id
     org-refile
     org-src
     ox
     ox-beamer
     ox-bibtex
     ox-extra
     ox-html
     ox-latex
     pdf-view
     project
     python
     rfn-eshadow
     simple
     solar
     tramp-cache
     uniquify
     xref
     vc-hooks
     ;; keep-sorted end
     )))

(use-package delight)
(use-package bind-key)

(setq load-prefer-newer t)

;; =============================================================================
;; Key binding
;; =============================================================================

(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)
(global-set-key (kbd "C-;") #'comment-or-uncomment-region)
(global-set-key (kbd "C-J") #'me/join-next-line)
(global-set-key (kbd "C-z") #'delete-other-windows)

(global-set-key (kbd "M-<delete>") #'kill-word)

;; FN keys
;; -----------------------------------------------------------------------------

(global-set-key (kbd "<f6>") #'calendar)
;; f8 -- deft
;; f10 -- menu

;; Remaping
;; -----------------------------------------------------------------------------

(global-set-key [remap copy-rectangle-as-kill] #'me/copy-rectangle-as-kill)
(global-set-key [remap isearch-backward]       #'isearch-backward-regexp)
(global-set-key [remap isearch-forward]        #'isearch-forward-regexp)
(global-set-key [remap list-buffers]           #'ibuffer)
(global-set-key [remap switch-to-buffer]       #'helm-mini)
(global-set-key [remap yank-pop]               #'helm-show-kill-ring)

;; C-c user key
;; -----------------------------------------------------------------------------

;; C-c b -- helm-bibtex
;; C-c c -- helm-flycheck

(use-package drag-stuff
  :bind ("C-c d" . drag-stuff-mode)
  :delight (drag-stuff-mode " ")
  :config (drag-stuff-define-keys))

(bind-keys :prefix-map me-editing-command-map
           :prefix "C-c e"
           ("2"   . me/double-space-after-dot)
           ("c"   . set-buffer-file-coding-system)
           ("d"   . delete-duplicate-lines)
           ("M-w" . me/copy-region-escaped)
           ("s l" . sort-lines)
           ("s L" . sort-fields)
           ("s s" . me/sort-symbols)
           ("s w" . me/sort-words)
           ("S"   . me/sudo-edit)
           ("t"   . me/gnome-terminal)
           ("v"   . add-file-local-variable)
           ("V"   . add-file-local-variable-prop-line))

(use-package multiple-cursors
  :bind (:prefix-map me-multiple-cursors-command-map
         :prefix "C-c m"
         ("C-a" . mc/edit-beginnings-of-lines)
         ("C-e" . mc/edit-ends-of-lines)
         ("a"   . mc/mark-all-like-this-dwim)
         ("l"   . mc/edit-lines)
         ("i n" . mc/insert-numbers)
         ("i l" . mc/insert-letters))
  :custom
  (mc/list-file (file-name-concat me-emacs-cache-dir "mc-lists.el")))

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
;; M-k kill-sentence
;; M-l downcase-word
;; M-m back-to-indentation
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

(global-set-key (kbd "M-/") #'helm-dabbrev)
(global-set-key (kbd "M-<down>")  #'sp-down-sexp)
(global-set-key (kbd "M-<left>")  #'sp-backward-sexp)
(global-set-key (kbd "M-<right>") #'sp-forward-sexp)
(global-set-key (kbd "M-<up>")    #'sp-up-sexp)

;;; M-s search

;; M-s g helm-ag
;; M-s h highlight-xxx
;; M-s q vr/query-replace
;; M-s s helm-swoop

(use-package repeat)

;; =============================================================================
;; General helpers
;; =============================================================================

(defun me--ad-with-region-or-line (orig-fun &rest args)
  "Advice added around ORIG-FUN with ARGS to operate on line or region."
  (if (region-active-p)
      (progn
        (apply orig-fun args)
        (message "Region action"))
    (let ((bol (+ (line-beginning-position) (current-indentation)))
          (eol (line-end-position)))
      (save-excursion
        (push-mark bol)
        (goto-char eol)
        (apply orig-fun (list bol eol (nth 2 args))))
      (message "Line action"))))

(defun me-with-region-or-line (func &optional remove)
  "Call FUNC on region if region is active, otherwise line.
If not REMOVE, add advice to FUNC, i.e., when called with no
active region, call FUNC on current line.  Otherwise remove
advice."
  (if remove
      (advice-remove func #'me--ad-with-region-or-line)
    (advice-add func :around #'me--ad-with-region-or-line)))

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

(defun me/copy-rectangle-as-kill (start end)
  "Do `copy-rectangle-as-kill' in START and END, also save to system clipboard."
  (interactive "r")
  (call-interactively #'copy-rectangle-as-kill)
  (with-temp-buffer
    (yank-rectangle)
    (delete-trailing-whitespace)
    (kill-new (buffer-string))))

(defun me/make-username (stem)
  "Make a random username from STEM.

It is used to make an arbitrary but determinsitc username, e.g.,
for a website.  I have Catch-all configured for one of my
domains, so I can use username@mydomain as the email.

The username needs to include two parts:
- A unique handle about the website that I can easily recall.
- A random string that prevents spam guessing my pattern.
  Ideally this string is arbitrary but deterministic so that I
  can regenerate later.
"
  (interactive "sUsername stem: ")
  (let* ((suffix
         (downcase
          (substring (base32-encode (secure-hash 'sha256 stem nil nil t))
                     0 5)))
         (username (format "%s.%s" stem suffix)))
    (message "Username: %s copied" username)
    (kill-new username)))

;; =============================================================================
;; Appearance
;; =============================================================================

(blink-cursor-mode 0)
(mouse-avoidance-mode 'animate)

(setq x-underline-at-descent-line t)

(scroll-bar-mode 0)
(setq scroll-margin 0
      scroll-preserve-screen-position nil)

(setq visible-bell t)
(setq inhibit-startup-message t
      resize-mini-windows t)

(defun me//set-title-bar()
  "Update title bar."
  (let* ((date (format-time-string "%Y-%m-%d %a"))
         (filepath (or buffer-file-name dired-directory (buffer-name)))
         (hostname (concat "@" (or (file-remote-p filepath 'host) "localhost")))
         (separator (make-string 8 ? )))
    (string-join `(,date ,hostname ,filepath) separator)))
(setq frame-title-format '((:eval (me//set-title-bar))))

;; Show the search result count.
(use-package anzu
  :delight
  :custom (anzu-search-threshold 1000)
  :config (global-anzu-mode))

;; Center the editing content.
(use-package writeroom-mode
  :custom
  (writeroom-fullscreen-effect nil)
  (writeroom-major-modes
   '( prog-mode conf-mode dired-mode Info-mode calendar-mode text-mode
      org-agenda-mode bibtex-mode bookmark-bmenu-mode
      LilyPond-mode notmuch-show-mode protobuf-mode))
  (writeroom-major-modes-exceptions '(web-mode))
  (writeroom-maximize-window nil)
  (writeroom-mode-line t)
  (writeroom-use-derived-modes t)
  (writeroom-width 100)

  :config
  (delete 'writeroom-set-menu-bar-lines writeroom-global-effects)
  (global-writeroom-mode))

;; Select interesting regions
(use-package expand-region
  :bind ("C-=" . #'er/expand-region))

(use-package smartparens
  :delight
  :config
  (require 'smartparens-config)

  (bind-keys
   ("M-S-<backspace>" . sp-backward-unwrap-sexp)
   ("M-S-<delete>"    . sp-unwrap-sexp)
   :prefix-map smartparens-mode-map
   :prefix "C-c s"
   ("<backspace>"   . sp-splice-sexp-killing-backward)
   ("<delete>"      . sp-splice-sexp-killing-forward)
   ("<left>"        . sp-backward-slurp-sexp)
   ("<right>"       . sp-forward-slurp-sexp)
   ("a"             . sp-beginning-of-sexp)
   ("b"             . sp-backward-sexp)
   ("C-<left>"      . sp-forward-barf-sexp)
   ("C-<right>"     . sp-backward-barf-sexp)
   ("D"             . sp-backward-down-sexp)
   ("d"             . sp-down-sexp)
   ("e"             . sp-end-of-sexp)
   ("E"             . sp-up-sexp)
   ("f"             . sp-forward-sexp)
   ("k"             . sp-kill-sexp)
   ("K"             . sp-backward-kill-sexp)
   ("n"             . sp-next-sexp)
   ("p"             . sp-previous-sexp)
   ("r"             . sp-rewrap-sexp)
   ("s"             . sp-split-sexp)
   ("t"             . sp-transpose-sexp)
   ("w"             . sp-copy-sexp))

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

;; fonts
;; -----------------------------------------------------------------------------

(defun me//high-resolution-p ()
  "Return TRUE if high resolution."
  (let ((width (display-pixel-width)))
    (not (not (memq width '(1920 1998 2048 2560))))))

(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :family "Iosevka Term SS09"
                      :height 160)

  (let ((size (if (me//high-resolution-p) 22 44)))
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font
       (frame-parameter nil 'font) charset (font-spec
                                            :family "Sarasa Mono TC"
                                            :size size)))

    (set-fontset-font "fontset-default"
                      (cons (decode-char 'ucs #xF000)
                            (decode-char 'ucs #xF890))
                      (font-spec :family "Font Awesome 6 Free"
                                 :size size))))

(set-face-attribute 'fixed-pitch nil :height 110)

(use-package ligature
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->"
     "<!--" "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>"
     "<====>" "<!---" "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
     ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>"
     "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; modeline
;; -----------------------------------------------------------------------------

(use-package time
  :custom
  (display-time-24hr-format t)
  (display-time-day-and-date nil)
  (display-time-default-load-average nil)

  :config
  (display-time))

(use-package simple
  :custom
  (indent-tabs-mode nil)
  (blink-matching-paren nil)

  :config
  (column-number-mode)
  (size-indication-mode))

(use-package rfn-eshadow
  :config
  (file-name-shadow-mode))

(use-package uniquify
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-separator "/"))

;; =============================================================================
;; General editing
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

(use-package files
  :custom
  (backup-by-copying t)
  (confirm-kill-emacs 'yes-or-no-p)
  (delete-old-versions t)
  (kept-new-versions 30)
  (kept-old-versions 30)
  (small-temporary-file-directory "/tmp/")
  (version-control t)
  (view-read-only t)

  :config
  (let ((backup-dir (file-name-concat me-emacs-cache-dir "backup")))
    (unless (file-exists-p backup-dir)
      (mkdir backup-dir))
    (setopt backup-directory-alist `(("." . ,backup-dir)))
    (me//cleanup-old-files backup-dir 7))

  ;; Backup buffer before each save.
  (add-hook 'before-save-hook #'me//force-backup-of-buffer))

(use-package vc-hooks
  :custom
  (vc-make-backup-files t))

;; startup.el
(setopt auto-save-list-file-prefix
        (file-name-concat me-emacs-cache-dir "auto-save-list/saves-"))

(use-package select
  :custom
  (select-enable-clipboard t))

;; indent.el
(setopt tab-always-indent t
        standard-indent 2
        tab-stop-list (number-sequence 2 120 2))

(use-package sort
  :custom
  (sort-fold-case t))

(setopt
 delete-by-moving-to-trash t
 fill-column 80
 history-delete-duplicates t
 tab-width 4
 truncate-lines t)

(use-package comp
  :custom
  (native-comp-async-report-warnings-errors nil))

(use-package delsel
  :config
  (delete-selection-mode))

(add-hook 'before-save-hook 'time-stamp)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)

(use-package subword
  :config
  (global-subword-mode 1))

;; Whitespace-mode need to be called before highlight-indent-guides, otherwise
;; no guides are shown.
;; (use-package highlight-indent-guides
;;   :delight
;;   :hook ((python-mode) . highlight-indent-guides-mode)
;;   :config
;;   (setopt highlight-indent-guides-auto-character-face-perc 10
;;           highlight-indent-guides-auto-top-character-face-perc 30
;;           highlight-indent-guides-character ?⎜
;;           highlight-indent-guides-method 'character
;;           highlight-indent-guides-responsive 'top))

(use-package indent-bars
  :hook (python-mode . indent-bars-mode)
  :custom
  (indent-bars-width-frac 0.2)
  (indent-bars-no-descend-lists t))

(use-package whitespace
  :delight

  :custom
  (whitespace-line-column nil)
  (whitespace-style '( empty face indentation lines-tail
                       space-after-tab space-before-tab
                       tabs trailing))

  :hook (((prog-mode org-mode) . whitespace-mode)
         (before-save . whitespace-cleanup)))

(use-package volatile-highlights
  :delight
  :config
  (volatile-highlights-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package wgrep
  :config (setopt wgrep-auto-save-buffer t))

(use-package golden-ratio-scroll-screen
  :config
  (setopt golden-ratio-scroll-highlight-flag nil)
  (global-set-key [remap scroll-down-command] #'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] #'golden-ratio-scroll-screen-up))

(use-package visual-regexp
  :bind ("M-s q" . vr/query-replace))

(use-package helm-flyspell)
(use-package flyspell
  :delight
  :hook ((prog-mode . flyspell-prog-mode)
         ((tex-mode org-mode) . flyspell-mode))
  :bind (:map flyspell-mode-map
              ("C-;" . comment-or-uncomment-region)
              ("s-;" . helm-flyspell-correct)))

;; -----------------------------------------------------------------------------
;; UNDO
;;
;; Vundo supports navigating undo as a tree structure, and works well with
;; undo/redo history loaded by undo-fu-session, although there are no
;; inter-dependencies as both packages operate on Emacs built-in undo.
;; -----------------------------------------------------------------------------

;; Saves the undo history across sessions.
(use-package undo-fu-session
  :custom
  (undo-fu-session-directory (file-name-concat me-emacs-cache-dir "undo"))
  :config
  (undo-fu-session-global-mode))

;; Navigates undo history in a tree style.
(use-package vundo
  :bind (("C-x u" . vundo)
         :map vundo-mode-map
         ("l"       . #'vundo-forward)
         ("<right>" . #'vundo-forward)
         ("h"       . #'vundo-backward)
         ("<left>"  . #'vundo-backward)
         ("j"       . #'vundo-next)
         ("<down>"  . #'vundo-next)
         ("k"       . #'vundo-previous)
         ("<up>"    . #'vundo-previous)
         ("<home>"  . #'vundo-stem-root)
         ("<end>"   . #'vundo-stem-end)
         ("q"       . #'vundo-quit)
         ("C-g"     . #'vundo-quit)
         ("RET"     . #'vundo-confirm))
  :custom-face
  (vundo-highlight ((t (:inverse-video t)))))

;; Searching
;; -----------------------------------------------------------------------------

(use-package helm-ag
  :after helm
  :bind ("M-s a" . helm-ag))

(use-package helm-swoop
  :after helm
  :bind (("M-s s" . helm-swoop)
         :map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch))
  :config
  (setq helm-multi-swoop-edit-save t))

(use-package imenu-list
  :config
  (setopt imenu-list-position 'left))

(use-package imenu
  :custom (imenu-sort-function #'imenu--sort-by-name))

;; (use-package projectile
;;   :bind (:map projectile-mode-map
;;               ("C-c p" . projectile-command-map))
;;   :init
;;   (projectile-mode))
;; (use-package helm-projectile)

;; Completion
;; -----------------------------------------------------------------------------

(use-package abbrev
  :custom
  (abbrev-file-name (file-name-concat me-emacs-data-dir "abbrev_defs")))

(use-package yasnippet
  :delight yas-minor-mode
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :config
  (setopt yas-snippet-dirs `(,(file-name-concat me-emacs-data-dir "snippets")))
  (yas-reload-all))

(use-package company
  :delight
  :custom
  (company-format-margin-function #'company-text-icons-margin)
  (company-insertion-on-trigger-p)

  :hook (prog-mode . company-mode)

  :config
  (setopt company-idle-delay nil)
  (bind-keys :map prog-mode-map
             ("<tab>" . company-indent-or-complete-common)))

;; Encoding
;; -----------------------------------------------------------------------------

(let ((my-prefer-coding-system
       '(cp950 gb2312 cp936 gb18030 utf-16 utf-8)))
  (dolist (c my-prefer-coding-system)
    (prefer-coding-system c)))

;; =============================================================================
;; Bookkeeping
;; =============================================================================

(defun me//make-file-precious-when-remote ()
  "Set FILE-PRECIOUS-FLAG for remote files."
  (when (file-remote-p default-directory)
    (set (make-local-variable 'file-precious-flag) t)))
(add-hook 'find-file-hook #'me//make-file-precious-when-remote)

;; Save recent visited file list.
(use-package recentf
  :custom
  (recentf-max-saved-items 50)
  (recentf-save-file (file-name-concat me-emacs-cache-dir "recentf"))

  :config
  (let ((ignores `(,(file-name-concat me-emacs-cache-dir ".*") "~/.mail/.*")))
    (mapc (lambda (x) (add-to-list 'recentf-exclude x)) ignores))

  (recentf-mode))

;; Save minibuffer history
(use-package savehist
  :custom
  (savehist-additional-variables '(search ring regexp-search-ring))
  (savehist-file (file-name-concat me-emacs-cache-dir "savehist"))

  :config
  (savehist-mode))

;; Save file editing positions across sessions.
(use-package saveplace
  :custom (save-place-file (file-name-concat me-emacs-cache-dir "saveplace"))
  :config (save-place-mode))

;; Save *scratch* buffer content to files.
(use-package persistent-scratch
  :custom

  (persistent-scratch-backup-directory
   (file-name-concat me-emacs-cache-dir "scratch.d"))
  (persistent-scratch-save-file
   (file-name-concat me-emacs-cache-dir "scratch.d/scratch"))

  :config
  (setopt persistent-scratch-backup-filter
           (persistent-scratch-keep-backups-not-older-than (days-to-time 30)))
  (persistent-scratch-setup-default))

(use-package bookmark
  :custom
  (bookmark-default-file (file-name-concat me-emacs-data-dir "bookmarks"))
  :config
  (defun me//init-bookmark-bmenu ()
    (set (make-local-variable 'writeroom-width) 150)
    (set-face-bold 'bookmark-menu-bookmark nil)
    (hl-line-mode))

  (add-hook 'bookmark-bmenu-mode-hook #'me//init-bookmark-bmenu))

;; =============================================================================
;; General utilities
;; =============================================================================

;; (use-package go-translate
;;   :custom
;;   (gt-langs '(en fr))
;;   (gt-default-translator
;;    (gt-translator
;;     :engines (gt-google-engine)
;;     :render  (gt-buffer-render))))

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package esh-mode
  :config
  (setopt eshell-directory-name (file-name-concat me-emacs-cache-dir "eshell")))

(use-package tramp
  :load-path "~/.cache/emacs/straight/build/tramp"
  :custom
  (tramp-backup-directory-alist nil))

(use-package tramp-cache
  :load-path "~/.cache/emacs/straight/build/tramp"
  :custom
  (tramp-persistency-file-name (file-name-concat me-emacs-cache-dir "tramp")))

(use-package flycheck
  :load-path "~/.cache/emacs/straight/build/flycheck")

(use-package eglot
  :load-path "~/.cache/emacs/straight/build/eglot"
  :config
  (set-face-attribute 'eglot-highlight-symbol-face nil
                      :inherit 'modus-themes-nuanced-red))

;; Dired
;; -----------------------------------------------------------------------------

(use-package ffap)

(defun me//init-dired ()
  "Initialize Dired mode."
  (hl-line-mode))

(use-package dired
  :bind (:map dired-mode-map
              ("f" . find-file-literally-at-point))
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-listing-switches "-alh")

  :config
  (add-hook 'dired-mode-hook #'me//init-dired)

  (defface me-dired-dim-0 '((t (:foreground "gray50")))
    "Dimmed face."
    :group 'me-dired)

  (defface me-dired-dim-1 '((t (:foreground "gray70")))
    "Dimmed face."
    :group 'me-dired)

  (defface me-dired-executable
    '((((class color) (min-colors 8)) :background "#b3fabf"))
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

(use-package ls-lisp
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-use-string-collate nil)
  (ls-lisp-ignore-case nil))

(use-package async
  :delight dired-async-mode
  :config (dired-async-mode))

;; Calendar
;; -----------------------------------------------------------------------------

(use-package solar
  :custom
  (calendar-latitude 32.6)
  (calendar-longitude -85.5))

(use-package calendar
  :custom
  (calendar-chinese-all-holidays-flag t)
  (calendar-intermonth-header
   (propertize "Wk" 'font-lock-face '(:foreground "gray50" :bold t)))
  (calendar-intermonth-text
   '(propertize
     (format "%2d"
             (car (calendar-iso-from-absolute
                   (calendar-absolute-from-gregorian
                    (list month day year)))))
     'font-lock-face '(:foreground "gray70")))
  (calendar-mark-diary-entries-flag t)
  (calendar-mark-holidays-flag t)
  (calendar-view-diary-initially-flag nil)
  (calendar-view-holidays-initially-flag nil)
  (calendar-week-start-day 1)
  (diary-file (file-name-concat me-emacs-data-dir "time-machine/diary"))

  :config
  (calendar-set-date-style 'iso))

(use-package cal-china-x
  :custom-face
  (cal-china-x-important-holiday-face ((t :background "#ff9580")))

  :config
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)

  (setopt holiday-other-holidays
          (append '((holiday-fixed 3  8  "婦女節")
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
                    (holiday-lunar 9 9   "重陽節"))
                  holiday-other-holidays))

  (setopt calendar-holidays
          (append cal-china-x-chinese-holidays
                  cal-china-x-general-holidays
                  holiday-other-holidays))

  ;; Prevents cal-china-x from chaning the date format in diary.
  (setopt calendar-date-display-form calendar-iso-date-display-form))

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
                (name . "\\.lua")
                (mode . google3-build-mode)
                (mode . go-mode)))
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
                (name . "^\\*scratch\\*$")))
           ("Image"
            (or (mode . image-mode)))
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

(use-package google-coding-style
  :hook (c-mode-common . google-set-c-style))

(use-package cc-mode
  :bind (:map c++-mode-map
              ("C-!" . clang-format))
  :mode "\\.h\\'")

(use-package clang-format)

(use-package web-mode
  :mode ("\\.\\(html\\|htm\\)\\'" "\\.php\\'")
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2))

(use-package sphinx-doc
  :delight)
(use-package python-isort)

(defun me//isort-region-or-buffer (&optional beg end)
  "Sort region in BEG and END if active, whole buffer otherwise."
  (interactive "r")
  (if (region-active-p)
      (progn
        (python-isort-region beg end)
        (deactivate-mark)
        (message "Region sorted."))
    (python-isort-buffer)
    (message "All imports sorted.")))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :bind (:map python-mode-map
              ("C-!" . #'google-pyformat)
              ("C-c C-s" . #'me//isort-region-or-buffer))
  :custom
  (python-indent-offset 2)
  :config
  (defun me//init-python()
    "Init python model."
    (sphinx-doc-mode)
    (setq-local comment-inline-offset 2)
    (setq-local yas-indent-line 'fixed)
    (setq-local comment-column 0))

  (add-hook 'python-mode-hook #'me//init-python))

(use-package json-mode)

(use-package js
  :custom (js-indent-level 2))

(use-package js2-mode
  :mode "\\.js\\'"
  :delight (js2-mode "JS2")
  :bind (:map js2-mode-map
              ("C-!" . clang-format-region))
  :custom
  (js2-basic-offset 2)
  (js2-include-node-externs t)
  (js2-include-browser-externs t)

  :config
  (setq-default js2-additional-externs
                '("$"
                  "KeyEvent"
                  "google"
                  "sessionStorage"
                  "URLSearchParams"
                  "URL"
                  "ResizeObserver")))

(use-package image-mode
  :bind (:map image-mode-map
              ("H"   . image-transform-fit-to-window)
              ("q"   . quit-window)
              ("Q"   . kill-this-buffer)
              ("r"   . image-transform-set-rotation)
              ("W"   . image-transform-fit-to-window)
              ("SPC" . image-transform-reset-to-initial)))

(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'"     . ssh-config-mode)
         ("/sshd?_config\\'"      . ssh-config-mode)
         ("/known_hosts\\'"       . ssh-known-hosts-mode)
         ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

(use-package octave
  :mode "\\.m\\'"
  :custom (octave-comment-char ?%))

(use-package markdown-mode
  :config
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '("`" "`"))
  (add-to-list 'ispell-skip-region-alist '("^```" . "^```"))
  (setq-default markdown-hide-urls t))

(use-package typescript-mode
  :bind (:map typescript-mode-map
              ("C-!" . clang-format))
  :custom
  (typescript-indent-level 2))

(use-package cython-mode)
(use-package jsonnet-mode)
(use-package ncl-mode)
(use-package protobuf-mode)
(use-package yaml-mode)
(use-package base32)

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
  :load-path "~/.cache/emacs/straight/build/org/"
  :custom
  (org-adapt-indentation nil)
  (org-directory (file-name-concat me-emacs-data-dir "org"))
  (org-display-remote-inline-images 'cache)
  (org-export-backends '(ascii beamer html latex md))
  (org-fold-catch-invisible-edits 'smart)
  (org-hide-emphasis-markers t)
  (org-hide-macro-markers t)
  (org-hierarchical-todo-statistics nil)
  (org-image-actual-width nil)
  (org-log-done 'time)
  (org-modules '(ol-bbdb ol-bibtex ol-gnus org-clock org-tempo org-habit org-table))
  (org-provide-todo-statistics t)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-startup-folded 'content)
  (org-startup-with-inline-images t)
  (org-support-shift-select t)
  (org-time-stamp-custom-formats '("<%m/%d/%y %a>" . "<%Y-%m-%d %a %R %z>"))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "KILL(k@)")))
  (org-todo-keyword-faces
   '(("TODO" :inherit modus-themes-nuanced-red)
     ("NEXT" :inherit modus-themes-nuanced-yellow)
     ("DONE" :inherit modus-themes-nuanced-green)
     ("WAIT" :inherit modus-themes-nuanced-blue)
     ("HOLD" :inherit modus-themes-nuanced-magenta)
     ("KILL" :inherit modus-themes-subtle-green)))
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  (org-use-fast-tag-selection 'auto)
  (org-use-fast-todo-selection t)

  :config
  (bind-keys :map org-mode-map
             ("C-J" . me/join-next-line))

  (add-hook 'org-mode-hook #'me//init-org)
  (define-key org-mode-map [remap fill-paragraph] #'org-fill-paragraph)
  (setopt org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (define-key org-mode-map (kbd "C-c [") nil)
  (load-file (file-name-concat org-directory "my-org-misc.el")))

(use-package org-appear
  :custom
  (org-appear-autolinks t)
  :hook (org-mode . org-appear-mode))

(use-package org-refile
  :custom
  (org-refile-targets '((nil :maxlevel . 4)))
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil))

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
  :custom
  (org-habit-following-days 1)
  (org-habit-graph-column   50)
  (org-habit-preceding-days 28))

(use-package org-clock
  :custom
  (org-clock-history-length 32)
  (org-clock-idle-time 10)
  (org-clock-in-resume t)
  (org-clock-into-drawer t)
  (org-clock-persist t)
  (org-clock-persist-file
   (file-name-concat me-emacs-cache-dir "org-clock-save.el"))
  (org-log-into-drawer t)

  :config
  (org-clock-persistence-insinuate))

(use-package org-agenda
  :custom
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-compact-blocks nil)
  (org-agenda-dim-blocked-tasks t)
  (org-agenda-files (file-name-concat org-directory "orgfile"))
  (org-agenda-include-diary nil)
  (org-agenda-show-all-dates t)
  (org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)
  (org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down user-defined-down
             category-keep)
     ((todo priority-down category-keep))
     ((tags priority-down category-keep))
     (search category-keep)))
  (org-agenda-start-with-log-mode t)
  (org-agenda-tags-column -130)
  (org-agenda-prefix-format
   '((agenda   . " %i %-12:c%?-20t%?-15 s")
     (timeline . "  % s")
     (todo     . " %i %-12:T")
     (tags     . " %i %-12:T")
     (search   . " %i %-12:T")))

  :config
  (defun me//init-org-agenda ()
    ;; (set (make-local-variable 'fill-column) 130)
    (set (make-local-variable 'writeroom-width) 150)
    (hl-line-mode))
  (add-hook 'org-agenda-mode-hook #'me//init-org-agenda)

  (advice-add 'org-agenda-goto :after
              (lambda (&rest args)
                (org-narrow-to-subtree)))

  (defun me//org-agenda-cmp-user-defined (a b)
    "Compare the todo states of strings A and B."
    (let* ((todo-priority '(("NEXT" . 6)
                            ("TODO" . 5)
                            ("WAIT" . 4)
                            ("HOLD" . 3)
                            ("DONE" . 2)
                            ("KILL" . 1)))
           (ma (or (get-text-property 1 'org-marker a)
                   (get-text-property 1 'org-hd-marker a)))
           (mb (or (get-text-property 1 'org-marker b)
                   (get-text-property 1 'org-hd-marker b)))
           (fa (and ma (marker-buffer ma)))
           (fb (and mb (marker-buffer mb)))
           (ta (or (get-text-property 1 'todo-state a) "KILL"))
           (tb (or (get-text-property 1 'todo-state b) "KILL"))
           (la (cdr (assoc ta todo-priority)))
           (lb (cdr (assoc tb todo-priority))))
      (cond ((< la lb) -1)
            ((< lb la) +1))))

  (setopt org-agenda-cmp-user-defined #'me//org-agenda-cmp-user-defined)
  (setopt org-agenda-custom-commands
        '(("d" "Daily agenda and all TODOs"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header
                    "High-priority unfinished tasks:")))
            (agenda "" ((org-agenda-span 'day)))
            (alltodo ""
                     ((org-agenda-skip-function
                       '(or (me//org-skip-subtree-if-habit)
                            (me//org-skip-subtree-if-priority ?A)
                            (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header
                       "ALL normal priority tasks:")))
            )
           ((org-agenda-compact-blocks nil))))))

(use-package org-capture
  :custom
  (org-capture-templates
   `(
     ("l" "Log daily" plain
      (file+olp+datetree
       ,(file-name-concat me-emacs-data-dir
                          (format-time-string "time-machine/%Y.org")))
      "%?"
      :empty-lines 1
      :jump-to-captured t
      :tree-type week)

     ("t" "TODO" entry
      (file+headline "todo.org" "Scratch")
      (file "capture/todo.org")
      :empty-lines 1
      :jump-to-captured t)
     )))

(use-package ox
  :custom
  (org-export-global-macros
   '(("tex" . "@@latex:$1@@")
     ("html" . "@@html:$1@@"))))

(use-package org-contrib)
(use-package ox-extra
  :config (ox-extras-activate '(ignore-headlines)))

(use-package ox-html
  :custom
  (org-html-allow-name-attribute-in-anchors t)
  (org-html-doctype "html5")
  (org-html-head-include-default-style nil)
  (org-html-head-include-scripts nil)
  (org-html-html5-fancy t)
  (org-html-htmlize-output-type 'css)
  (org-html-postamble t)
  (org-html-postamble-format
   '(("en" "<a class=\"author\"
           href=\"http://gongzhitaao.org\">%a</a> / <span
           class=\"date\">%T</span><span class=\"creator\">%c</span>"))))

(use-package ox-bibtex)
(use-package ox-latex
  :custom
  (org-latex-caption-above nil)
  (org-latex-hyperref-template "\\hypersetup{
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
  (org-latex-pdf-process
   `(,(concat "PDFLATEX=%latex texi2dvi"
              " --shell-escape --pdf --tidy --verbose --batch %f")))
  (org-latex-prefer-user-labels t)
  (org-latex-src-block-backend 'minted)

  :config
  (add-to-list 'org-latex-packages-alist '("dvipsnames,svgnames,x11names,hyperref" "xcolor"))
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
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

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

;; =============================================================================
;; Helm
;; =============================================================================

(use-package helm-mode
  :delight
  :load-path "~/.cache/emacs/straight/build/helm"
  :custom
  (helm-split-window-inside-p t)

  :custom-face
  (helm-M-x-key ((t :foreground "#0031a9")))

  :config
  (bind-keys ("C-c h"   . helm-command-prefix)
             ("M-x"     . helm-M-x)
             ("C-x C-f" . helm-find-files)
             ("M-s g"   . helm-ag)
             ("C-b"     . helm-mini)
             :map helm-map
             ("<tab>"   . helm-execute-persistent-action)
             ("C-i"     . helm-execute-persistent-action)
             ("C-z"     . helm-select-action)
             :map helm-command-map
             ("d"       . helm-bookmarks))

  (helm-mode 1)
  (helm-autoresize-mode))

(use-package helm-adaptive
  :custom
  (helm-adaptive-history-file
   (file-name-concat me-emacs-cache-dir "helm-adaptive-history"))
  :config
  (helm-adaptive-mode))

(use-package helm-bookmark
  :custom (helm-bookmark-show-location t))

(defun me//isearch-from-helm-occur ()
  "Continue isearch from helm-occur."
  (interactive)
  (helm-run-after-exit
   (lambda (initial)
     (isearch-forward nil t)
     (isearch-yank-string initial))
   helm-pattern))

(use-package helm-buffers
  :custom
  (helm-buffer-max-length 40)
  (helm-buffer-skip-remote-checking nil)
  (helm-buffers-fuzzy-matching t)

  :custom-face
  (helm-buffer-directory ((t :inherit 'dired-directory))))

(use-package helm-files
  :custom
  (helm-ff-file-name-history-use-recentf t)
  (helm-ff-search-library-in-sexp t)
  :custom-face
  (helm-ff-dotted-directory ((t :background "gray80"))))

(use-package helm-for-files
  :custom
  (helm-recentf-fuzzy-match t))

(use-package helm-elisp
  :custom
  (helm-lisp-fuzzy-completion t))

(use-package helm-locate
  :custom (helm-locate-fuzzy-match t))

;; delight
;; -----------------------------------------------------------------------------

(delight
 '((abbrev-mode nil t)
   (auto-fill-function " " t)
   (auto-revert-mode " " autorevert)
   (eldoc-mode nil t)
   (global-subword-mode nil subword)
   (isearch-mode " " t)
   (subword-mode nil subword)
   (view-mode " " view)))

(use-package re-builder
  :custom
  (reb-re-syntax 'string))

(use-package dockerfile-mode)

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

(use-package which-key
  :delight
  :config (which-key-mode))

;; deft
;; -----------------------------------------------------------------------------

(defun me//deft-parse-title (file contents)
  "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
  (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
    (if begin
        (string-trim (substring contents begin (match-end 0))
                     "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
      (deft-base-filename file))))

(use-package deft
  :bind ("<f8>" . deft)
  :custom
  (deft-auto-save-interval 0)
  (deft-default-extension "org")
  (deft-directory (file-name-concat me-emacs-data-dir "notes"))
  (deft-file-naming-rules '((noslash . "-")
                            (nospace . "-")
                            (case-fn . downcase)))
  (deft-recursive t)
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)

  :config
  (advice-add 'deft-parse-title :override #'me//deft-parse-title)

  (setq deft-strip-summary-regexp
        (concat "\\("
                "\n"              ; newline
                "\\|^:.+:.*$"     ; any line with a :SOMETHING:
                "\\|^#\\+.*$"     ; anyline starting with a #+
                "\\|^\\*.+.*$"    ; anyline where an asterisk starts the line
                "\\)")))

(use-package org-roam
  :custom
  (org-roam-directory (file-name-concat me-emacs-data-dir "notes"))
  (org-roam-complete-everywhere t)
  (org-roam-graph-viewer nil)
  :bind (:prefix-map me-org-roam-command-map
         :prefix "C-c n"
         ("c" . org-roam-capture)
         ("f" . org-roam-node-find)
         ("g" . org-roam-graph)
         ("i" . org-roam-node-insert)
         :map org-mode-map
         ("C-c n l" . org-roam-buffer-toggle))
  :config
  (org-roam-db-autosync-mode))

;; Appt
;; -----------------------------------------------------------------------------

(appt-activate 1)
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; =============================================================================
;; mail
;; =============================================================================

(require 'mail-conf)

;; Contacts
;; -----------------------------------------------------------------------------

(use-package bbdb
  :custom
  (bbdb-allow-duplicates t)
  (bbdb-complete-mail-allow-cycling t)
  (bbdb-file (file-name-concat me-emacs-data-dir "contacts.bbdb.gz"))
  (bbdb-mail-user-agent 'message-user-agent)
  (bbdb-message-all-addresses t)
  (bbdb-mua-pop-up nil)

  :config
  (bbdb-initialize 'message 'anniv)
  (add-hook 'message-setup-hook 'bbdb-mail-aliases)

  :straight ( :type git
              :repo "https://git.savannah.nongnu.org/git/bbdb.git"
              :files (:defaults "lisp/bbdb-site.el.in")))

;; =============================================================================
;; Bibliography manager
;; =============================================================================

(defvar me-bib (file-name-concat me-emacs-data-dir "bibliography")
  "My bibliography collection path.")
(defvar me-bib-files `(,(file-name-concat me-bib "refdb.bib"))
  "My bibliography files.")
(defvar me-bib-pdfs `(,(file-name-concat me-bib "pdf"))
  "Paths containing my PDFs of the bibliography.")
(defvar me-bib-notes (file-name-concat me-bib "notes")
  "Path to store my notes on each papers.")

(use-package helm-bibtex
  :load-path "~/.cache/emacs/straight/build/helm-bibtex"
  :bind ("C-c b" . helm-bibtex))

(use-package oc                         ;org-cite
  :custom
  (org-cite-global-bibliography me-bib-files))

;; org-ref
;; -----------------------------------------------------------------------------

(use-package f
  :load-path "~/.cache/emacs/straight/build/f.el")

(defun me//org-ref-notes-function (thekey)
  "Return the name of the note file by THEKEY."
  (bibtex-completion-edit-notes
   (list (car (org-ref-get-bibtex-key-and-file thekey)))))

(defun me//org-ref-add-timestamp ()
  "Add a timestamp field to a bibtex entry, ISO 8601 format."
  (interactive)
  (let ((ts (bibtex-autokey-get-field "timestamp")))
    (bibtex-set-field "timestamp" (format-time-string "%FT%T%z"))))

(use-package org-ref
  :load-path "~/.cache/emacs/straight/build/org-ref"
  :custom
  (doi-utils-download-pdf nil)
  (bibtex-completion-display-formats
   `((article       . "${author:20}  ${title:*}  ${year:4} ${keywords:40} ${journal:15} ${=has-pdf=:1} ${=has-note=:1}")
     (inbook        . "${author:20}  ${title:*}  ${year:4} ${keywords:40} ${chapter:15} ${=has-pdf=:1} ${=has-note=:1}")
     (incollection  . "${author:20}  ${title:*}  ${year:4} ${keywords:40} ${booktitle:15} ${=has-pdf=:1} ${=has-note=:1}")
     (inproceedings . "${author:20}  ${title:*}  ${year:4} ${keywords:40} ${booktitle:15} ${=has-pdf=:1} ${=has-note=:1}")
     (t             . ,(format "${author:20}  ${title:*}  ${year:4} ${keywords:40}  %s  ${=has-pdf=:1} ${=has-note=:1}" (make-string 13 ? )))))
  (bibtex-completion-additional-search-fields '(keywords journal booktitle))

  :config
  (bind-keys :map org-mode-map
             ("C-c ]" . org-ref-insert-ref-link))

  (dolist (func '(org-ref-downcase-bibtex-entry me//org-ref-add-timestamp))
    (add-hook 'org-ref-clean-bibtex-entry-hook func))

  (define-key org-ref-cite-keymap (kbd "M-<right>") #'org-ref-next-key)
  (define-key org-ref-cite-keymap (kbd "M-<left>") #'org-ref-previous-key)
  (define-key org-ref-cite-keymap (kbd "C-<left>") nil)
  (define-key org-ref-cite-keymap (kbd "C-<right>") nil)

  (remove-hook 'org-ref-clean-bibtex-entry-hook #'org-ref-replace-nonascii)
  (add-to-list 'org-ref-bibtex-journal-abbreviations
               '("ArXiv" "Archive e-print" "CoRR")))

;; (require 'org-ref-helm)

(eval-when-compile
  (defun me/cleanup-bibtex-file (arg)
    "Cleanup entries, start from the begnning if ARG."
    (interactive "P")
    (bibtex-progress-message "Cleanup bibtex buffer...")
    (save-excursion
      (if arg
          (bibtex-beginning-first-field)
        (bibtex-beginning-of-entry))
      (save-restriction
        (narrow-to-region (point) (point-max))
        (bibtex-map-entries (lambda (_key _start _end)
                              (bibtex-progress-message)
                              (org-ref-clean-bibtex-entry)))))
    (bibtex-progress-message 'done)))

(defun me/bibtex-find-text-begin ()
  "Go to the beginning of a field entry."
  (interactive)
  (bibtex-find-text t))

(defun me//random-time ()
  "Generate random timestamp from epoch and now."
  (random (time-convert (current-time) 'integer)))

(defun me//bibtex-entry-index ()
  "Return index of BibTeX entry head at or past position of point.

The index is a list (KEY TIMESTAMP) that is used for sorting the
entries of the BibTeX buffer.  Return nil if no entry found.
Move point to the end of the head of the entry found."
  (list (bibtex-key-in-head)
        (let ((ts (bibtex-autokey-get-field "timestamp")))
          (if (string-empty-p ts)
              (me//random-time)
            (time-convert (date-to-time ts) 'integer)))))

(defun me//bibtex-lessp (index1 index2)
  "Predicate for sorting BibTeX entries with indices INDEX1 and INDEX2.

Each index is a list (KEY TIMESTAMP)."
  (let ((t1 (nth 1 index1))
        (t2 (nth 1 index2)))
    (or (and (= t1 t2)
             (string< (car index1) (car index2)))
        (< t1 t2))))

;; (use-package bibtex
;;   :bind (:map bibtex-mode-map
;;               ([remap fill-paragraph]     . bibtex-fill-entry)
;;               ([remap bibtex-clean-entry] . org-ref-clean-bibtex-entry)
;;               ("C-c C-v"                  . bibtex-validate)
;;               ("<backtab>"                . me/bibtex-find-text-begin)
;;               ("M-<down>"                 . bibtex-end-of-entry)
;;               ("M-<up>"                   . bibtex-beginning-of-entry))

;;   :custom
;;   (bibtex-align-at-equal-sign t)
;;   (bibtex-autokey-name-year-separator "")
;;   (bibtex-autokey-titleword-length nil)
;;   (bibtex-autokey-titleword-separator "_")
;;   (bibtex-autokey-titlewords 1)
;;   (bibtex-autokey-titlewords-stretch 0)
;;   (bibtex-autokey-year-length 4)
;;   (bibtex-autokey-year-title-separator "-")
;;   (bibtex-dialect 'biblatex)
;;   (bibtex-entry-format t)
;;   (bibtex-maintain-sorted-entries t)
;;   (bibtex-text-indentation 20)

;;   :config
;;   (defun me//init-bibtex ()
;;     "Init bibtex mode."
;;     (set (make-local-variable 'fill-column) 140)
;;     (set (make-local-variable 'writeroom-width) 150)
;;     (setq bibtex-maintain-sorted-entries
;;           '(me//bibtex-entry-index me//bibtex-lessp)))
;;   (add-hook 'bibtex-mode-hook #'me//init-bibtex))

;; (use-package bibtex-completion
;;   :custom
;;   (bibtex-completion-bibliography me-bib-files)
;;   (bibtex-completion-library-path me-bib-pdfs)
;;   (bibtex-completion-notes-extension ".org")
;;   (bibtex-completion-notes-path me-bib-notes)
;;   (bibtex-completion-notes-symbol "N")
;;   (bibtex-completion-pdf-symbol ""))

;; reftex
;; -----------------------------------------------------------------------------

(use-package reftex
  :delight

  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-ref-style-default-list '("Cleveref" "Hyperref" "Fancyref"))
  (reftex-default-bibliography me-bib-files)

  :hook (tex-mode . reftex-mode))

;; -----------------------------------------------------------------------------

(use-package separedit
  :bind ("C-c ," . separedit)
  :custom
  (separedit-continue-fill-column t)
  (separedit-default-mode 'markdown-mode))

;; =============================================================================
;; Working with PDF
;; =============================================================================

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)

  (defun me/pdf-set-last-viewed-bookmark ()
    (interactive)
    (when (eq major-mode 'pdf-view-mode)
      (let ((bmk (me//pdf-get-bookmark-name)))
        (bookmark-set (me//pdf-get-bookmark-name))
        (bookmark-bmenu-save)
        (message "Bookmark %s set" bmk))))

  (defun me//pdf-jump-last-viewed-bookmark ()
    (let ((bmk (me//pdf-get-bookmark-name)))
      (when (me//pdf-has-last-viewed-bookmark bmk)
        (bookmark-jump bmk))))

  (defun me//pdf-has-last-viewed-bookmark (bmk)
    (assoc bmk bookmark-alist))

  (defun me//pdf-get-bookmark-name ()
    (concat " " (file-name-sans-extension
                  (file-name-nondirectory (buffer-file-name)))))

  (add-hook 'pdf-view-mode-hook 'me//pdf-jump-last-viewed-bookmark))

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

(defun me/image-forward-hscroll-few-lines ()
  "Scroll to left a few lines."
  (interactive)
  (image-forward-hscroll 7))

(defun me/image-backward-hscroll-few-lines ()
  "Scroll to left a few lines."
  (interactive)
  (image-backward-hscroll 7))

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

(defun me//get-cite-key ()
  "Get citation key if possible."
  (cond
   ((derived-mode-p 'org-mode)
    ;; If there is key under curor return it.  Otherwise, use the filename as
    ;; the key.  If the current file is a note associated with a PDF, the
    ;; filename it a key by default.
    (or (ignore-errors (org-ref-get-bibtex-key-under-cursor))
        (file-name-base (buffer-file-name))))
   ((derived-mode-p 'bibtex-mode)
    (bibtex-completion-key-at-point))
   ((derived-mode-p 'pdf-view-mode)
    (file-name-base (buffer-file-name)))
   (t nil)))

(defun me/org-ref-open-entry ()
  "Open bibtex file to key with which the note associated."
  (interactive)
  (let ((key (me//get-cite-key)))
    (if key
        (progn
          (find-file (cdr (org-ref-get-bibtex-key-and-file key)))
          (bibtex-search-entry key))
      (message "Non existing key %s" key))))

(defun me/org-ref-open-note ()
  "Open the associated note file."
  (interactive)
  (let* ((key (me//get-cite-key))
         (pdf-file (org-ref-get-pdf-filename key)))
    (if (and pdf-file (file-exists-p pdf-file))
        (org-ref-open-notes-at-point key)
      (message "Not open note for non-existing PDF %s" key))))

(defun me/org-ref-open-pdf (&optional arg)
  "Open the associated PDF.
If ARG, open with external program.  Otherwise open in Emacs."
  (interactive "P")
  (let* ((key (me//get-cite-key))
         (pdf-file (org-ref-get-pdf-filename key)))
    (if (and pdf-file (file-exists-p pdf-file))
        (org-open-file pdf-file (not arg))
      (if (derived-mode-p 'pdf-view-mode)
          (message "Already opened")
        (message "No PDF found with name %s" pdf-file)))))

(use-package pdf-view
  :bind (:map pdf-view-mode-map
              ("<delete>"   . pdf-view-scroll-up-or-next-page)
              ("<down>"     . me/pdf-view-next-few-lines)
              ("<up>"       . me/pdf-view-prev-few-lines)
              ("<left>"     . me/image-forward-hscroll-few-lines)
              ("<right>"    . me/image-backward-hscroll-few-lines)
              ("<PageUp>"   . pdf-view-scroll-down-or-previous-page)
              ("<PageDown>" . pdf-view-scroll-up-or-next-page)
              ("b"          . me/pdf-set-last-viewed-bookmark)
              ("c"          . me/org-ref-open-entry)
              ;; ("d"       . pdf-view-scroll-up-or-next-page)
              ;; ("e"       . pdf-view-scroll-down-or-previous-page)
              ("d"          . me/pdf-view-next-few-lines)
              ("e"          . me/pdf-view-prev-few-lines)
              ("g"          . pdf-view-goto-page)
              ;; ("j"       . pdf-view-scroll-up-or-next-page)
              ;; ("k"       . pdf-view-scroll-down-or-previous-page)
              ("j"          . me/pdf-view-next-few-lines)
              ("k"          . me/pdf-view-prev-few-lines)
              ("n"          . me/org-ref-open-note)
              ("z"          . delete-other-windows)
              ("C-<left>"   . pdf-view-previous-page-command)
              ("C-<right>"  . pdf-view-next-page-command))
  :custom
  (pdf-view-midnight-colors '("#e5e5e5" . "#333333")))

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
 (file-name-concat me-keylog (format-time-string "key-%FT%H%M%S.log")))

;; =============================================================================
;; Other stuff
;; =============================================================================

(require 'dm)

;; =============================================================================
;; Theme
;; =============================================================================

;; (straight-use-package
;;  '(nano :type git :host github :repo "rougier/nano-emacs"))

;; (use-package nano
;;   :custom
;;   (nano-font-family-monospaced "Iosevka Term SS09")
;;   (nano-font-size 16))

;; (require 'nano-help)
;; (require 'nano-modeline)
;; (require 'nano-layout)

(use-package modus-themes
  :load-path "~/.cache/emacs/straight/build/modus-themes"

  :custom
  (modus-themes-common-palette-overrides '((bg-region bg-ochre)
                                           (fg-region unspecified)))
  (modus-themes-bold-constructs nil)
  (modus-themes-italic-constructs t)
  (modus-themes-region '(bg-only no-extend))

  :custom-face
  (region ((t :extend nil)))

  :config
  (require-theme 'modus-themes)
  (load-theme 'modus-operandi :no-confirm))

;; =============================================================================
;; Now start the server
;; =============================================================================

(use-package server
  :config
  (unless (server-running-p) (server-start)))

;;; init.el ends here
