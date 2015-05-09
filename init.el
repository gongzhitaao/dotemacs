;;; my-editor.el --- My Editor configuration.
;;; Time-stamp: <2015-05-08 21:53:23 gongzhitaao>
(require 'cl)

;; -------------------------------------------------------------------
;; Some variables
;; -------------------------------------------------------------------
(setq user-full-name "Zhitao Gong")

(defvar my-dir user-emacs-directory
  "The root dir for my Emacs configuration.")
(defvar my-tmp (expand-file-name "tmp" user-emacs-directory)
  "Temp files that might be useful but that I don't care about.")
(defvar my-personal-dir (expand-file-name "~/Dropbox/emacs/personal"))
(defvar my-icons-dir (expand-file-name "icons" my-dir)
  "Where All the icons are stored.")
(defvar my-conf-dir (expand-file-name "conf" my-dir)
  "conf file")
(defvar my-package-file (expand-file-name "pkg.txt" my-dir)
  "The minimum packages I need.")

(unless (file-exists-p my-tmp) (make-directory my-tmp-dir))

;; -------------------------------------------------------------------
;; Some Helper
;; -------------------------------------------------------------------

(defun my-apply-region-or-line (func)
  "Apply FUNC to a region, or current line if mark is not
  active."
  (save-excursion
  (if (region-active-p)
      (funcall func (region-beginning) (region-end))
    (funcall func (line-beginning-position) (line-end-position)))))

(defun my-apply-region-or-para (func)
  "Apply FUNC to a region, or current paragraph if mark is not
  active."
  (save-excursion
    (if (not (region-active-p))
        (mark-paragraph))
    (funcall func (region-beginning) (region-end))))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

;; -------------------------------------------------------------------
;; Package
;; -------------------------------------------------------------------

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(defun my-read-n-install-missing ()
    "Read package list from pkg.txt, if exists.  And install
missing packages when neccessary."
    (interactive)
    (with-temp-buffer
      (insert-file-contents my-package-file)
      (let* ((saved-sorted-package-list
	      (car (read-from-string (buffer-string))))
	     (missing-package-list
	      (remove-if 'package-installed-p saved-sorted-package-list)))
	(if missing-package-list
	    (progn (message "%s" missing-package-list)
		   (mapc 'package-install missing-package-list))))))
(my-read-n-install-missing)

;; -------------------------------------------------------------------
;; Editor
;; -------------------------------------------------------------------

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(delete-selection-mode t)

(add-hook 'write-file-functions
	  (lambda ()
	    (delete-trailing-whitespace)
	    (time-stamp)))

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
(blink-cursor-mode 0)
(setq scroll-preserve-screen-position t)
(setq require-final-newline t)
(mouse-avoidance-mode 'animate)
(setq blink-matching-paren nil)

;; look
(tool-bar-mode 0)
(menu-bar-mode 1)
(scroll-bar-mode 0)

(setq visilble-bell t)

(setq inhibit-startup-message t
      resize-mini-windows t)

(column-number-mode 1)
(setq size-indication-mode t)

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

(require 'diminish)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" my-tmp))
(setq-default save-place t)

(require 'savehist)
(setq savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (expand-file-name "savehist" my-tmp))
(savehist-mode +1)

(require 'recentf)
(add-to-list 'recentf-exclude (expand-file-name ".*" my-tmp))
(setq recentf-save-file (expand-file-name "recentf" my-tmp))
(recentf-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

(require 'tramp)
(setq tramp-default-method "ssh"
      tramp-persistency-file-name (expand-file-name "tramp" my-tmp))

(set-default 'imenu-auto-rescan t)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(require 'expand-region)

(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

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

(require 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

(setq backup-directory-alist `((".*" . ,my-tmp)))
(setq auto-save-list-file-prefix (expand-file-name ".saves-" my-tmp))

(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq scroll-margin 0
      scroll-preserve-screen-position 1)

(setq delete-by-moving-to-trash t)

(global-subword-mode 1)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq view-read-only t)

(eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
    "Show line numbers temporarily, while prompting for the line
number input"
    (interactive)
    (unwind-protect
        (progn
          (linum-mode 1)
          (goto-line (read-number "Goto line: ")))
      (linum-mode -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(require 'fill-column-indicator)
(setq fci-rule-color "white")

(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil :background "#3B3D3A")

(require 'ido)
(ido-mode 'both) ;; for buffers and files
(setq ido-save-directory-list-file (expand-file-name "idolast" my-tmp)
      ;; ignore these buffers during completion
      ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
                           "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
      ;; case insensitive
      ido-case-fold t
      ;; remember last directory
      ido-enable-last-directory-history t
      ido-max-work-file-list 50
      ido-use-filename-at-point nil
      ido-use-url-at-point nil
      ido-enable-flex-matching nil
      ido-max-prospects 6
      ido-confirm-unique-completion t)

(setq confirm-nonexistent-file-or-buffer nil)

;; increase minibuffer size when ido completion is active
(add-hook 'ido-minibuffer-setup-hook
          (function
           (lambda ()
             (make-local-variable 'resize-minibuffer-window-max-height))))

(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-basic-offset 2
      js2-bounce-indent-p nil)
(setq js2-mode-indent-ignore-first-tab t)
(setq-default js2-global-externs
              '("assert" "clearInterval" "clearTimeout" "console" "d3" "describe" "document" "google" "hljs" "Infinity" "jQuery" "JSON" "localStorage" "module" "require" "setInterval" "setTimeout" "window"))

;; annoying mode, dont like it
;; (require 'multi-web-mode)
;; (setq mweb-default-major-mode 'html-mode)
;; (setq mweb-tags
;;       '((js-mode  "<script\\(\\s-+type=\"text/javascript\"\\|language=\"javascript\"[^>]*\\)?>" "</script>")
;;         (css-mode "<style\\(\\s-+type=\"text/css\"[^>]*\\)?>" "</style>")
;;         (php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")))
;; (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;; (multi-web-global-mode 1)

(require 'web-mode)
(setq web-mode-enable-engine-detection t)
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 1)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (smartparens-mode t)))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq sgml-basic-offset 0)

(setq mweb-submode-indent-offset 2)
(setq sql-indent-offset 1)

(setq tab-stop-list (number-sequence 2 120 2))
(add-hook 'prog-mode-hook 'which-function-mode)

(eval-after-load "sql"
  '(load-library "sql-indent"))
(setq sql-indent-offset 1)

;; -------------------------------------------------------------------
;; Keymap
;; -------------------------------------------------------------------

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'query-replace-regexp)

(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "<f9>") 'recentf-open-files)
;; f10 - menu
(global-set-key (kbd "<f11>") 'ispell)
(global-set-key (kbd "<f12>") 'gnus-other-frame)

(global-set-key (kbd "C-c k") 'browse-kill-ring)
(global-set-key (kbd "C-c g") 'ace-jump-mode)
(global-set-key (kbd "C-c |") 'fci-mode)
(global-set-key (kbd "C-c ,") 'color-identifiers-mode)
(global-set-key (kbd "C-c =") 'align-regexp)
(global-set-key (kbd "C-c /")
                (function (lambda ()
                            (interactive)
                            (my-apply-region-or-para
                             'comment-or-uncomment-region))))
(global-set-key (kbd "C-c \\")
                (function (lambda ()
                            (interactive)
                            (my-apply-region-or-para
                             'indent-region))))

(global-set-key (kbd "C-c <left>") 'decrease-left-margin)
(global-set-key (kbd "C-c <right>") 'increase-left-margin)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(global-set-key (kbd "C-c C-<left>") 'decrease-left-margin)
(global-set-key (kbd "C-c C-<right>") 'increase-left-margin)
(global-set-key (kbd "C-c C-c")
                (function (lambda ()
                            (interactive)
                            (my-apply-region-or-line
                             'comment-or-uncomment-region))))

(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(defun my-sp-keymap()
  "My sp-keymap"
  (define-key sp-keymap (kbd "M-s f") 'sp-forward-sexp)
  (define-key sp-keymap (kbd "M-s b") 'sp-backward-sexp)

  (define-key sp-keymap (kbd "M-s d") 'sp-down-sexp)
  (define-key sp-keymap (kbd "M-s D") 'sp-backward-down-sexp)
  (define-key sp-keymap (kbd "M-s a") 'sp-beginning-of-sexp)
  (define-key sp-keymap (kbd "M-s e") 'sp-end-of-sexp)

  (define-key sp-keymap (kbd "M-s u") 'sp-up-sexp)
  (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
  (define-key sp-keymap (kbd "M-s U") 'sp-backward-up-sexp)
  (define-key sp-keymap (kbd "M-s t") 'sp-transpose-sexp)

  (define-key sp-keymap (kbd "M-s n") 'sp-next-sexp)
  (define-key sp-keymap (kbd "M-s p") 'sp-previous-sexp)

  (define-key sp-keymap (kbd "M-s k") 'sp-kill-sexp)
  (define-key sp-keymap (kbd "M-s w") 'sp-copy-sexp)

  (define-key sp-keymap (kbd "M-s s") 'sp-forward-slurp-sexp)
  (define-key sp-keymap (kbd "M-s r") 'sp-forward-barf-sexp)
  (define-key sp-keymap (kbd "M-s S") 'sp-backward-slurp-sexp)
  (define-key sp-keymap (kbd "M-s R") 'sp-backward-barf-sexp)
  (define-key sp-keymap (kbd "M-s F") 'sp-forward-symbol)
  (define-key sp-keymap (kbd "M-s B") 'sp-backward-symbol)

  (define-key sp-keymap (kbd "M-s [") 'sp-select-previous-thing)
  (define-key sp-keymap (kbd "M-s ]") 'sp-select-next-thing)

  (define-key sp-keymap (kbd "M-s M-i") 'sp-splice-sexp)
  (define-key sp-keymap (kbd "M-s <delete>") 'sp-splice-sexp-killing-forward)
  (define-key sp-keymap (kbd "M-s <backspace>") 'sp-splice-sexp-killing-backward)
  (define-key sp-keymap (kbd "M-s M-<backspace>") 'sp-splice-sexp-killing-around)

  (define-key sp-keymap (kbd "M-s M-w") 'sp-wrap)
  (define-key sp-keymap (kbd "M-s M-u") 'sp-unwrap-sexp)
  (define-key sp-keymap (kbd "M-s M-b") 'sp-backward-unwrap-sexp)

  (define-key sp-keymap (kbd "M-s M-t") 'sp-prefix-tag-object)
  (define-key sp-keymap (kbd "M-s M-p") 'sp-prefix-pair-object)
  (define-key sp-keymap (kbd "M-s M-c") 'sp-convolute-sexp)
  (define-key sp-keymap (kbd "M-s M-a") 'sp-absorb-sexp)
  (define-key sp-keymap (kbd "M-s M-e") 'sp-emit-sexp)
  (define-key sp-keymap (kbd "M-s M-p") 'sp-add-to-previous-sexp)
  (define-key sp-keymap (kbd "M-s M-n") 'sp-add-to-next-sexp)
  (define-key sp-keymap (kbd "M-s M-j") 'sp-join-sexp)
  (define-key sp-keymap (kbd "M-s M-s") 'sp-split-sexp)
  (define-key sp-keymap (kbd "M-s M-r") 'sp-raise-sexp))

(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)
(global-set-key (kbd "M-]") 'rainbow-delimiters-mode)

(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode 1)
(setq sp-navigate-close-if-unbalanced t)
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
               (sp-local-tag "i" "\"<" "\">")
               (sp-local-tag "i" "\"[" "\"]"))
(my-sp-keymap)

;; -------------------------------------------------------------------
;; font and encoding system
;; -------------------------------------------------------------------

(let ((my-prefer-coding-system
       '(cp950 gb2312 cp936 gb18030 utf-16 utf-8)))
  (dolist (c my-prefer-coding-system)
    (prefer-coding-system c)))

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset (font-spec :family "WenQuanYi Zen Hei Mono"
                      :size 16)))

(load-theme 'naquadah t)
(global-hl-line-mode 1)

(set-face-attribute hl-line-face nil :background "#3B3D3A")
(set-face-attribute 'sp-pair-overlay-face nil :background "#005500")

(when (file-exists-p my-conf-dir)
  (mapc 'load (directory-files my-conf-dir 't "^[^#].*\.el$")))

(let ((my-file (expand-file-name "chinese-wubi" my-conf-dir)))
  (when (file-exists-p my-file)
    (add-to-list 'load-path my-file)
    (require 'chinese-wubi)))

(server-start)

;;; init.el ends here
