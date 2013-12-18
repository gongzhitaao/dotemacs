;;; misc.conf.el --- Extensive configuration for Emacs
;;; Time-stamp: <2013-12-18 17:09:33 CST gongzhitaao>

;; -------------------------------------------------------------------
;; font and encoding system
;; -------------------------------------------------------------------

(let ((my-prefer-coding-system
       '(cp950 gb2312 cp936 gb18030 utf-16 utf-8)))
  (dolist (c my-prefer-coding-system)
    (prefer-coding-system c)))

;; en
(set-face-attribute 'default nil :font "Dejavu Sans Mono:pixelsize=14")

;; zh
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset (font-spec :family "WenQuanYi Zen Hei Mono"
		      :size 16)))

(setq auto-mode-alist
      (append '((".*rc$" . conf-mode)
                ("\\.md$" . markdown-mode)
                ("\\(Makefile.*\\)\\(\\.make$\\)?$" . makefile-mode)
                ("\\.js$" . js2-mode)
                ("\\.m$" . octave-mode)
                ("\\.\\(h\\|cu\\)$" . c++-mode)
                ("CMake.*\\.txt$" . cmake-mode))
              auto-mode-alist))

(my-add-hooks
 '(lambda ()
    (auto-fill-mode 1))
 '(markdown-mode-hook LaTeX-mode-hook latex-mode-hook))

;; -------------------------------------------------------------------
;; Temporary/Cache files
;; -------------------------------------------------------------------

;; backup editing file in one foler so that it doesn't cluster my
;; folder with tilded filenames
(let ((my-backup-dir (concat my-emacs-cache "backups")))

  (unless (file-exists-p my-backup-dir) (make-directory my-backup-dir))

  (setq backup-directory-alist `(("." . ,my-backup-dir))
        backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

  (message "Auto cleaning week's old backup files...")
  (let ((week (* 60 60 24 7))
        (current (float-time (current-time))))
    (dolist (file (directory-files my-backup-dir t))
      (when (and (backup-file-name-p file)
                 (> (- current (float-time (nth 5 (file-attributes file)))) week))
        (message "%s" file)
        (delete-file file)))))

(let ((my-autosave-dir (concat my-emacs-cache "autosave/")))
  (setq auto-save-list-file-prefix (concat my-autosave-dir ".saves-")))

(let ((my-tramp-file (concat my-emacs-cache "tramp")))
  (require 'tramp)
  (setq tramp-default-method "ssh"
        tramp-persistency-file-name my-tramp-file))

(let ((my-savehist-file (concat my-emacs-cache "savehist")))
  (savehist-mode t)
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-file my-savehist-file))

;; save recently used files
(let ((my-recent-file (concat my-emacs-cache "recentf")))
  (require 'recentf)
  (recentf-mode 1)
  (setq recentf-save-file my-recent-file
        recentf-max-saved-items 100
        recentf-max-menu-items 15))

(let ((my-abbrev-file (concat my-emacs-cache "abbrev_defs")))
  (setq abbrev-file-name my-abbrev-file)
  (abbrev-mode t)
  (setq save-abbrevs t)
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
  (add-hook 'kill-emacs-hook
            'write-abbrev-file))

(let ((my-saveplace (concat my-emacs-cache "saveplace")))
  (setq-default save-place t)
  (require 'saveplace)
  (setq save-place-file my-saveplace))

;; cleanup buffer name when buffer with the same name is closed.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-ignore-buffers-re "^\\*")

(eval-after-load "filecache"
  '(progn (message "Loading file cache...")
          (file-cache-add-directory "~/")
          (file-cache-add-directory-list '("~/Documents"))))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(put 'dired-find-alternate-file 'disabled nil)

;; -------------------------------------------------------------------
;; auto complete, ido
;; -------------------------------------------------------------------
(require 'ido)
(ido-mode 'both) ;; for buffers and files
(setq ido-save-directory-list-file (concat my-emacs-cache "ido.last")
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

;; -------------------------------------------------------------------
;; auto complete
;; -------------------------------------------------------------------
(require 'yasnippet)
(auto-insert-mode 1)

(require 'auto-complete)
(global-auto-complete-mode 1)
(require 'auto-complete-c-headers)
(add-to-list 'ac-sources 'ac-source-c-headers)

(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`
(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
	(append '(ac-source-math-unicode
		  ac-source-math-latex
		  ac-source-latex-commands)
		ac-sources)))
(add-hook 'latex-mode-hook 'ac-latex-mode-setup)

(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq ac-js2-evaluate-calls t)

;; -------------------------------------------------------------------
;; rainbow-delimiters-mode
;; -------------------------------------------------------------------

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; -------------------------------------------------------------------
;; bibtex-mode
;; -------------------------------------------------------------------

(add-hook 'bibtex-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "s-\\") 'bibtex-fill-entry)
	     (set-fill-column 120)
	     (bibtex-set-dialect 'biblatex)))

(setq bibtex-text-indentation 20)
(setq bibtex-align-at-equal-sign t)

;; -------------------------------------------------------------------
;; calendar
;; -------------------------------------------------------------------

(require 'appt)

(add-hook 'calendar-load-hook
	  (lambda ()
	    (calendar-mark-holidays)
	    (diary-mark-entries)))
(add-hook 'calendar-today-visible-hook
	  (lambda ()
	    (calendar-mark-today)))
(add-hook 'diary-list-entries-hook
	  (lambda ()
	    (diary-sort-entries)))
(setq diary-file (concat my-emacs-data "diary"))
(calendar-set-date-style 'iso)

;; -------------------------------------------------------------------
;; encrypt
;; -------------------------------------------------------------------

(require 'epa-file)

(defadvice epg--start (around advice-epg-disable-agent disable)
  "Make epg--start not able to find a gpg-agent"
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (setenv "GPG_AGENT_INFO" agent)))

(defun epg-disable-agent ()
  "Make EasyPG bypass any gpg-agent"
  (interactive)
  (ad-enable-advice 'epg--start 'around 'advice-epg-disable-agent)
  (ad-activate 'epg--start)
  (message "EasyPG gpg-agent bypassed"))

(defun epg-enable-agent ()
  "Make EasyPG use a gpg-agent after having been disabled with
epg-disable-agent"
  (interactive)
  (ad-disable-advice 'epg--start 'around 'advice-epg-disable-agent)
  (ad-activate 'epg--start)
  (message "EasyPG gpg-agent re-enabled"))
(epg-disable-agent)

;; -------------------------------------------------------------------
;; prog mode
;; -------------------------------------------------------------------
(require 'cc-mode)

(add-hook 'prog-mode-hook
	  (lambda ()
	    (electric-pair-mode 1)
	    (hl-line-mode 1)
	    (set-face-attribute hl-line-face nil :background "#3B3D3A")))

(setq-default indent-tabs-mode nil
	      tab-width 4)

(my-add-hooks '(lambda ()
		 (local-set-key "\C-n" 'next-error)
		 (local-set-key "\C-p" 'previous-error))
	      '(c++-mode-hook c-mode-hook python-mode-hook emacs-lisp-mode-hook))

(add-hook 'c-mode-common-hook 'google-set-c-style)

(add-hook 'c-mode-hook
	  (lambda ()
	    (add-to-list 'ac-sources 'ac-source-c-headers)
	    (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

(add-hook 'python-mode-hook
	  '(lambda ()
	     (local-unset-key (kbd "<f7>"))
	     (local-set-key (kbd "<f7>") 'pylint)))

(require 'compile)

(add-hook 'compilation-mode-hook
	  '(lambda ()
	     (setq compilation-scroll-output 'first-error)))

(setq gdb-many-windows t)

;; -------------------------------------------------------------------
;; Multi web mode
;; -------------------------------------------------------------------

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
		  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")
		  (js2-mode "<script\\( +type=\"text/javascript\"\\|language=\"javascript\"\\)?[^>]*>" "</script>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; -------------------------------------------------------------------
;; deft
;; -------------------------------------------------------------------

(when (require 'deft nil 'noerror)
  (setq deft-extension "org"
	deft-directory (concat my-emacs-data "deft")
	deft-text-mode 'org-mode))

;; -------------------------------------------------------------------
;; byte compile source if needed when killing emacs
;; -------------------------------------------------------------------

(provide 'misc.conf)
;;; misc.config.el ends here
