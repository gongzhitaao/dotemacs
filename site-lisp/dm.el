;;; dm.el --- DeepMind specific setup                -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq google-update-load-path nil)
(use-package aio)
(use-package with-editor)
(require 'google)

(use-package vc-fig
  :load-path "/usr/share/google-emacs/site-lisp/emacs-google-config/devtools/editors/emacs/vc"
  :straight nil)
(use-package google3-formatters
  :load-path "/usr/share/google-emacs/site-lisp/emacs-google-config/devtools/editors/emacs/google3_formatters"
  :straight nil)
(use-package bazel)
(use-package p4)
(use-package company)
(use-package google-borg-helpers
  :load-path "/usr/share/google-emacs/site-lisp/emacs-google-config/third_party/elisp/google_borg_helpers"
  :straight nil)

(let ((google-unorthodox-hooks '(google-maybe-untabify-buffer
                                 google-maybe-delete-trailing-whitespace
                                 google-maybe-trim-newlines)))
  (dolist (hook google-unorthodox-hooks)
    (remove-hook 'write-file-hooks hook)
    (add-hook 'before-save-hook hook)))

(defun me/g3-google3-filepath (filepath)
  "Return google3/ path if FILEPATH is a google3 file."
  (string-match "/google/src/cloud/\\w+/\\w+/google3/" filepath)
  (let ((start (match-end 0))
        (g3path ""))
    (when (< start (length filepath))
      (setq g3path (substring filepath start))
      (kill-new g3path)
      (message g3path))))

(advice-add 'dired-copy-filename-as-kill
            :filter-return #'me/g3-google3-filepath)
(add-to-list 'auto-mode-alist '("\\.st_schemas\\'" . spansdl-mode))

(defun me//make-temp-file-in-tmp (args)
  "Make temporary file in /tmp instead of PREFIX and pass ARGS along."
  (cons
   (replace-regexp-in-string
    "/google/src/cloud/\\([[:alnum:]-_.]+?/\\)+"
    temporary-file-directory (car args))
   (cdr args)))
(advice-add 'make-temp-file :filter-args #'me//make-temp-file-in-tmp)

(use-package google-pyformat)
;; (use-package ffap
;;   :config
;;   (setf ffap-alist (assoc-delete-all (rx anything) ffap-alist)))

(provide 'dm)
;;; dm.el ends here
