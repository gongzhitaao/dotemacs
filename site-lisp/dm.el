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

(defun me//g3-make-py-import (filepath)
  "Return a proper Python import statement for FILEPATH."
  (let* ((parts (split-string filepath "/"))
         (module (car (last parts)))
         (ext (file-name-extension module)))
    ;; Prepends the leading "google3".
    (push "google3" parts)
    ;; Removes the module which needs further processing.
    (setq parts (butlast parts))
    ;; Formats the standard import statement.
    (format "from %s import %s"
            (string-join parts ".")
            (pcase ext
              ("py" (file-name-sans-extension module))
              ("proto" (format "%s_pb2" (file-name-sans-extension module)))))))

(defun me//g3-make-cc-include (filepath)
  "Return a proper cc include statement for FILEPATH."
  (let ((ext (file-name-extension filepath))
        (stem (file-name-sans-extension filepath))
        (suffix ""))
    (setq suffix (pcase ext
                   ("h" ".h")
                   ("cc" ".h")
                   ("proto" ".proto.h")))
    (format "#include \"%s%s\"" stem suffix)))

(defun me//g3-make-build-target (filepath)
  "Return a proper build target for FILEPATH."
  (let ((dir (substring (file-name-directory filepath) 0 -1))
        (module (file-name-sans-extension (file-name-base filepath))))
    (if (string= "proto" (file-name-extension filepath))
        (setq module (concat module "_proto")))
    (format "//%s:%s" dir module)))

(defun me//string-capitalized-p (str)
  (let ((case-fold-search nil))
    (string-match-p "\\`[[:upper:]]+\\'" str)))

(defun me//g3-filepath (filepath)
  "Return google3/ path if FILEPATH is a google3 file."
  (string-match "/google/src/cloud/\\w+/\\w+/google3/" filepath)
  (let ((start (match-end 0))
        (g3path "")
        (candidates nil)
        (selection ""))
    (when (< start (length filepath))
      ;; Now we have an absolute path pointing to a file in a citc client.

      ;; Get the file path relative to the google3/ directory.
      (setq g3path (substring filepath start))

      (push g3path candidates)

      (unless (me//string-capitalized-p (file-name-base g3path))
        (push (me//g3-make-build-target g3path) candidates))

      (pcase (file-name-extension g3path)
        ("h" (push (me//g3-make-cc-include g3path) candidates))
        ("cc" (push (me//g3-make-cc-include g3path) candidates))
        ("py" (push (me//g3-make-py-import g3path) candidates))
        ("proto"
         (push (me//g3-make-py-import g3path) candidates)
         (push (me//g3-make-cc-include g3path) candidates)))

      (if (eq (length candidates) 1)
          (setq selection (nth 0 candidates))
        (setq selection (completing-read "Select to copy: " candidates)))

      (unless (string= selection "")
        (kill-new selection)
        (message selection)))))

(advice-add 'dired-copy-filename-as-kill
            :filter-return #'me//g3-filepath)
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
(use-package google-java-format
  :load-path "/usr/share/emacs/site-lisp/emacs-google-config/third_party/java_src/google_java_format/project/core/src/main/scripts")
;; (use-package ffap
;;   :config
;;   (setf ffap-alist (assoc-delete-all (rx anything) ffap-alist)))

(provide 'dm)
;;; dm.el ends here
