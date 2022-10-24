;;; dm.el --- DeepMind specific setup                -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'google)

;; (use-package bazel)
;; (use-package aio)
;; (use-package with-editor)

;; (use-package google3-build-mode
;;   :after (bazel aio with-editor)
;;   :config
;;   (setq google3-build-cleanup-on-save nil))

(let ((google-unorthodox-hooks '(google-maybe-untabify-buffer
                                 google-maybe-delete-trailing-whitespace
                                 google-maybe-trim-newlines)))
  (dolist (hook google-unorthodox-hooks)
    (remove-hook 'write-file-hooks hook)
    (add-hook 'before-save-hook hook)))

(provide 'dm)
;;; dm.el ends here
