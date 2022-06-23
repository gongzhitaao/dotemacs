;;; dm.el --- DeepMind specific setup                -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package google)

(use-package writeroom-mode
  :config
  (add-to-list 'writeroom-major-modes 'protobuf-mode))

(use-package google3-build-mode
  :config
  (setq google3-build-cleanup-on-save 'reformat-file))

(let ((google-unorthodox-hooks '(google-maybe-untabify-buffer
                                 google-maybe-delete-trailing-whitespace
                                 google-maybe-trim-newlines)))
  (dolist (hook google-unorthodox-hooks)
    (remove-hook 'write-file-hooks hook)
    (add-hook 'before-save-hook hook)))

(provide 'dm)
;;; dm.el ends here
