;;; dm.el --- DeepMind specific setup                -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package google
  :defer 2)

(bind-keys :prefix-map me-editing-command-map
           :prefix "C-c e"
           ("g"   . me/grab-google3-python-imports))

(defun me//citc-file-p (file-name)
  "Return non-nil if FILE-NAME is in citc client."
  (string-match "google3" file-name))

(defun me/grab-google3-python-imports ()
  "Use current file as import."
  (interactive)
  (let ((google3-start-pos (me//citc-file-p buffer-file-name)))
    (when google3-start-pos
      (kill-new
       (replace-regexp-in-string
        "/" "."
        ;; extracts google3/path/to/your/py/file sans .py
        (substring buffer-file-name
                   google3-start-pos
                   ;; -3 for .py
                   -3))))))

(use-package writeroom-mode
  :config
  (add-to-list 'writeroom-major-modes 'protobuf-mode))

(defun me//turn-on-gogolink ()
  "Turn on gogolink whenver appropriate."
  (if (derived-mode-p 'org-mode)
      (gogolink-mode)
    (gogolink-prog-mode)))

(use-package gogolink
  :config
  (defface me-gogolink-link-face
    '((t :foreground "LightCyan3"
         :underline t
         :inherit gogolink-link-face))
    "gogolink link face")
  (setq gogolink-link-face 'me-gogolink-link-face)
  (add-hook 'find-file-hook #'me//turn-on-gogolink))

(use-package google-cc-extras
  :bind (:map c++-mode-map
         ("C-!" . google-clang-format)))

(use-package typescript-mode
  :bind (:map typescript-mode-map
         ("C-!" . google-clang-format))
  :config
  (setq typescript-indent-level 2)
  (setq tide-tsserver-directory
        (concat "/google/src/head/depot/google3"
                "/third_party/javascript/node_modules"
                "/typescript/stable/lib")))

(use-package google3-build-mode
  :config
  (setq google3-build-cleanup-on-save 'reformat-file))

;; (use-package google3-eglot
;;   :config
;;   (setq google3-eglot-c++-server 'clangd)
;;   (google3-eglot-setup))

(provide 'dm)
;;; dm.el ends here
