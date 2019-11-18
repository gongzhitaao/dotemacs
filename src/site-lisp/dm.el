;;; dm.el --- DeepMind specific setup                -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package google
  :defer 2)

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

(add-to-list 'org-capture-templates
             '("w" "Work log" plain
               (file+olp+datetree "/google/src/cloud/gongzhitaao/worklog/google3/experimental/deepmind/gongzhitaao/worklog/worklog.org")
               "%?"
               :empty-lines 1
               :jump-to-captured t
               :tree-type week) t)

(use-package google3-build-mode
  :config
  (setq google3-build-cleanup-on-save 'reformat-file))

;; (use-package google3-eglot
;;   :config
;;   (setq google3-eglot-c++-server 'clangd)
;;   (google3-eglot-setup))

(provide 'dm)
;;; dm.el ends here
