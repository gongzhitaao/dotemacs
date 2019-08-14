;;; dm.el --- DeepMind specific setup                -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package google
  :defer 2)

(defvar org-capture-templates)          ; make linter happy
(add-to-list 'org-capture-templates
             '("d" "DeepMind Tech Log" plain
               (file+olp+datetree "dm.org")
               "%?"
               :empty-lines 1
               :jump-to-captured t
               :tree-type week))

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

(provide 'dm)
;;; dm.el ends here
