;;; my-package.el --- My package configuration.

(require 'cl)
(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(defun save-packages ()
  "Save to saved-packages.txt the currently activated packages,
  i.e., packages listed in `package-activated-list'"
  (interactive)
  (with-temp-buffer
    (pp ())))

(defvar my-packages
  '(ace-jump-mode                 ;quick cursor location jump
    ack-and-a-half                ;front-end for ack
    anzu                          ;show number of matches in mode-line
    auctex                        ;integrated env for *Tex*
    auto-complete                 ;auto completion
    bbdb                          ;The Insidious Big Brother Database
    org browse-kill-ring
    color-identifiers-mode
    dash deft diminish elisp-slime-nav
    epl expand-region fill-column-indicator flycheck gist
    gitconfig-mode gitignore-mode glsl-mode google-c-style grizzl
    guru-mode multi-web-mode naquadah-theme projectile
    magit move-text rainbow-mode rainbow-delimiters
    smartparens undo-tree
    volatile-highlights zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(mapc #'(lambda (pkg)
          (unless (package-installed-p pkg)
            (package-install pkg)))
      my-packages)

(provide 'my-package)
;;; my-package.el ends here
