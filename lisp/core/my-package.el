;;; my-package.el --- My package configuration.

;; Copyright (C) 2014  Zhitao Gong

;; Author: Zhitao Gong <zzg0009@auburn.edu>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl)
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar my-packages
  '(ace-jump-mode ack-and-a-half anzu
    bbdb org browse-kill-ring
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

;; (defvar my-auto-mode-alist
;;   '(("\\.coffee\\'" coffee-mode coffee-mode)
;;     ("\\.css\\'" css-mode css-mode)
;;     ("\\.csv\\'" csv-mode csv-mode)
;;     ("\\.go\\'" go-mode go-mode)
;;     ("\\.haml\\'" haml-mode haml-mode)
;;     ("\\.\(la\)?tex\\'" auctex LaTeX-mode)
;;     ("\\.less\\'" less-css-mode less-css-mode)
;;     ("\\.\(markdown\|md\)\\'" markdown-mode markdown-mode)
;;     ("\\.php\\'" php-mode php-mode)
;;     ("\\.sass\\'" sass-mode sass-mode)
;;     ("\\.scss\\'" scss-mode scss-mode)
;;     ("\\.textile\\'" textile-mode textile-mode)
;;     ("\\.ya?ml\\'" yaml-mode yaml-mode)))

;; (mapc #'(lambda (entry)
;;           (let ((ext (nth 0 entry))
;;                 (pkg (nth 1 entry))
;;                 (mode (nth 2 entry)))
;;             (unless (package-installed-p pkg)
;;               (package-install pkg))
;;             (add-to-list 'auto-mode-alist '(ext . mode))))
;;       my-auto-mode-alist)

(provide 'my-package)
;;; my-package.el ends here
