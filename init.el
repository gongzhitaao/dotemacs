;;; init.el --- My Emacs init file

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

(defvar my-dir user-emacs-directory
  "The root dir for my Emacs configuration")
(defvar my-core-dir (expand-file-name "lisp/core" my-dir)
  "My core functionality.")
(defvar my-modules-dir (expand-file-name "lisp/modules" my-dir)
  "Module configuration.")
(defvar my-personal-dir (expand-file-name "personal" my-dir)
  "My personal data.")
(defvar my-vendors-dir (expand-file-name "lisp/vendors" my-dir)
  "Packages not mananged by package.el yet..")
(defvar my-savefile-dir (expand-file-name "savefile" my-dir)
  "Where all the automatically generated save/histry files rest.")
(defvar my-icons-dir (expand-file-name "icons" my-dir)
  "Where All the icons are stored.")

(unless (file-exists-p my-savefile-dir)
  (make-directory my-savefile-dir))

(let ((default-directory (expand-file-name "lisp" my-dir)))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory my-vendors-dir))
  (normal-top-level-add-subdirs-to-load-path))

(require 'my-core)
(require 'my-package)
(require 'my-keybinding)
(require 'my-editor)

;; load the personal settings
(setq custome-file (expand-file-name "lisp/custom.el" my-dir))
(when (file-exists-p my-modules-dir)
  (mapc 'load (directory-files my-modules-dir 't "^[^#].*-conf\.el$")))
(when (file-exists-p my-vendors-dir)
  (mapc 'load (directory-files my-modules-dir 't "^[^#].*\.el$")))

(server-start)

(provide 'init)
;;; init.el ends here
