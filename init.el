;;; init.el --- My Emacs init file

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
(setq custom-file (expand-file-name "lisp/custom.el" my-dir))
(when (file-exists-p my-modules-dir)
  (mapc 'load (directory-files my-modules-dir 't "^[^#].*-conf\.el$")))
(when (file-exists-p my-vendors-dir)
  (mapc 'load (directory-files my-modules-dir 't "^[^#].*\.el$")))

(server-start)

(provide 'init)
;;; init.el ends here
