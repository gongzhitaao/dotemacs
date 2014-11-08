;;; my-package.el --- My package configuration.

(require 'cl)
(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(defvar my-package-file (expand-file-name "package.txt" my-core-dir)
  "Where all my installed packages are stored.  During Emacs
  startup, any package listed here but not installed in the
  system will be installed.")

(defun my-save-package-list ()
  "Save to package.txt the currently activated packages,
  i.e., packages listed in `package-activated-list'"
  ;; (interactive)
  (with-temp-buffer
    (let ((my-sorted-package-list (sort package-activated-list 'string<)))
      (pp my-sorted-package-list (current-buffer)))
    (write-region (point-max) (point-min) my-package-file)))

(defun my-read-n-install-missing ()
  "Read package list from package.txt, if exists.  And install
missing packages when neccessary."
  (interactive)
    (with-temp-buffer
      (insert-file-contents my-package-file)
      (let* ((saved-sorted-package-list
              (car (read-from-string (buffer-string))))
             (missing-package-list
              (remove-if 'package-installed-p saved-sorted-package-list)))
        (if missing-package-list
            (message "%s" missing-package-list)
            (mapc 'package-install missing-package-list)))))

;;(add-hook 'kill-emacs-hook 'my-save-package-list)
;; (add-hook 'emacs-startup-hook 'my-read-n-install-missing)

(provide 'my-package)
;;; my-package.el ends here
