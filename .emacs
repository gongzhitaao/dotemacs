
;; Time-stamp: <2012-09-18 20:24:50 gongzhitaao>

(defconst my-emacs-path "~/.emacs.d/site-lisp/" "my emacs load path")

(defun my-add-subdirs-to-load-path (dir)
  (interactive)
  (let ((default-directory (concat dir "/")))
    (add-to-list 'load-path dir)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	(normal-top-level-add-subdirs-to-load-path))))

(my-add-subdirs-to-load-path my-emacs-path)

(load "basic")
(load "bindings")
(load "org")

(server-start)
