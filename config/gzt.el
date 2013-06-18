;;; gzt.el
;;; Time-stamp: <2013-06-15 15:29:48 CDT gongzhitaao>
;;;
;;; Convinient custome functions

(defun gzt/add-hooks (func hooks)
  "add FUNC to multiple HOOKs"
  (mapc (lambda (hook)
          (add-hook hook func))
        hooks))

(defun gzt/byte-recompile-directory ()
  "Convinient function for recompiling my config and plugin lisp
files"
  (interactive)
  (byte-recompile-directory my-emacs-config-dir 0)
  (byte-recompile-directory my-emacs-plugin-dir 0))

(defun gzt/toggle-comment-region-or-line ()
  "Toggle comment on active region or current line if no region
is active"
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
  )

(provide 'gzt)
