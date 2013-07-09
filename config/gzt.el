;;; gzt.el
;;; Time-stamp: <2013-07-08 19:54:29 CDT gongzhitaao>
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

(defun gzt/apply-region-or-line (func)
  "Apply FUNC to a region, or current line if mark is not
  active."
  (if (region-active-p)
      (funcall func (region-beginning) (region-end))
    (funcall func (line-beginning-position) (line-end-position))
    )
  )

(defun gzt/toggle-comment-region-or-line ()
  "Toggle comment on active region or current line if no region
is active"
  (interactive)
  (gzt/apply-region-or-line 'comment-or-uncomment-region))

(defun gzt/kill-ring-save ()
  "Save the region, or line if mark is not active, as if killed,
  but don't kill it."
  (interactive)
  (gzt/apply-region-or-line 'kill-ring-save))

(defun gzt/kill-region ()
  "Kill the region, or current line if mark is not active and
  save it to the kill ring"
  (interactive)
  (gzt/apply-region-or-line 'kill-region))

(defun gzt/popup (title msg &optional icon sound)
  "Show a popup if on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context. Optionally, ICON and SOUND
could be customized, default null"
  (interactive)
  (if (display-graphic-p)
      (shell-command (concat "notify-send "
                             (if icon (concat "-i " icon) "")
                             " '" title "' '" msg "'"))
    (message (concat title ": " msg)))
  (when sound (shell-command
               (concat "mplayer -really-quiet " sound " 2> /dev/null"))))

(provide 'gzt)
