;;; helper.el --- Helper functions

(defun my-add-hooks (func hooks)
  "add FUNC to multiple HOOKs"
  (mapc (lambda (hook)
          (add-hook hook func))
        hooks))

(defun my-apply-region-or-line (func)
  "Apply FUNC to a region, or current line if mark is not
  active."
  (if (region-active-p)
      (funcall func (region-beginning) (region-end))
    (funcall func (line-beginning-position) (line-end-position))))

(defun my-popup (title msg &optional icon sound)
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
;;; helper.el ends here
