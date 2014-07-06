;;; my-core.el --- Core functionality
;;; Time-stamp: <2014-06-26 17:04:08 CDT gongzhitaao>

;;; Code:

(defun s-trim-left (s &optional pat)
  "Remove whitespace or user defined PAT at the beginning of S."
  (let ((pat (if pat pat "\\`[[:space:]]+")))
    (if (string-match pat s)
        (replace-match "" t t s)
      s)))

(defun s-trim-right (s &optional pat)
  "Remove whitespace or user defined PAT at the end of S."
  (let ((pat (if pat pat "[[:space:]]+\\'")))
    (if (string-match pat s)
        (replace-match "" t t s)
      s)))

(defun s-trim (s &optional pat)
  "Remove whitespace or user defined pattern at the beginning and
end of S."
  (let ((pat (if pat pat "[[:space:]]+")))
    (s-trim-left (s-trim-right s (format "%s\\'" pat))
                 (format "\\`%s" pat))))

(defun my-apply-region-or-line (func)
  "Apply FUNC to a region, or current line if mark is not
  active."
  (if (region-active-p)
      (funcall func (region-beginning) (region-end))
    (funcall func (line-beginning-position) (line-end-position))))

(defun my-clear-shell ()
  (interactive)
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)))

(provide 'my-core)
;;; my-core.el ends here
