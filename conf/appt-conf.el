;;; appt-conf.el
;;; Time-stamp: <2015-05-05 20:15:01 CDT gongzhitaao>

(appt-activate 1)
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

(defun my-appt-display (mins-till-appt cur-time msg)
  "Convinient wrapper for appt popup display"
  (notifications-notify
   :title (format "Appt in %s minute(s)" mins-till-appt)
   :body msg
   :app-icon (expand-file-name "appointment-soon.png" my-icons-dir)))

(setq appt-disp-window-function (function my-appt-display))

;;; appt-conf.el ends here
