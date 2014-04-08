
;; -------------------------------------------------------------------
;; Appt
;; -------------------------------------------------------------------
(appt-activate 1)

(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

(defun my-appt-display (mins-till-appt cur-time msg)
  "Convinient wrapper for appt popup display"
  (my-popup (format "Appointment in %s minute(s)" mins-till-appt) msg
             "/usr/share/icons/gnome/32x32/status/appointment-soon.png"
             "/usr/share/sounds/ubuntu/stereo/phone-incoming-call.ogg"))

(setq appt-disp-window-function (function my-appt-display))
