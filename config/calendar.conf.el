;;; calendar.conf.el
;;; Time-stamp: <2012-12-14 12:09:39 CST gongzhitaao>

(require 'appt)

(add-hook 'calendar-load-hook
          (lambda ()
            (calendar-mark-holidays)
            (diary-mark-entries)))

(add-hook 'calendar-today-visible-hook
          (lambda ()
            (calendar-mark-today)))

(add-hook 'diary-list-entries-hook
          (lambda ()
            (diary-sort-entries)))

(setq diary-file "~/Documents/dotemacs/diary")

(calendar-set-date-style 'iso)

(provide 'calendar.conf)
