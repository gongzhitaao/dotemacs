;;; org.conf.el
;;; Time-stamp: <2013-12-18 16:42:15 CST gongzhitaao>

(require 'org-install)
(require 'org)

(add-hook 'org-mode-hook
          (lambda ()
            "Do some inits after org loaded"

            ;; prevent unindented adding or removal of agenda files
            (local-unset-key (kbd "C-c ["))
            (local-unset-key (kbd "C-c ]"))
            (local-unset-key (kbd "C-c ;"))

            ;; load useful modules
            (let ((my-org-modules
                   '(org-bbdb
                     org-bibtex
                     org-clock
                     org-docview
                     org-habit
                     org-special-blocks)))
              (dolist (m my-org-modules)
                (add-to-list 'org-modules m t))
              (dolist (m org-modules)
                (require m)))

            (auto-fill-mode 1)))

;; -------------------------------------------------------------------
;; Miscellaneous
;; -------------------------------------------------------------------
(setq org-time-stamp-custom-formats
      '("<%m/%d/%y %a>" . "<%Y-%m-%d %a %R %z>"))

(setq org-hierarchical-todo-statistics nil)
(setq org-agenda-include-diary t)
(setq org-clock-idle-time 10)
(setq org-directory (concat my-emacs-root "emacs.d/org"))
(setq org-use-property-inheritance t)

;; -------------------------------------------------------------------
;; TODO
;; -------------------------------------------------------------------
(setq org-use-fast-todo-selection t
      org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d@)")
        (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "KILL(k@)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "cyan" :weight bold)
        ("DONE" :foreground "green" :weight bold)
        ("WAIT" :foreground "yellow" :weight bold)
        ("HOLD" :foreground "magenta" :weight bold)
        ("KILL" :foreground "forest green" :weight bold)))

;; -------------------------------------------------------------------
;; Agenda
;; -------------------------------------------------------------------
(setq org-agenda-files "~/Documents/dotemacs/emacs.d/org/orgfile")

(setq org-agenda-dim-blocked-task t)
(setq org-agenda-compact-blocks t)

(setq org-agenda-repeating-timestamp-show-all t)
(setq org-agenda-show-all-dates t)

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (timeline . "  % s")
        (todo . " %i %-12:T")
        (tags . " %i %-12:T")
        (search . " %i %-12:T")))

(setq org-agenda-tags-column -100
      org-habit-graph-column 45
      org-habit-preceding-days 28
      org-habit-following-days 1
      org-agenda-start-with-log-mode t
      org-tags-column -70)

;; (setq org-agenda-category-icon-alist
;;       '(("Project" "~/Documents/dotemacs/config/img/icon-repeat.png")
;; ))

;; -------------------------------------------------------------------
;; Clock
;; -------------------------------------------------------------------
;; (org-clock-persistence-insinuate t)

(setq org-clock-history-length 32
      org-clock-in-resume t)

;; -------------------------------------------------------------------
;; Log thing
;; -------------------------------------------------------------------
(setq org-log-into-drawer t
      org-clock-into-drawer t)

;; -------------------------------------------------------------------
;; Tags
;; -------------------------------------------------------------------
(setq org-use-fast-tag-selection nil)

;; -------------------------------------------------------------------
;; Capture
;; -------------------------------------------------------------------
(setq org-capture-templates
      '(("t" "New TODO" entry
         (file+headline "todo.org" "Tasks")
         "* %^{Title} %^G\n  %u\n  %?\n\n\n")
        ("j" "New journal" entry
         (file+datetree (format "~/Documents/dailydigest/%s.org" (format-time-string "%Y")))
         "* %^{Brief} %^G\n%?\n\n")))

;; -------------------------------------------------------------------
;; Publishing
;; -------------------------------------------------------------------
(setq org-publish-project-alist
      '(("poj"
         :base-directory "~/Documents/oj/gh-pages/report/poj"
         :publishing-directory "~/Documents/oj/gh-pages/_includes/poj"
         :recursive t
         :section-numbers t
         :skip-before-1st-heading nil
         :table-of-contents nil
         :body-only t
         :sub-superscript {}
         :email "me@gongzhitaao.org")
        ("uva"
         :base-directory "~/Documents/oj/gh-pages/report/uva"
         :publishing-directory "~/Documents/oj/gh-pages/_includes/uva"
         :recursive t
         :section-numbers t
         :skip-before-1st-heading nil
         :table-of-contents nil
         :body-only t
         :sub-superscript {}
         :email "me@gongzhitaao.org")))

;; -------------------------------------------------------------------
;; BBDB thing
;; -------------------------------------------------------------------
(setq bbdb-file "contacts.bbdb"
      bbdb-north-american-phone-numbers-p nil)

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

(provide 'org.conf)
