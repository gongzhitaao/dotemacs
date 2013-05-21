;;; org.conf.el
;;; Time-stamp: <2013-05-19 20:54:45 CDT gongzhitaao>

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

            (auto-fill-mode 1)
            ))

;; -------------------------------------------------------------------
;; TODO
;; -------------------------------------------------------------------
(setq org-use-fast-todo-selection t
      org-treat-S-cursor-todo-selection-as-state-change nil
      )

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
(setq org-agenda-files
      '("~/Documents/org/gtd/todo.org"
        "~/Documents/org/gtd/habits.org"
        ))

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
      org-tags-column -70
      )

;; (setq org-agenda-category-icon-alist
;;       '(("Project" "~/Documents/dotemacs/config/img/icon-repeat.png")
;; ))

;; -------------------------------------------------------------------
;; Clock
;; -------------------------------------------------------------------
;; (org-clock-persistence-insinuate t)

(setq org-clock-history-length 32
      org-clock-in-resume t
      )

;; -------------------------------------------------------------------
;; Log thing
;; -------------------------------------------------------------------
(setq org-log-into-drawer t
      org-clock-into-drawer t
      )

;; -------------------------------------------------------------------
;; Tags
;; -------------------------------------------------------------------
(setq org-use-fast-tag-selection nil)

;; -------------------------------------------------------------------
;; Capture
;; -------------------------------------------------------------------
(setq org-capture-templates
      '(("n" "Note" entry (file "~/Documents/org/gtd/notes.org")
         "* %? :note:%^G\n%U\n")
        ("t" "Todo" entry
         (file+headline "~/Documents/org/gtd/todo.org" "Tasks")
         "* TODO %? %^G\n")
        ("w" "Wish todo" entry (file "~/Documents/org/gtd/someday.org")
         "* TODO %? %^G\n%U\n")
        ))

;; -------------------------------------------------------------------
;; Publishing
;; -------------------------------------------------------------------
(setq org-publish-project-alist
      '(("oj"
         :base-directory "~/Documents/org/oj/"
         :publishing-directory "~/Documents/oj/gh-pages/_posts/"
         :recursive t
         :secion-numbers t
         :skip-before-1st-heading nil
         :table-of-contents t
         :sub-superscript {}
         :email "zhitaao.gong@gmail.com"
         :publishing-function org-publish-org-to-jekyll
                                        ; custome properties
         :jekyll-layout "post")
        ))

;; -------------------------------------------------------------------
;; BBDB thing
;; -------------------------------------------------------------------
(setq bbdb-file "~/Documents/org/gtd/contacts.bbdb"
      bbdb-north-american-phone-numbers-p nil
      )

;; -------------------------------------------------------------------
;; Miscellaneous
;; -------------------------------------------------------------------
(setq org-time-stamp-custom-formats
      '("<%m/%d/%y %a>" . "<%Y-%m-%d %a %R %z>"))

(setq org-hierarchical-todo-statistics nil)
(setq org-directory "~/Documents/org/gtd")
(setq org-agenda-include-diary t)
(setq org-clock-idle-time 10)

(provide 'org.conf)
