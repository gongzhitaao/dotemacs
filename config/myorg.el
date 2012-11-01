
;;; myorg.el
;;; Time-stamp: <2012-11-01 22:27:04 gongzhitaao>

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
                   '(org-bbdb org-bibtex org-docview org-habit)))
              (dolist (m my-org-modules)
                (add-to-list 'org-modules m t))
              (dolist (m org-modules)
                (require m)))
))

;; ----------------------------------------------------------------------
;; Agenda
;; ----------------------------------------------------------------------
(setq org-agenda-files
      '("~/Documents/org/gtd/todo.org"
        "~/Documents/org/gtd/habits.org"
))

(setq org-agenda-dim-blocked-task t)
(setq org-agenda-compact-blocks t)

(setq org-agenda-repeating-timestamp-show-all t)
(setq org-agenda-show-all-dates t)

(setq org-agenda-custom-commands
      '(("d" "Done" todo "DONE"
        ((org-agenda-overriding-header "To archive")
         (org-tags-match-list-sublevels nil)))
))

(setq org-agenda-tags-column -80
      org-habit-graph-column 84
      org-habit-preceding-days 28
      org-habit-following-days 0
)

;; ----------------------------------------------------------------------
;; Clock
;; ----------------------------------------------------------------------
;; (org-clock-persistence-insinuate t)

(setq org-clock-history-length 32
      org-clock-in-resume t
)

;; ----------------------------------------------------------------------
;; Log thing
;; ----------------------------------------------------------------------
(setq org-log-into-drawer t
      org-clock-into-drawer t
)

;; ----------------------------------------------------------------------
;; Tags
;; ----------------------------------------------------------------------
(setq org-use-fast-tag-selection nil)

(setq org-tag-alist
      '((:startgroup)
        ("@computer" . ?C)
        ("@afk" . ?A)
        (:endgroup)
        ("HOLD" . ?H)
        ("NEXT" . ?N)
        ("WAIT" . ?W)
        ("KILL" . ?K)
        ("code" . ?c)
        ("hobby" . ?h)
        ("social" . ?s) ; social activities, phone friends and etc.
        ("tianer" . ?t) ; tianer-related tasks
))

;; ----------------------------------------------------------------------
;; TODO
;; ----------------------------------------------------------------------
(setq org-use-fast-todo-selection t
      org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
        (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "KILL(k@/!)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "cyan" :weight bold)
        ("DONE" :foreground "green" :weight bold)
        ("WAIT" :foreground "yellow" :weight bold)
        ("HOLD" :foreground "magenta" :weight bold)
        ("KILL" :foreground "forest green" :weight bold)))

(setq org-todo-state-tags-triggers
      '(("TODO" ("WAIT") ("KILL") ("HOLD"))
        ("NEXT" ("WAIT") ("KILL") ("HOLD"))
        ("DONE" ("WAIT") ("KILL") ("HOLD"))
        ("WAIT" ("WAIT" . t))
        ("HOLD" ("WAIT" . t) ("HOLD" . t))
        ("KILL" ("KILL" . t))
        (done ("WAIT") ("HOLD"))
))

;; ----------------------------------------------------------------------
;; Capture
;; ----------------------------------------------------------------------
(setq org-capture-templates
      '(("n" "Note" entry (file "~/Documents/org/gtd/notes.org")
         "* %? :note:%^G\n%U\n")
        ("t" "Todo" entry
         (file+headline "~/Documents/org/gtd/todo.org" "Tasks")
         "* TODO %? %^G\n%U\n")
        ("w" "Wish todo" entry (file "~/Documents/org/gtd/someday.org")
         "* TODO %? %^G\n%U\n")
))

;; ----------------------------------------------------------------------
;; Publishing
;; ----------------------------------------------------------------------
(require 'org-jekyll)

(setq org-publish-project-alist
      '(("oj"
         :base-directory "~/Documents/org/oj/"
         :publishing-directory "~/Documents/oj/gh-pages/_posts/"
         :recursive t
         :secion-numbers t
         :skip-before-1st-heading nil
         :table-of-contents t
         :sub-superscript "{}"
         :email "zhitaao.gong@gmail.com"
         :publishing-function org-publish-org-to-jekyll
         ; custome properties
         :jekyll-layout "post")
))

;; ----------------------------------------------------------------------
;; BBDB thing
;; ----------------------------------------------------------------------
(setq bbdb-file "~/Documents/org/gtd/contacts.bbdb"
      bbdb-north-american-phone-numbers-p nil
)

;; ----------------------------------------------------------------------
;; Miscellaneous
;; ----------------------------------------------------------------------
(setq org-time-stamp-custom-formats
      '("<%m/%d/%y %a>" . "<%Y-%m-%d %a %R %z>"))

(provide 'myorg)
