
;;; myorg.el
;;; Time-stamp: <2012-10-17 22:28:54 gongzhitaao>

(require 'org-install)
(require 'org)

(setq org-modules '(org-bbdb
                    org-bibtex
                    org-crypt
                    org-docview
                    org-gnus
                    org-habit
                    org-id
                    org-info
                    org-jsinfo
                    org-inlinetask
                    org-irc
                    org-mew
                    org-mhe
                    org-rmail
                    org-vm
                    org-w3m
                    org-wl))

(add-hook 'org-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-c ["))
            (local-unset-key (kbd "C-c ]"))
            (local-unset-key (kbd "C-c ;"))))

;; ----------------------------------------------------------------------
;; Agenda
;; ----------------------------------------------------------------------
(setq org-agenda-files
      '("~/Documents/org/todo.org"
        "~/Documents/org/habits.org"
        "~/Documents/org/someday.org"
))

(setq org-agenda-dim-blocked-task t)
(setq org-agenda-compact-blocks t)

(setq org-agenda-repeating-timestamp-show-all t)
(setq org-agenda-show-all-dates t)

(setq org-agenda-custom-commands
      '(("d" "Done" todo "DONE"
        ((org-agenda-overriding-header "To archive")
         (org-tags-match-list-sublevels t)))
        ("0" "Agenda"
         ((agenda "" nil)
          (tags-todo "STYLE=\"habit\""
                     ((org-agenda-overriding-header "Habits")
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
))))

;; ----------------------------------------------------------------------
;; Clock
;; ----------------------------------------------------------------------
;; (org-clock-persistence-insinuate t)

(setq org-clock-history-length 32)
(setq org-clock-in-resume t)

;; ----------------------------------------------------------------------
;; Log thing
;; ----------------------------------------------------------------------
(setq org-log-into-drawer t)
(setq org-clock-into-drawer t)

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
(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

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
      '(("n" "Note" entry (file "~/Documents/org/notes.org")
         "* %? :note:%^G\n%U\n")
        ("t" "Todo" entry
         (file+headline "~/Documents/org/todo.org" "Tasks")
         "* TODO %? %^G\n%U\n")
        ("w" "Wish todo" entry (file "~/Documents/org/someday.org")
         "* TODO %? %^G\n%U\n")
))

;; ----------------------------------------------------------------------
;; Publishing
;; ----------------------------------------------------------------------
(require 'org-publish)
(let ((post-src "~/Documents/org/posts/")
      (post-des "~/Documents/gongzhitaao.github.com/_posts/")
      (image-src "~/Documents/org/posts/img/")
      (image-des "~/Documents/gongzhitaao.github.com/assets/img/"))

  (setq org-publish-project-alist
        '(("post"
           :base-directory post-src
           :publishing-directory post-des
           :publishing-function org-publish-org-to-html
           :secion-numbers t
           :table-of-contents t
           :sub-superscript "{}"
           :email "zhitaao.gong@gmail.com"
           :style "<link rel=\"stylesheet\" type=\"text/css\"\
href=\"/assets/css/org.css\" />")

          ("image"
           :base-directory image-src
           :base-extension "jpg\\|gif\\|png"
           :publishing-directory image-des
           :publishing-function org-publish-attachment)

          ("blog" :components ("post" "image"))
)))

;; ----------------------------------------------------------------------
;; BBDB thing
;; ----------------------------------------------------------------------
(setq bbdb-file "~/Documents/org/contacts.bbdb")
(setq bbdb-north-american-phone-numbers-p nil)

;; ----------------------------------------------------------------------
;; Miscellaneous
;; ----------------------------------------------------------------------

(setq org-time-stamp-custom-formats
      '("<%m/%d/%y %a>" . "<%Y-%m-%d %a %R %z>"))

(provide 'myorg)
