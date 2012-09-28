
;;; myorg.el
;;; Time-stamp: <2012-09-28 22:01:01 gongzhitaao>

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

(setq org-agenda-files
      '("~/Documents/org/todo.org"
        "~/Documents/org/habits.org"
        "~/Documents/org/someday.org"
))

(add-hook 'org-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-c ["))
            (local-unset-key (kbd "C-c ]"))
            (local-unset-key (kbd "C-c ;"))))

(setq org-agenda-repeating-timestamp-show-all t)
(setq org-agenda-show-all-dates t)
(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-use-fast-tag-selection nil)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
        (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "TERM(e@/!)")))

(setq org-tag-alist
      '((:startgroup)
        ("@computer" . ?C)
        ("@afk" . ?A)
        (:endgroup)
        ("HOLD" . ?H)
        ("NEXT" . ?N)
        ("WAIT" . ?W)
        ("TERM" . ?T)
        ("code" . ?c)
        ("hobby" . ?h)
        ("social" . ?s) ; social activities, phone friends and etc.
        ("tianer" . ?t) ; tianer-related tasks
))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "cyan" :weight bold)
        ("DONE" :foreground "green" :weight bold)
        ("WAIT" :foreground "yellow" :weight bold)
        ("HOLD" :foreground "magenta" :weight bold)
        ("TERM" :foreground "forest green" :weight bold)))

(setq org-todo-state-tags-triggers
      '(("TODO" ("WAIT") ("TERM") ("HOLD"))
        ("NEXT" ("WAIT") ("TERM") ("HOLD"))
        ("DONE" ("WAIT") ("TERM") ("HOLD"))
        ("WAIT" ("WAIT" . t))
        ("HOLD" ("WAIT" . t) ("HOLD" . t))
        ("TERM" ("TERM" . t))
        (done ("WAIT") ("HOLD"))
))

(setq org-capture-templates
      '(("n" "Note" entry (file "~/Documents/org/notes.org")
         "* %? :note:%^G\n%U\n")
        ("t" "Todo" entry (file+headline "~/Documents/org/todo.org" "Tasks")
         "* TODO %? %^G\n%U\n%?")
        ("w" "Wish todo" entry (file "~/Documents/org/someday.org")
         "* TODO %? %^G\n%U\n%?")))

;; BBDB thing
(setq bbdb-file "~/Documents/org/contacts.bbdb")
(setq bbdb-north-american-phone-numbers-p nil)

(provide 'myorg)
