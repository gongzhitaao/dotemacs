
;;; myorg.el
;;; Time-stamp: <2012-09-26 18:57:15 gongzhitaao>

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

(setq org-agenda-files '("~/Documents/org/"))

(setq org-agenda-repeating-timestamp-show-all t)
(setq org-agenda-show-all-dates t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)")
        (sequence "PROPOSE(p)" "WORKING(w!)" "|" "FINISHED(f!)")
        (sequence "|" "CANCELED(c@)")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
	(sequence "WAITING(w@/!)" "HOLD(h@/!)" "|"
		  "CANCELLED(c@/!)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("NEXT" :foreground "blue" :weight bold)
	("DONE" :foreground "forest green" :weight bold)
	("WAITING" :foreground "orange" :weight bold)
	("HOLD" :foreground "magenta" :weight bold)
	("CANCELLED" :foreground "forest green" :weight bold)
	("PHONE" :foreground "forest green" :weight bold)))

(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
	("WAITING" ("WAITING" . t))
	("HOLD" ("WAITING" . t) ("HOLD" . t))
	(done ("WAITING") ("HOLD"))
	("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
	("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
	("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

(setq org-capture-templates
      '(("j" "Journal" entry (file+datetree "~/Documents/org/diary.org")
         "* %?\n%U\n" :clock-in t :clock-resume t)
	("n" "Note" entry (file "~/Documents/org/notes.org")
	 "* %? :NOTE:\n%U\n" :clock-in t :clock-resume t)
        ("t" "Todo" entry (file+headline "~/Documents/org/todo.org" "Tasks")
	 "** TODO %?\n%U\n" :clock-in t :clock-resume t)))

;; BBDB thing
(setq bbdb-file "~/Documents/org/contacts.bbdb")
(setq bbdb-north-american-phone-numbers-p nil)

(provide 'myorg)
