
;;; org.el

(setq org-log-done 'time)

(custom-set-variables
 '(org-directory "~/Document/org")
 '(org-agenda-files "~/Documents/org/todo.org")
 '(org-default-notes-file "~/Documents/org/notes.org")
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-notes-order t)
 '(org-use-fast-todo-selection t)
 '(org-treat-S-cursor-todo-selection-as-sate-change nil)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-remember-store-without-prompt t)

 '(org-todo-keywords
   (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
		   (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE"))))
 '(org-todo-keyword-faces
   (quote (("TODO" :foreground "red" :weight bold)
		   ("NEXT" :foreground "blue" :weight bold)
		   ("DONE" :foreground "forest green" :weight bold)
		   ("WAITING" :foreground "orange" :weight bold)
		   ("HOLD" :foreground "magenta" :weight bold)
		   ("CANCELLED" :foreground "forest green" :weight bold)
		   ("PHONE" :foreground "forest green" :weight bold))))
 '(org-todo-state-tags-triggers
	(quote (("CANCELLED" ("CANCELLED" . t))
			("WAITING" ("WAITING" . t))
			("HOLD" ("WAITING" . t) ("HOLD" . t))
			(done ("WAITING") ("HOLD"))
			("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
			("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
			("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
 '(org-capture-templates
   (quote (("t" "todo" entry (file "~/git/org/.org")
			"* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
		   ("r" "respond" entry (file "~/git/org/notes.org")
			"* TODO Respond to %:from on %:subject\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
		   ("n" "note" entry (file "~/git/org/refile.org")
			"* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
		   ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
			"* %?\n%U\n" :clock-in t :clock-resume t)
		   ("w" "org-protocol" entry (file "~/git/org/refile.org")
			"* TODO Review %c\n%U\n" :immediate-finish t)
		   ("p" "Phone call" entry (file "~/git/org/refile.org")
			"* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
		   ("h" "Habit" entry (file "~/git/org/refile.org")
			"* NEXT %?\n%U\n%a\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))))

(provide 'org)
