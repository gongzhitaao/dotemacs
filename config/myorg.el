
;;; myorg.el
;;; Time-stamp: <2012-09-24 10:11:09 gongzhitaao>

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)")
	(sequence "PROPOSE(p)" "WORKING(w!)" "|" "FINISHED(f!)")
	(sequence "|" "CANCELED(c@)")))

(require 'remember)
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(defun find-journal-file ()
  (let (journal-file journal-header)
    (setq journal-file (format "~/Documents/org/journal/%s.org" (format-time-string "%Y-%m")))
    (unless (file-exists-p journal-file)
      (setq journal-header "\
# -*- mode: org -*-\n\
#+STARTUP: overview\n\
#+TAGS: BOOKS(b) DIARY(d) IDEAS(i)\n\
#+STARTUP: hidestars")
      (shell-command (format "echo \"%s\">%s" journal-header journal-file)))
    journal-file))

(setq org-remember-templates
      '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U"
	 "~/Document/org/gtd.org" "Tasks")
	("Journal" ?j "* %^{Head Line} %U %^g\n%l%?"
	 (function find-journal-file))
	("Book" ?b "\n* %^{Book Title} %t :READING:\n%[~/Document/org/booktemp]\n"
	 (function find-journal-file))
	("Diary" ?d "")))

(provide 'myorg)
