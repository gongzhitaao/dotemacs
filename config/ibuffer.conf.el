;;; ibuffer.conf.el
;;; Time-stamp: <2013-02-23 10:56:31 CST gongzhitaao>

(require 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Planner"
                (or (mode . org-agenda-mode)
                    (mode . org-mode)
                    (filename . "Documents/org/gtd/")
                    (mode . bbdb-mode)
                    (name . "^\\*Calendar\\*$")
                    (name . "^diary$")))
               ("Programming"
                (or (mode . emacs-lisp-mode)
                    (mode . c++-mode)
                    (mode . c-mode)
                    (mode . python-mode)
                    (mode . shell-script-mode)
                    (mode . html-mode)
                    (mode . markdown-mode)
                    (mode . javascript-mode)
                    (mode . js-mode)
                    (mode . css-mode)
                    (mode . yaml-mode)
                    (mode . xml-mode)
                    (mode . nxml-mode)))
               ("Dired" (mode . dired-mode))
               ("TeX"
                (or (mode . LaTeX-mode)
                    (mode . latex-mode)
                    (mode . tex-mode)
                    (mode . TeX-mode)
                    (mode . plain-tex-mode)
                    (mode . bibtex-mode)))
               ("Mail"
                (or (mode . message-mode)
                    (mode . mail-mode)
                    (mode . gnus-group-mode)
                    (mode . gnus-summary-mode)
                    (mode . gnus-article-mode)
                    (mode . gnus-server-mode)
                    (mode . gnus-browse-mode)
                    (name . "^\\.newsrc-dribble")))
               ("Helper"
                (or (mode . calc-mode)
                    (mode . info-mode)
                    (mode . Info-mode)
                    (mode . help-mode)
                    (name . "^\\*scratch\\*$")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")
            (local-set-key (kbd "<right>") 'ibuffer-forward-filter-group)
            (local-set-key (kbd "<left>") 'ibuffer-backward-filter-group)))

(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   (t (format "%8dB" (buffer-size)))))

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))

(provide 'ibuffer.conf)
