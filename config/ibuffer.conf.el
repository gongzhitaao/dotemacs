;;; ibuffer.conf.el
;;; Time-stamp: <2013-09-26 17:19:01 CDT gongzhitaao>

(require 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Planner"
                (or (mode . org-agenda-mode)
                    (filename . "Documents/dotemacs/emacs.d/org/")
                    (mode . bbdb-mode)
                    (name . "^\\*Calendar\\*$")
                    (name . "^diary$")))
               ("Dired" (mode . dired-mode))
               ("Web"
                (or (mode . html-mode)
                    (mode . css-mode)
                    (name . "\\.js")
                    (name . "\\.php")))
               ("Coding"
                (or (mode . shell-script-mode)
                    (name . "\\.[ch]\\(pp\\|xx\\|\\+\\+\\)*")
                    (name . "\\.py")
                    (name . "\\.ya?ml")
                    (name . "\\.el")
                    (mode . emacs-lisp-mode)
                    (name . "\\.sql")
                    (name . "\\.m")))
               ("Text"
                (or (name . "\\.\\(tex\\|bib\\)")
                    (name . "\\.\\org")
                    (mode . markdown-mode)))
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
                (or (mode . makefile-mode)
                    (mode . cmake-mode)
                    (mode . calc-mode)
                    (mode . info-mode)
                    (mode . Info-mode)
                    (mode . help-mode)
                    (name . "^\\*scratch\\*$")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")
            (local-set-key (kbd "<right>") 'ibuffer-forward-filter-group)
            (local-set-key (kbd "<left>") 'ibuffer-backward-filter-group)
            (hl-line-mode 1)))

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
