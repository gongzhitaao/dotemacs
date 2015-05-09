;;; ibuffer-conf.el
;;; Time-stamp: <2015-05-05 20:53:38 CDT gongzhitaao>

(require 'ibuffer)

(setq ibuffer-saved-filter-groups
      `(("default"
         ("Planner"
          (or (mode . org-agenda-mode)
              (filename . "/home/gongzhitaao/Dropbox/emacs/personal/org/")
              (name . "\\.bbdb")
              (mode . bbdb-mode)
              (name . "^\\*Calendar\\*$")
              (name . "^diary$")))
         ("Dired" (mode . dired-mode))
         ("Web"
          (or (name . "\\.js")
              (name . "\\.css")
              (name . "\\.html")
              (name . "\\.php")
              (name . "\\.xml")
              (mode . yaml-mode)))
         ("Text"
          (or (name . "\\.\\(tex\\|bib\\|csv\\)")
              (mode . org-mode)
              (mode . markdown-mode)
              (mode . text-mode)))
         ("Data"
          (or (mode . gnuplot-mode)
              (mode . octave-mode)
              (mode . R-mode)))
         ("Coding"
          (or (mode . shell-script-mode)
              (mode . sh-mode)
              (mode . emacs-lisp-mode)
              (name . "\\.[ch]\\(pp\\|xx\\|\\+\\+\\)?")
              (mode . python-mode)
              (name . "\\.ya?ml")
              (name . "\\.sql")))
         ("Mail"
          (or (mode . message-mode)
              (mode . mail-mode)
              (mode . gnus-group-mode)
              (mode . gnus-summary-mode)
              (mode . gnus-article-mode)
              (mode . gnus-server-mode)
              (mode . gnus-browse-mode)
              (name . "^\\.newsrc-dribble")))
         ("Console"
          (or (mode . inferior-ess-mode)
              (mode . inferior-python-mode)
              (mode . eshell-mode)
              (mode . gnuplot-comint-mode)
              (mode . comint-mode)))
         ("Helper"
          (or (mode . makefile-mode)
              (mode . makefile-gmake-mode)
              (mode . cmake-mode)
              (mode . calc-mode)
              (mode . Info-mode)
              (mode . help-mode)
              (mode . ess-help-mode)
              (name . "^\\*scratch\\*$"))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")
            (local-set-key (kbd "<right>") 'ibuffer-forward-filter-group)
            (local-set-key (kbd "<left>") 'ibuffer-backward-filter-group)
            (substitute-key-definition 'ibuffer-find-file 'ido-find-file (current-local-map))
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

;;; ibuffer-conf.el ends here
