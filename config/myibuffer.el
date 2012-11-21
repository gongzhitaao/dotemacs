
;;; myibuffer.el
;; Time-stamp: <2012-11-20 21:39:16 CST gongzhitaao>

(require 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Planner" (or
                           (mode . org-agenda-mode)
                           (filename . "Documents/org/gtd/")
                           (mode . bbdb-mode)
                           (name . "^\\*Calendar\\*$")
                           (name . "^diary$")))
               ("Markup" (or
                            (mode . org-mode)
                            (mode . html-mode)
                            (mode . markdown-mode)))
               ("1st Lang" (or
                         (mode . c++-mode)
                         (mode . c-mode)
                         (mode . python-mode)))
               ("2st Lang" (or
                       (mode . javascript-mode)
                       (mode . js-mode)
                       (mode . shell-script-mode)
                       (mode . css-mode)))
               ("Emacs" (or
                         (mode . emacs-lisp-mode)
                         (name . "^\\*scratch\\*$")))
               ("(La)Tex" (or
                       (mode . LaTeX-mode)
                       (mode . tex-mode)
                       (mode . latex-mode)))
               ("Mail" (or
                        (mode . message-mode)
                        (mode . mail-mode)
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (mode . gnus-server-mode)
                        (mode . gnus-browse-mode)
                        (name . "^\\.newsrc-dribble")))
               ("Dired" (mode . dired-mode))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

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

(provide 'myibuffer)
