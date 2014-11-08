;;; gnus.conf.el
;;; Time-stamp: <2014-11-07 18:51:37 CST gongzhitaao>

(require 'gnus)
(require 'gnus-diary)

(setq user-full-name "Zhitao Gong")
(setq user-mail-address "me@gongzhitaao.org")

(setq gnus-init-file "/home/gongzhitaao/.emacs.d/lisp/modules/gnus-conf.el")

(setq gnus-select-method
      '(nnimap "LocalMail"
               (nnimap-address "localhost")
               (nnimap-stream network)
               (nnimap-server-port 143)))

(defun header-age-level (header)
  "Return the age of the header

The age are divided into three levels:
0: no more than one day old
1: no more than one week old
2: otherwise

Based on the age of the header, I set different foreground color
for the header string.
"
  (let* ((now (time-to-day-in-year (current-time)))
         (header-date-time
          (time-to-day-in-year (safe-date-to-time
                                (mail-header-date header))))
         (mail-age (- now header-date-time)))
    (cond
     ((< mail-age 1) 0)
     ((< mail-age 7) 1)
     (t 2))))

(defface my-date-one-day-old-face
  '((default (:foreground "#ADFF2F")))
  "...")
(defface my-date-one-week-old-face
  '((default (:foreground "#79B221")))
  "...")
(defface my-date-more-than-one-week-old-face
  '((default (:foreground "#456613")))
  "...")

(defun gnus-user-format-function-color-date (header)
  (let ((header-date-time-string
         (format-time-string
          "%Y-%m-%d %H:%M" (safe-date-to-time (mail-header-date header))))
        (age-level (header-age-level header)))
    (cond
      ((= 0 age-level)
       (propertize header-date-time-string
		   'face '(my-date-one-day-old-face default)
		   'gnus-face t))
      ((= 1 age-level)
       (propertize header-date-time-string
		   'face '(my-date-one-week-old-face default)
		   'gnus-face t))
      (t
       (propertize header-date-time-string
                   'face '(my-date-more-than-one-week-old-face default)
                   'gnus-face t)))))

(setq-default
 gnus-summary-line-format "%U%R%z %(%u&color-date;  %-30,30f  %B%s%)\n"
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))

(setq gnus-summary-to-prefix "→"
      gnus-summary-newsgroup-prefix "⇶"
      ;; Marks
      gnus-ancient-mark ?✓
      gnus-ticked-mark ?⚑
      gnus-dormant-mark ?⚐
      gnus-expirable-mark ?♻
      gnus-read-mark ?✓
      gnus-del-mark ?✗
      gnus-canceled-mark ?✗
      gnus-killed-mark ?☠
      gnus-replied-mark ?↺
      gnus-forwarded-mark ?↪
      gnus-cached-mark ?☍
      gnus-recent-mark ?✩
      gnus-unseen-mark ?★
      gnus-unread-mark ?✉
      gnus-score-over-mark ?↑           ; ↑ ☀
      gnus-score-below-mark ?↓         ; ↓ ☂
      gnus-sum-thread-tree-false-root " ◌ "
      gnus-sum-thread-tree-single-indent "◎ "
      gnus-sum-thread-tree-indent "   "
      gnus-sum-thread-tree-root "● "
      gnus-sum-thread-tree-leaf-with-other "├─▶ "
      gnus-sum-thread-tree-single-leaf     "└─▶ "
      gnus-sum-thread-tree-vertical        "│ ")

(setq gnus-gcc-mark-as-read t)
(setq gnus-face-properties-alist
      '((pbm . (:face gnus-x-face :ascent center))
        (png . (:ascent center))))
(setq gnus-treat-from-gravatar 'head)
(setq gnus-treat-mail-gravatar 'head)

(setq gnus-message-archive-group
      `(("Tiger" "nnimap+zzg0009@auburn.edu:Tiger/Sent Items")
        ("Gmail" "nnimap+zhitaao.gong@gmail.com:Gmail/[Gmail]/Sent Mail")
        (".*" ,(format-time-string "sent.%Y-%m"))))

(setq message-confirm-send t)

(setq message-signature-directory
      (expand-file-name "signature" my-personal-dir))

(setq gnus-parameters
      '(("Tiger.*"
         (charset . utf-8)
         (posting-style
          (address "zzg0009@auburn.edu")
          (name "Zhitao Gong")
          (signature-file "tiger")
          (organization "Department of Computer Science & \
Software Engineering")))
        ("Gmail.*"
         (charset . utf-8)
         (posting-style
          (address "zhitaao.gong@gmail.com")
          (name "Zhitao Gong")
          (signature-file "gmail")
          (organization "Auburn University")))))

(setq gnus-permanently-visible-groups
      (concat "^\\(Tiger\\|Gmail\\)/INBOX\\'\\|"
              "^Tiger/Sent Items\\'\\|"
              "^Gmail/\\[Gmail\\]/Sent Mail\\'\\|"
              "^archive\\'\\|"
              "^nndiary:Reminder\\'"))

(let ((my-mails (concat "\\(zhitaao\.gong@gmail\.com\\)\\|"
                       "\\(zzg0009@\\(tigermail\.\\)?auburn\.edu\\)\\|"
                       "\\(me@gongzhitaao\.org\\)")))
  (setq message-dont-reply-to-names my-mails))
(setq gnus-ignored-from-addresses "Zhitao\\( Gong\\)?")

(add-hook 'message-send-hook 'ispell-message)
(add-hook 'mail-send-hook  'ispell-message)

(gnus-demon-add-handler 'gnus-group-get-new-news 2 t)
(gnus-demon-init)

(add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)

(setq send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "msmtp")

(add-hook 'gnus-article-prepare-hook 'gnus-article-date-local)
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

(setq message-forward-ignored-headers ""
      message-make-forward-subject-function 'message-forward-subject-fwd
      message-forward-as-mime nil)

(add-to-list 'mm-discouraged-alternatives "text/html")
(add-to-list 'mm-discouraged-alternatives "image/.*")

(setq mail-user-agent 'gnus-user-agent)
(setq read-mail-command 'gnus)

(setq mm-text-html-renderer 'w3m)
(setq mm-inline-large-images nil)
(auto-image-file-mode t)
(setq mm-inline-text-html-with-images nil)
(setq mm-w3m-safe-url-regexp nil)

(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-message)
(add-hook 'message-mode-hook 'turn-on-orgstruct)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)
(add-hook 'message-mode-hook 'turn-on-orgtbl)

(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (hl-line-mode 1)))
(add-hook 'gnus-group-mode-hook
          (lambda ()
            (hl-line-mode 1)))

(provide 'gnus-conf)
