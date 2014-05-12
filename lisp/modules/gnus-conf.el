;;; gnus.conf.el
;;; Time-stamp: <2014-05-12 09:45:41 CDT gongzhitaao>

(require 'gnus)

(setq user-full-name "Zhitao Gong")
(setq user-mail-address "zzg0009@auburn.edu")

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
  (let* ((now (float-time (current-time)))
         (one-day (* 24 60 60))
         (one-week (* 7 24 60 60))
         (header-date-time (float-time (safe-date-to-time
                                        (mail-header-date header))))
         (header-date-time-string (format-time-string
                                   "%Y-%m-%d %H:%M"
                                   (seconds-to-time header-date-time)))
         (mail-age (- now header-date-time)))
    (cond
     ((<= mail-age one-day) 0)
     ((<= mail-age one-week) 1)
     (t 2))))

(copy-face 'default 'my-date-one-day-old-face)
(copy-face 'default 'my-date-one-week-old-face)
(copy-face 'default 'my-date-more-than-one-week-old-face)
(set-face-foreground 'my-date-one-day-old-face "#ADFF2F")
(set-face-foreground 'my-date-one-week-old-face "#79B221")
(set-face-foreground 'my-date-more-than-one-week-old-face "#456613")

(copy-face 'default 'my-author-one-day-old-face)
(copy-face 'default 'my-author-one-week-old-face)
(copy-face 'default 'my-author-more-than-one-week-old-face)
(set-face-foreground 'my-author-one-day-old-face "#FFD700")
(set-face-foreground 'my-author-one-week-old-face "#B29600")
(set-face-foreground 'my-author-more-than-one-week-old-face "#665600")

(defun gnus-user-format-function-color-date (header)
  (let ((header-date-time-string (format-time-string
                                  "%Y-%m-%d %H:%M"
                                  (safe-date-to-time
                                   (mail-header-date header))))
        (age-level (header-age-level header)))
    (case age-level
      (0 (propertize header-date-time-string
                     'face 'my-date-one-day-old-face
                     'gnus-face t))
      (1 (propertize header-date-time-string
                     'face 'my-date-one-week-old-face
                     'gnus-face t))
      (otherwise
       (propertize header-date-time-string
                   'face 'my-date-more-than-one-week-old-face
                   'gnus-face t)))))

(defun extract-author (str)
  "Extract author from a string formated as `Author Name <email
address>'"
  (let ((author (s-trim (substring str 0 (string-match "<" str))
                        "[[:space:]\"]+")))
    (if (string= "" author)
        str
      author)))

(defun gnus-user-format-function-color-author (header)
  (let ((header-author-string
         (extract-author (mail-header-from header)))
        (age-level (header-age-level header)))
    (case age-level
      (0 (propertize header-author-string
                     'face 'my-author-one-day-old-face
                     'gnus-face t))
      (1 (propertize header-author-string
                     'face 'my-author-one-week-old-face
                     'gnus-face t))
      (otherwise
       (propertize header-author-string
                   'face 'my-author-more-than-one-week-old-face
                   'gnus-face t)))))

(setq-default
 ;; gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
 ;; gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-line-format "%U%R%z %(%u&color-date;  %-30,30u&color-author;  %B%s%)\n"
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

(setq message-confirm-send t)

(setq message-signature-directory
      (expand-file-name "signature" my-personal-dir))

(setq gnus-parameters
      '(("Tiger.*"
         (charset . utf-8)
         (posting-style
          (address "zzg0009@auburn.edu")
          (name "Zhitao Gong <鞏志燾>")
          (signature-file "tiger")
          (organization "Department of Computer Science & \
Software Engineering")))
        ("Gmail.*"
         (charset . utf-8)
         (posting-style
          (address "zhitaao.gong@gmail.com")
          (name "Zhitao")
          (signature-file "gmail")
          (organization "Auburn University")))))

(setq gnus-permanently-visible-groups
      "\\(Tiger\\|Gmail\\)/INBOX\\'")

(add-hook 'message-send-hook 'ispell-message)
(add-hook 'mail-send-hook  'ispell-message)

(gnus-demon-add-handler 'gnus-group-get-new-news 2 t)
(gnus-demon-init)

(add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)

(setq send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "msmtp")

(add-hook 'gnus-article-prepare-hook 'gnus-article-date-local)
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

(setq message-forward-ignored-headers
      "^Received:\\|^Content-Transfer-Encoding:\\|^DKIM-\\|^X-\\|^Message-ID:\\|^Received-\\|^In-Reply-To\\|References:\\|^Return-Path:")

(setq mail-user-agent 'gnus-user-agent)
(setq read-mail-command 'gnus)

(setq mm-text-html-renderer 'w3m)
(setq mm-inline-large-images t)
(auto-image-file-mode t)
(setq mm-inline-text-html-with-images t)
(setq mm-w3m-safe-url-regexp nil)

(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-message)

;; When I hit "forward as mail", Gnus will forward articles as inline
;; content (that is, part of the message), not as MIME.
(setq message-forward-as-mime nil)

(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (hl-line-mode 1)))
(add-hook 'gnus-group-mode-hook
          (lambda ()
            (hl-line-mode 1)))

(provide 'gnus-conf)
