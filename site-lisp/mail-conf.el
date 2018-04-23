;;; mail-conf.el --- mail config

;;; Commentary:
;; Setup mu4e, dependencies include message from Gnus (compose), mu4e-alert
;; (destop alert new emails), msmtp (sending mails), BBDB (contact database).

;;; Code:

(setq user-mail-address "zhitaao.gong@gmail.com"
      user-full-name "Zhitao Gong")

;; -----------------------------------------------------------------------------
;; Core settings
;; -----------------------------------------------------------------------------

(require 'mu4e)

(setq mu4e-maildir (expand-file-name "Mail" me-home))

(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "Personal"
          :match-func
          (lambda (msg)
            (when msg (string-prefix-p
                       "/Personal" (mu4e-message-field msg :maildir))))
          :vars
          `((mu4e-trash-folder . "/Personal/trash")
            (mu4e-sent-folder . "/Personal/sent")
            (mu4e-drafts-folder . "/Personal/draft")
            (mu4e-refile-folder . "/Personal/archive")
            (user-mail-address . "zhitaao.gong@gmail.com")
            (mu4e-sent-messages-behavior . 'delete)
            (message-signature-file . ,(expand-file-name "signature/personal"
                                                         me-emacs-data))))
        ,(make-mu4e-context
          :name "Tiger"
          :match-func
          (lambda (msg)
            (when msg (string-prefix-p
                       "/Tiger" (mu4e-message-field msg :maildir))))
          :vars
          `((mu4e-trash-folder . "/Tiger/trash")
            (mu4e-sent-folder . "/Tiger/sent")
            (mu4e-drafts-folder . "/Tiger/draft")
            (mu4e-refile-folder . "/Tiger/archive")
            (user-mail-address . "zzg0009@auburn.edu")
            (message-signature-file . ,(expand-file-name "signature/tiger"
                                                         me-emacs-data))))))

(setq mu4e-compose-dont-reply-to-self t)
(setq mu4e-user-mail-address-list
      '("zzg0009@auburn.edu" "zzg0009@tigermail.auburn.edu" "gong@auburn.edu"
        "zhitaao.gong@gmail.com"
        "gongzhitaao@google.com" "gongzhitaao@fb.com"))

(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy nil)
(setq mu4e-view-mode-hook '(bbdb-mua-auto-update))
(setq mu4e-compose-complete-addresses t)

(defun me//process-sent-messages ()
  "Post-process sent messages based on email address.

Gmail/IMAP takes care of copying sent messages to sent folder.
So we just delete it locally."
  (if (string-match-p "\\(@gmail\\.com\\)" (message-sendmail-envelope-from))
      'delete 'sent))
(setq mu4e-sent-messages-behavior #'me//process-sent-messages)

(add-to-list 'mu4e-bookmarks
             '("flag:flagged AND NOT flag:trashed" "Flagged messages" ?f))

;; -------------------------------------------------------------------
;; BBDB contacts
;; -------------------------------------------------------------------

(use-package bbdb
  :config
  (bbdb-initialize 'gnus 'message 'anniv 'mu4e)

  (setq bbdb-complete-mail-allow-cycling t
        bbdb-allow-duplicates t
        bbdb-message-all-addresses t
        bbdb-file (expand-file-name "contacts.bbdb.gz" me-emacs-data))

  (setq bbdb-mail-user-agent 'message-user-agent)
  (add-hook 'message-setup-hook 'bbdb-mail-aliases))

;; -----------------------------------------------------------------------------
;; visual look
;; -----------------------------------------------------------------------------

(setq mu4e-use-fancy-chars t)

(setq mu4e-headers-has-child-prefix    '("+"  . "○╮")
      mu4e-headers-empty-parent-prefix '("-"  . "● ")
      mu4e-headers-first-child-prefix  '("\\" . "╰─")
      mu4e-headers-duplicate-prefix    '("="  . "≡ ")
      mu4e-headers-default-prefix      '("|"  . "● "))

(setq mu4e-headers-draft-mark     '("D" . "℮")
      mu4e-headers-flagged-mark   '("F" . "⚐")
      mu4e-headers-new-mark       '("N" . "⋆")
      mu4e-headers-passed-mark    '("P" . "⟫")
      mu4e-headers-replied-mark   '("R" . "")
      mu4e-headers-seen-mark      '("S" . "░")
      mu4e-headers-trashed-mark   '("T" . "✖")
      mu4e-headers-attach-mark    '("a" . "◥")
      mu4e-headers-encrypted-mark '("x" . "")
      mu4e-headers-signed-mark    '("s" . "☡")
      mu4e-headers-unread-mark    '("u" . "█"))

(let ((marks '((refile  . ("r" . "▶"))
               (delete  . ("D" . "✖"))
               (flag    . ("+" . "⚑"))
               (move    . ("m" . "▷"))
               (read    . ("!" . "░"))
               (trash   . ("d" . "♣"))
               (unflag  . ("-" . "⚐"))
               (untrash . ("=" . "♧"))
               (unread  . ("?" . "█"))
               (unmark  . (" " . " "))
               (action  . ("a" . "◯"))
               (something . ("*" . "♯")))))
  (dolist (elm marks)
    (plist-put (alist-get (car elm) mu4e-marks) :char (cdr elm))))

(setq mu4e-headers-from-or-to-prefix '("" . "▶ "))

(setq mu4e-headers-fields '((:human-date . 18)
                            (:flags . 10)
                            (:from-or-to . 30)
                            (:thread-subject)))

(set-face-foreground 'mu4e-context-face "dark green")
(set-face-foreground 'mu4e-modeline-face "DarkOrange4")
(set-face-background 'mu4e-header-highlight-face "black")

;; -----------------------------------------------------------------------------
;; Message buffer
;; -----------------------------------------------------------------------------

(setq message-forward-ignored-headers ""
      message-make-forward-subject-function 'message-forward-subject-fwd)

(setq message-forward-as-mime nil)
(setq message-forward-before-signature nil)

(setq message-citation-line-function #'message-insert-formatted-citation-line)
(setq message-citation-line-format "On %a, %b %d %Y at %R, %N wrote:\n")

(defun me//init-message ()
  "Init setup for message mode."
  (turn-on-orgstruct)
  (turn-on-orgstruct++)
  (turn-on-orgtbl)
  (setq fill-column 80))

(add-hook 'message-mode-hook #'me//init-message)

;; -----------------------------------------------------------------------------
;; sending mails
;; -----------------------------------------------------------------------------

(add-hook 'message-send-hook 'ispell-message)
(add-hook 'mail-send-hook  'ispell-message)

(setq send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "msmtp")

;; -----------------------------------------------------------------------------
;; alert
;; -----------------------------------------------------------------------------

(setq display-time-mail-string " "
      display-time-use-mail-icon nil)

;; (setq mu4e-get-mail-command "true")
(setq mu4e-index-cleanup nil
      mu4e-index-lazy-check t)

(setq mu4e-update-interval 300)
(require 'mu4e-alert)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
(mu4e-alert-set-default-style 'notifications)

;; -----------------------------------------------------------------------------
;; startup
;; -----------------------------------------------------------------------------

(defun me//seconds-from-now (interval &optional wait)
  "Calculate INTERVAL seconds from now."
  (let* ((m (mod (string-to-int (format-time-string "%M")) interval))
         (s (string-to-int (format-time-string "%S")))
         (elapsed (+ (* m 60) s))
         (w (or wait 30)))
    (if (< elapsed (- w 15))
        w
      (- (+ (* interval 60) w) elapsed))))

(defun me//start-mu4e-bg ()
  "Start mu4e in background."
  (mu4e t))
(run-at-time (me//seconds-from-now 5) nil #'me//start-mu4e-bg)

(provide 'mail-conf)

;;; mail-conf.el ends here