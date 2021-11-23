;;; mail-conf.el --- Reading mails                   -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defvar me-home)
(defvar me-emacs-data-private)

;; =============================================================================
;; mail
;; =============================================================================

(setq user-mail-address "zhitaao.gong@gmail.com"
      user-full-name "Zhitao Gong")

;; Core settings
;; -----------------------------------------------------------------------------

(defun me//process-sent-messages ()
  "Post-process sent messages based on email address.

Gmail/IMAP takes care of copying sent messages to sent folder.
So we just delete it locally."
  (if (string-match-p
       (regexp-opt '("@gmail.com" "@tigermail.auburn.edu" "@auburn.edu"))
       (message-sendmail-envelope-from))
      'delete 'sent))

(defvar display-time-mail-string)
(defvar display-time-use-mail-icon)
(setq display-time-mail-string " "
      display-time-use-mail-icon nil)

(eval-when-compile
  (require 'use-package))

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;; (use-package mu4e
;;   :config
;;   (setq mu4e-maildir (expand-file-name ".mail" me-home))

;;   (setq mu4e-contexts
;;         `(,(make-mu4e-context
;;             :name "Personal"
;;             :match-func
;;             (lambda (msg)
;;               (when msg (string-prefix-p
;;                          "/personal" (mu4e-message-field msg :maildir))))
;;             :vars
;;             `((mu4e-trash-folder . "/personal/trash")
;;               (mu4e-sent-folder . "/personal/sent")
;;               (mu4e-drafts-folder . "/personal/drafts")
;;               (mu4e-refile-folder . "/personal/archive")
;;               (user-mail-address . "zhitaao.gong@gmail.com")
;;               (message-signature-file . ,(expand-file-name "signature/personal"
;;                                                            me-emacs-data-private))))
;;           ,(make-mu4e-context
;;             :name "Work"
;;             :match-func
;;             (lambda (msg)
;;               (when msg (string-prefix-p
;;                          "/work" (mu4e-message-field msg :maildir))))
;;             :vars
;;             `((mu4e-trash-folder . "/work/trash")
;;               (mu4e-sent-folder . "/work/sent")
;;               (mu4e-drafts-folder . "/work/drafts")
;;               (mu4e-refile-folder . "/work/archive")
;;               (user-mail-address . "gongzhitaao@google.com")
;;               (message-signature-file . ,(expand-file-name
;;                                           "signature/work.google"
;;                                           me-emacs-data-private))))
;;           ,(make-mu4e-context
;;             :name "Tiger"
;;             :match-func
;;             (lambda (msg)
;;               (when msg (string-prefix-p
;;                          "/tiger" (mu4e-message-field msg :maildir))))
;;             :vars
;;             `((mu4e-trash-folder . "/tiger/trash")
;;               (mu4e-sent-folder . "/tiger/sent")
;;               (mu4e-drafts-folder . "/tiger/draft")
;;               (mu4e-refile-folder . "/tiger/archive")
;;               (user-mail-address . "zzg0009@auburn.edu")
;;               (message-signature-file . ,(expand-file-name "signature/tiger"
;;                                                            me-emacs-data-private))))
;;           ,(make-mu4e-context
;;             :name "Reg"
;;             :match-func
;;             (lambda (msg)
;;               (when msg (string-prefix-p
;;                          "/reg" (mu4e-message-field msg :maildir))))
;;             :vars
;;             `((mu4e-trash-folder . "/reg/trash")
;;               (mu4e-sent-folder . "/reg/sent")
;;               (mu4e-drafts-folder . "/reg/draft")
;;               (mu4e-refile-folder . "/reg/archive")
;;               (user-mail-address . "zhitaao.gong.reg@gmail.com")
;;               (message-signature-file . ,(expand-file-name "signature/personal"
;;                                                            me-emacs-data-private))))))

;;   (setq mu4e-compose-dont-reply-to-self t)
;;   (setq mu4e-user-mail-address-list
;;         '("zzg0009@auburn.edu" "zzg0009@tigermail.auburn.edu" "gong@auburn.edu"
;;           "zhitaao.gong@gmail.com" "zhitaao.gong.reg@gmail.com"
;;           "gongzhitaao@google.com" "gongzhitaao@fb.com"))

;;   (setq mu4e-attachment-dir (expand-file-name "Downloads" me-home)
;;         mu4e-change-filenames-when-moving nil
;;         mu4e-compose-complete-addresses t
;;         mu4e-compose-context-policy nil
;;         mu4e-context-policy 'pick-first
;;         mu4e-headers-include-related nil
;;         mu4e-headers-results-limit 50
;;         mu4e-index-cleanup t
;;         mu4e-index-lazy-check nil
;;         mu4e-sent-messages-behavior #'me//process-sent-messages
;;         mu4e-update-interval 300
;;         mu4e-use-fancy-chars t
;;         mu4e-view-mode-hook '(bbdb-mua-auto-update)
;;         mu4e-view-scroll-to-next nil
;;         mu4e-view-show-addresses t)

;;   (add-to-list 'mu4e-view-actions
;;                '("Brower" . mu4e-action-view-in-browser) t)
;;   (setq browse-url-generic-program "web-browswer")

;;   (add-to-list 'mu4e-bookmarks
;;                '("flag:flagged AND NOT flag:trashed" "Flagged messages" ?f))

;;   (setq mu4e-headers-attach-mark         '("a" . "◥")
;;         mu4e-headers-default-prefix      '("|"  . "● ")
;;         mu4e-headers-draft-mark          '("D" . "℮")
;;         mu4e-headers-duplicate-prefix    '("="  . "≡ ")
;;         mu4e-headers-empty-parent-prefix '("-"  . "● ")
;;         mu4e-headers-encrypted-mark      '("x" . "")
;;         mu4e-headers-first-child-prefix  '("\\" . "╰─")
;;         mu4e-headers-flagged-mark        '("F" . "⚐")
;;         mu4e-headers-has-child-prefix    '("+"  . "○╮")
;;         mu4e-headers-new-mark            '("N" . "⋆")
;;         mu4e-headers-passed-mark         '("P" . "⟫")
;;         mu4e-headers-replied-mark        '("R" . "")
;;         mu4e-headers-seen-mark           '("S" . "░")
;;         mu4e-headers-signed-mark         '("s" . "☡")
;;         mu4e-headers-trashed-mark        '("T" . "✖")
;;         mu4e-headers-unread-mark         '("u" . "█")
;;         mu4e-headers-from-or-to-prefix   '("" . "▶ "))

;;   (let ((marks '((refile  . ("r" . "▶"))
;;                  (delete  . ("D" . "✖"))
;;                  (flag    . ("+" . "⚑"))
;;                  (move    . ("m" . "▷"))
;;                  (read    . ("!" . "░"))
;;                  (trash   . ("d" . "♣"))
;;                  (unflag  . ("-" . "⚐"))
;;                  (untrash . ("=" . "♧"))
;;                  (unread  . ("?" . "█"))
;;                  (unmark  . (" " . " "))
;;                  (action  . ("a" . "◯"))
;;                  (something . ("*" . "♯")))))
;;     (dolist (elm marks)
;;       (plist-put (alist-get (car elm) mu4e-marks) :char (cdr elm))))

;;   (setq mu4e-headers-fields '((:human-date . 18)
;;                               (:flags . 10)
;;                               (:from-or-to . 30)
;;                               (:thread-subject)))

;;   (set-face-foreground 'mu4e-context-face "dark green")
;;   (set-face-foreground 'mu4e-modeline-face "DarkOrange4")
;;   (set-face-background 'mu4e-header-highlight-face "black")
;;   (set-face-foreground 'mu4e-cited-2-face "SteelBlue2"))

;; Message buffer
;; -----------------------------------------------------------------------------

(use-package orgalist)

(defun me//init-message ()
  "Init setup for message mode."
  (orgalist-mode)
  (turn-on-orgtbl)
  (turn-on-flyspell)
  (setq fill-column 80))

(use-package message
  :config
  (setq message-citation-line-format "On %a, %b %d %Y at %R, %N wrote:\n"
        message-citation-line-function #'message-insert-formatted-citation-line
        message-forward-as-mime nil
        message-forward-before-signature nil
        message-forward-ignored-headers ""
        message-make-forward-subject-function #'message-forward-subject-fwd)
  (add-hook 'message-mode-hook #'me//init-message)
  (add-hook 'message-send-hook 'ispell-message))

(use-package gnus-alias
  :config
  ;; Define two identities, "home" and "work"
  (setq gnus-alias-identity-alist
        `(("personal"
           nil ;; Does not refer to any other identity
           "Zhitao <zhitaao.gong@gmail.com>" ;; Sender address
           nil ;; No organization header
           nil ;; No extra headers
           nil ;; No extra body text
           ,(expand-file-name "signature/personal"
                              me-emacs-data-private))
          ("work"
           nil
           "Zhitao <gongzhitaao@deepmind.com>"
           "DeepMind."
           nil
           nil
           ,(expand-file-name "signature/personal"
                              me-emacs-data-private))))

  ;; Use "home" identity by default
  (setq gnus-alias-default-identity "personal")

  ;; Define rules to match work identity
  (setq gnus-alias-identity-rules
        '(("work" ("any" "gongzhitaao@\\(google\\.com\\|deepmind\\.com\\)" both)
           "work")))

  ;; Determine identity when message-mode loads
  (add-hook 'message-setup-hook 'gnus-alias-determine-identity))

;; sending mails
;; -----------------------------------------------------------------------------

(use-package sendmail
  :config
  (setq send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "msmtp")
  (add-hook 'mail-send-hook  'ispell-message))

;; message notification, only in the modeline
;; -----------------------------------------------------------------------------

;; (use-package mu4e-alert
;;   :config
;;   (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
;;   (mu4e-alert-set-default-style 'mode-line))

;; Automatically start.
;;
;; The mbsync runs on 5, 10, 15, ... every hour.  It does not make sense for
;; mu4e to update before mbsync.  Thus we start mu4e on x+0.5min, where x is the
;; next nearest 5-min.
;; -----------------------------------------------------------------------------

(defun me//seconds-from-now (interval &optional wait)
  "Calculate INTERVAL+WAIT seconds from now."
  (let* ((m (mod (string-to-number (format-time-string "%M")) interval))
         (s (string-to-number (format-time-string "%S")))
         (elapsed (+ (* m 60) s))
         (w (or wait 30)))
    (if (< elapsed (- w 15))
        w
      (- (+ (* interval 60) w) elapsed))))

(defun me//start-mu4e-bg ()
  "Start mu4e in background."
  (mu4e t))
;; (run-at-time (me//seconds-from-now 5) nil #'me//start-mu4e-bg)

(provide 'mail-conf)
;;; mail-conf.el ends here
