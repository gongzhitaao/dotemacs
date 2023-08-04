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

;; (defun me//process-sent-messages ()
;;   "Post-process sent messages based on email address.

;; Gmail/IMAP takes care of copying sent messages to sent folder.
;; So we just delete it locally."
;;   (if (string-match-p
;;        (regexp-opt '("@gmail.com" "@tigermail.auburn.edu" "@auburn.edu"))
;;        (message-sendmail-envelope-from))
;;       'delete 'sent))

(defvar display-time-mail-string)
(defvar display-time-use-mail-icon)
(setq display-time-mail-string " "
      display-time-use-mail-icon nil)

(eval-when-compile
  (require 'use-package))

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
        message-forward-before-signature t
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
           "Zhitao <gongzhitaao@google.com>"
           "Google DeepMind"
           nil
           nil
           ,(expand-file-name "signature/work"
                              me-emacs-data-private))))

  ;; Use "home" identity by default
  (setq gnus-alias-default-identity "personal")

  ;; Define rules to match work identity
  (setq gnus-alias-identity-rules
        '(("work" ("any" "gongzhitaao@\\(google\\|deepmind\\)\\.com" both)
           "work")))

  ;; Determine identity when message-mode loads
  (add-hook 'message-setup-hook 'gnus-alias-determine-identity))

;; sending mails
;; -----------------------------------------------------------------------------

(defun me//get-email-address (from)
  "Get email address from FROM field."
  (if (null (string-match "<\\([^@<]+@[^>]+\\)>" from))
      nil
    (substring (match-string 0 from) 1 -1)))

(defun me//auto-choose-email-account ()
  "Choose email account automatically."
    (let* ((accounts '(("gongzhitaao@google.com" . "~/.mail/corp")
                       ("gongzhitaao@deepmind.com" . "~/.mail/corp")
                       ("zhitaao.gong@gmail.com" . "~/.mail/personal")))
           (from (me//get-email-address (message-fetch-field "from")))
           (mail-dir (alist-get from accounts nil nil #'string=)))
      (if mail-dir
          (set (make-local-variable 'message-sendmail-extra-arguments)
                `("send" "--quiet" "-t" "-C" ,mail-dir))
        (warn "Account: %s not found" mail-dir))))

(use-package sendmail
  :config
  (setq send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "gmi")

  (add-hook 'message-send-hook #'me//auto-choose-email-account))

(use-package notmuch
  :config
  (defvar me-notmuch-tag-map-prefix "f"
    "Prefix to active the tags keymap.")

  (dolist (key-fname-tags
           '(("d" "trash" '("-inbox" "-unread" "+trash"))
             ("D" "untrash" '("+inbox" "-trash"))
             ("f" "unstar" '("-flagged"))
             ("F" "star" '("+flagged"))
             ("i" "mark-unimportant" '("-important"))
             ("I" "mark-important" '("+important"))
             ("r" "mark-read" '("-unread"))
             ("R" "mark-unread" '("+unread"))))
    (let* ((key
            (string-join `(,me-notmuch-tag-map-prefix ,(car key-fname-tags))))
           (fname (cadr key-fname-tags))
           (tags (caddr key-fname-tags)))
      (defalias (intern fname)
        `(lambda ()
           ,(format "Apply %s to highlighted mails." tags)
           (interactive)
           (notmuch-search-tag ,tags)))
      (define-key notmuch-search-mode-map key (intern fname))))

  (setq-default notmuch-search-oldest-first nil)

  (setq notmuch-saved-searches
        '(
          ;; No access to corp emails because Google corp policy tightens.
          ;; (:name "corp/inbox:7d" :query "tag:corp and tag:inbox date:<7d>.."
          ;;  :key "ci")
          ;; (:name "corp/unread" :query "tag:corp and tag:unread" :key "cu")
          ;; (:name "corp/starred" :query "tag:corp and tag:flagged" :key "cs")

          (:name "haha/inbox:7d" :query "tag:haha and tag:inbox date:<7d>.."
           :key "hi")
          (:name "haha/unread" :query "tag:haha and tag:unread" :key "hu")
          (:name "haha/starred" :query "tag:haha and tag:flagged" :key "hs")))

  (setq notmuch-fcc-dirs nil))

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

;; (defun me//seconds-from-now (interval &optional wait)
;;   "Calculate INTERVAL+WAIT seconds from now."
;;   (let* ((m (mod (string-to-number (format-time-string "%M")) interval))
;;          (s (string-to-number (format-time-string "%S")))
;;          (elapsed (+ (* m 60) s))
;;          (w (or wait 30)))
;;     (if (< elapsed (- w 15))
;;         w
;;       (- (+ (* interval 60) w) elapsed))))

;; (defun me//start-mu4e-bg ()
;;   "Start mu4e in background."
;;   (mu4e t))
;; ;; (run-at-time (me//seconds-from-now 5) nil #'me//start-mu4e-bg)

(provide 'mail-conf)
;;; mail-conf.el ends here
