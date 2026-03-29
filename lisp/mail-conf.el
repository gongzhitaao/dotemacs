;;; mail-conf.el --- Reading mails                   -*- lexical-binding: t; -*-
;;; Commentary:
;; Public mail configuration.  Personal details (addresses, identities,
;; account mappings) live in mail-private.el which is not tracked in git.

;;; Code:

(require 'mail-private)

;; Core settings
;; -----------------------------------------------------------------------------

(defvar display-time-mail-string)
(defvar display-time-use-mail-icon)
(setq display-time-mail-string " "
      display-time-use-mail-icon nil)

;; Message buffer
;; -----------------------------------------------------------------------------

(use-package orgalist)

(defun me--init-message ()
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
        message-auto-save-directory (file-name-concat me-emacs-cache-dir "message")
        message-make-forward-subject-function #'message-forward-subject-fwd)
  (add-hook 'message-mode-hook #'me--init-message)
  (add-hook 'message-send-hook 'ispell-message))

(use-package gnus-alias
  :config
  (setq gnus-alias-identity-alist me-mail-identities)
  (setq gnus-alias-default-identity me-mail-default-identity)
  (setq gnus-alias-identity-rules me-mail-identity-rules)
  (add-hook 'message-setup-hook 'gnus-alias-determine-identity))

;; sending mails
;; -----------------------------------------------------------------------------

(defun me--get-email-address (from)
  "Get email address from FROM field."
  (if (null (string-match "<\\([^@<]+@[^>]+\\)>" from))
      nil
    (substring (match-string 0 from) 1 -1)))

(defun me--auto-choose-email-account ()
  "Choose email account automatically."
  (let* ((from (me--get-email-address (message-fetch-field "from")))
         (mail-dir (alist-get from me-mail-accounts nil nil #'string=)))
    (if mail-dir
        (setq-local message-sendmail-extra-arguments
                    `("send" "--quiet" "-t" "-C" ,mail-dir))
      (warn "Account: %s not found" from))))

(use-package sendmail
  :ensure nil
  :config
  (setq send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "gmi")
  (add-hook 'message-send-hook #'me--auto-choose-email-account))

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
  (setq notmuch-saved-searches me-notmuch-saved-searches)
  (setq notmuch-fcc-dirs nil))

(provide 'mail-conf)
;;; mail-conf.el ends here
