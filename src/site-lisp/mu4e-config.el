;;; mu4e-config.el --- Mu4e Config.                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Zhitao Gong

;; Author: Zhitao Gong <zhitaao.gong@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(use-package mu4e
  :config
  (setq mu4e-maildir (expand-file-name ".mail" me-home))

  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Personal"
            :match-func
            (lambda (msg)
              (when msg (string-prefix-p
                         "/personal" (mu4e-message-field msg :maildir))))
            :vars
            `((mu4e-trash-folder . "/personal/trash")
              (mu4e-sent-folder . "/personal/sent")
              (mu4e-drafts-folder . "/personal/drafts")
              (mu4e-refile-folder . "/personal/archive")
              (user-mail-address . "zhitaao.gong@gmail.com")
              (message-signature-file . ,(expand-file-name "signature/personal"
                                                           me-emacs-data-private))))
          ,(make-mu4e-context
            :name "Work"
            :match-func
            (lambda (msg)
              (when msg (string-prefix-p
                         "/work" (mu4e-message-field msg :maildir))))
            :vars
            `((mu4e-trash-folder . "/work/trash")
              (mu4e-sent-folder . "/work/sent")
              (mu4e-drafts-folder . "/work/drafts")
              (mu4e-refile-folder . "/work/archive")
              (user-mail-address . "gongzhitaao@google.com")
              (message-signature-file . ,(expand-file-name
                                          "signature/work.google"
                                          me-emacs-data-private))))
          ,(make-mu4e-context
            :name "Tiger"
            :match-func
            (lambda (msg)
              (when msg (string-prefix-p
                         "/tiger" (mu4e-message-field msg :maildir))))
            :vars
            `((mu4e-trash-folder . "/tiger/trash")
              (mu4e-sent-folder . "/tiger/sent")
              (mu4e-drafts-folder . "/tiger/draft")
              (mu4e-refile-folder . "/tiger/archive")
              (user-mail-address . "zzg0009@auburn.edu")
              (message-signature-file . ,(expand-file-name "signature/tiger"
                                                           me-emacs-data-private))))
          ,(make-mu4e-context
            :name "Reg"
            :match-func
            (lambda (msg)
              (when msg (string-prefix-p
                         "/reg" (mu4e-message-field msg :maildir))))
            :vars
            `((mu4e-trash-folder . "/reg/trash")
              (mu4e-sent-folder . "/reg/sent")
              (mu4e-drafts-folder . "/reg/draft")
              (mu4e-refile-folder . "/reg/archive")
              (user-mail-address . "zhitaao.gong.reg@gmail.com")
              (message-signature-file . ,(expand-file-name
                                          "signature/personal"
                                          me-emacs-data-private))))))

  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-user-mail-address-list
        '("zzg0009@auburn.edu" "zzg0009@tigermail.auburn.edu" "gong@auburn.edu"
          "zhitaao.gong@gmail.com" "zhitaao.gong.reg@gmail.com"
          "gongzhitaao@google.com" "gongzhitaao@fb.com"))

  (setq mu4e-attachment-dir (expand-file-name "Downloads" me-home)
        mu4e-change-filenames-when-moving nil
        mu4e-compose-complete-addresses t
        mu4e-compose-context-policy nil
        mu4e-context-policy 'pick-first
        mu4e-headers-include-related nil
        mu4e-headers-results-limit 50
        mu4e-index-cleanup t
        mu4e-index-lazy-check nil
        mu4e-sent-messages-behavior #'me//process-sent-messages
        mu4e-update-interval 300
        mu4e-use-fancy-chars t
        mu4e-view-mode-hook '(bbdb-mua-auto-update)
        mu4e-view-scroll-to-next nil
        mu4e-view-show-addresses t)

  (add-to-list 'mu4e-view-actions
               '("Brower" . mu4e-action-view-in-browser) t)
  (setq browse-url-generic-program "web-browswer")

  (add-to-list 'mu4e-bookmarks
               '("flag:flagged AND NOT flag:trashed" "Flagged messages" ?f))

  (setq mu4e-headers-attach-mark         '("a" . "◥")
        mu4e-headers-default-prefix      '("|"  . "● ")
        mu4e-headers-draft-mark          '("D" . "℮")
        mu4e-headers-duplicate-prefix    '("="  . "≡ ")
        mu4e-headers-empty-parent-prefix '("-"  . "● ")
        mu4e-headers-encrypted-mark      '("x" . "")
        mu4e-headers-first-child-prefix  '("\\" . "╰─")
        mu4e-headers-flagged-mark        '("F" . "⚐")
        mu4e-headers-has-child-prefix    '("+"  . "○╮")
        mu4e-headers-new-mark            '("N" . "⋆")
        mu4e-headers-passed-mark         '("P" . "⟫")
        mu4e-headers-replied-mark        '("R" . "")
        mu4e-headers-seen-mark           '("S" . "░")
        mu4e-headers-signed-mark         '("s" . "☡")
        mu4e-headers-trashed-mark        '("T" . "✖")
        mu4e-headers-unread-mark         '("u" . "█")
        mu4e-headers-from-or-to-prefix   '("" . "▶ "))

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

  (setq mu4e-headers-fields '((:human-date . 18)
                              (:flags . 10)
                              (:from-or-to . 30)
                              (:thread-subject)))

  (set-face-foreground 'mu4e-context-face "dark green")
  (set-face-foreground 'mu4e-modeline-face "DarkOrange4")
  (set-face-background 'mu4e-header-highlight-face "black")
  (set-face-foreground 'mu4e-cited-2-face "SteelBlue2"))

(provide 'mu4e-config)
;;; mu4e-config.el ends here
