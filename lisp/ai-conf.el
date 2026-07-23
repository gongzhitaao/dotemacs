;;; ai-conf.el --- AI coding agents  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Zhitao Gong

;; Author: Zhitao Gong <zhitaao.gong@gmail.com>
;; Keywords: internal

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

;; Extracted from init.el to keep it manageable.  Everything here exists
;; to run coding agents inside Emacs, including `eat', which I use only
;; as the terminal backend for `claude-code.el' rather than as a general
;; purpose terminal.

;;; Code:

;;; * Terminal backend

(use-package inheritenv
  :straight (:type git :host github :repo "purcell/inheritenv"))

(defun me--set-eat-buffer-font ()
  "Apply my preferred font to all Eat terminal buffers."
  (buffer-face-set '(:family "JuliaMono" :height 120)))

(use-package eat
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el")))
  :delight (eat-eshell-mode nil)
  :hook ((eat-mode . me--set-eat-buffer-font)
         (eat-eshell-mode . me--set-eat-buffer-font))
  :bind (:map eat-semi-char-mode-map
              ("C-z" . nil)
              ("M-w" . kill-ring-save)))

;;; * Claude Code

(use-package claude-code
  :delight
  :straight (:type git
                   :host github
                   :repo "stevemolitor/claude-code.el"
                   :branch "main"
                   :depth 1
                   :files ("*.el" (:exclude "images/*")))
  :bind-keymap
  ( "C-c c" . claude-code-command-map) ;; or your preferred key
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  ( :repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
  :config
  (claude-code-mode)
  (setq claude-code-eat-read-only-mode-cursor-type '(hollow nil nil)))

;; `with-eval-after-load' rather than a bare `set-face-attribute': this
;; has to land after `load-theme', and claude-code is lazy enough that it
;; always does.
(with-eval-after-load 'claude-code
  (set-face-attribute 'claude-code-repl-face nil
                      :family "JuliaMono"
                      :height 120))

(defun me--claude-code-compact-modeline ()
  "Compact modeline label for Claude Code buffers: a robot glyph, a LAN
marker + `m²' for `makermaker-*' hosts, and just the final path component.
The real buffer name is preserved; the full name shows on hover."
  (let* ((dir  (directory-file-name
                (or (file-remote-p default-directory 'localname)
                    default-directory)))
         (host (file-remote-p default-directory 'host))
         (host (cond ((null host) nil)
                     ((string-match-p "\\`makermaker-" host) "m²")
                     (t host)))
         (label (concat "󰚩 "
                        (and host (concat (nerd-icons-mdicon "nf-md-lan_connect")
                                          " " host " "))
                        (file-name-nondirectory dir))))
    (setq-local mode-line-buffer-identification
                (list (propertize label
                                  'face 'mode-line-buffer-id
                                  'help-echo (buffer-name))))))

(add-hook 'claude-code-start-hook #'me--claude-code-compact-modeline)

(provide 'ai-conf)
;;; ai-conf.el ends here
