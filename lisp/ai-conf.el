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
;;
;; Two ways in, deliberately kept side by side for now:
;;
;;   C-c c  `claude-code.el' -- drives the CLI inside an eat terminal.
;;          Everything the CLI can do, but it is a terminal.
;;   C-c a  `agent-shell'    -- speaks ACP to the agent, so the session
;;          is a plain comint buffer.  Fewer features, real Emacs keys.

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

;;; ** Persistent remote Claude sessions

;; On a TRAMP directory `claude-code.el' starts claude through an
;; Emacs-owned ssh channel, so quitting Emacs takes the remote session
;; down with it.  Wrapping the remote invocation in tmux moves the
;; session's lifetime to the remote host: killing the eat buffer only
;; detaches the client, and starting Claude again in the same directory
;; re-attaches to the still-running conversation.
;;
;; `env' fixes up the terminal for tmux: eat exports TERM=eat-truecolor
;; plus a TERMINFO pointing at a *local* directory, neither of which the
;; remote ncurses can resolve, and tmux refuses to start without a
;; usable terminfo entry.  (To keep truecolor instead, rsync
;; `eat-term-terminfo-directory' to ~/.terminfo on the remote and drop
;; the TERM= override below.)

(defcustom me-claude-code-remote-tmux t
  "Whether to run remote Claude Code sessions inside tmux."
  :type 'boolean
  :group 'claude-code)

(defvar-local me-claude-code-tmux-session nil
  "Name of the remote tmux session backing this Claude buffer.")

(defun me--claude-code-tmux-session-name (buffer-name)
  "Stable tmux session name for the Claude buffer BUFFER-NAME.
The hash keeps directory and instance distinct; the readable prefix
keeps `tmux ls' output useful."
  (let ((base (file-name-nondirectory
               (directory-file-name
                (or (file-remote-p default-directory 'localname)
                    default-directory)))))
    (format "claude-%s-%s"
            (replace-regexp-in-string "[^A-Za-z0-9_-]" "-" base)
            (substring (md5 buffer-name) 0 6))))

(defun me--claude-code-remote-tmux (orig backend buffer-name program
                                         &optional switches)
  "Around advice for `claude-code--term-make' running PROGRAM under tmux.
Only applies when `default-directory' is remote and the host has tmux;
otherwise ORIG runs with BACKEND, BUFFER-NAME, PROGRAM and SWITCHES
unchanged."
  (if (and me-claude-code-remote-tmux
           (file-remote-p default-directory)
           (executable-find "tmux" 'remote))
      (let* ((session (me--claude-code-tmux-session-name buffer-name))
             ;; Resolve claude here rather than letting tmux look it up:
             ;; tmux runs the pane command with the *server's* environment,
             ;; which was fixed whenever that server first started and need
             ;; not have ~/.local/bin on PATH.  Tramp's own-remote-path does
             ;; know where claude lives.
             (program (or (executable-find program 'remote) program))
             (buffer (funcall orig backend buffer-name "env"
                              (append (list "-u" "TERMINFO"
                                            "TERM=xterm-256color"
                                            "tmux" "new-session"
                                            "-A"   ; attach if it exists
                                            "-D"   ; ...evicting stale clients
                                            "-s" session
                                            "-c" (file-remote-p
                                                  default-directory 'localname)
                                            program)
                                      switches))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq me-claude-code-tmux-session session)))
        buffer)
    (funcall orig backend buffer-name program switches)))

(defun me-claude-code-kill-remote-session ()
  "Really end the remote Claude session behind the current buffer.
`claude-code-kill' only kills the tmux client, leaving claude running
on the far side; this kills the tmux session too."
  (interactive)
  (let ((session (buffer-local-value 'me-claude-code-tmux-session
                                     (current-buffer))))
    (cond
     ((not session) (user-error "Not a tmux-backed Claude buffer"))
     ((not (yes-or-no-p (format "Kill remote session %s? " session))) nil)
     (t (process-file "tmux" nil nil nil "kill-session" "-t" session)
        (claude-code-kill)
        (message "Killed remote session %s" session)))))

;; Advise after load: `claude-code--term-make' is a `cl-defgeneric', and
;; advising the symbol before it exists confuses its method dispatch.
(with-eval-after-load 'claude-code
  (advice-add 'claude-code--term-make :around #'me--claude-code-remote-tmux)
  (define-key claude-code-command-map (kbd "q")
              #'me-claude-code-kill-remote-session))

;;; * Agent shell

;; ACP (Agent Client Protocol) is the JSON-RPC protocol Zed introduced
;; for talking to coding agents.  `agent-shell' speaks it from a
;; `shell-maker' comint buffer, so unlike `claude-code.el' there is no
;; terminal emulator in the loop: normal keybindings, isearch, yanking
;; and fonts all behave.  Diffs and permission requests render as
;; in-buffer buttons.
;;
;; Needs the adapter on PATH (straight pulls agent-shell, acp and
;; shell-maker from MELPA on its own):
;;
;;     npm install -g @agentclientprotocol/claude-agent-acp
;;
;; The tradeoff is that ACP is a lowest-common-denominator protocol, so
;; some CLI-specific niceties are missing.  Hence keeping both for now.
;;
;; Authentication needs no setting: `agent-shell-anthropic-authentication'
;; defaults to (:login . t), which reuses the `claude' CLI's own
;; credentials.  Set it with `agent-shell-anthropic-make-authentication'
;; if an :api-key or :oauth token is ever wanted instead -- from
;; `:config' rather than `:custom', since that constructor does not exist
;; until the package has loaded.

(use-package agent-shell
  :bind ("C-c a" . agent-shell)
  :custom
  ;; One of the IDs listed under "Available models" when a shell starts.
  ;; claude-agent-acp offers auto, default, opus, opusplan, sonnet and
  ;; sonnet[1m].  Switch per session with C-c C-v.
  (agent-shell-anthropic-default-model-id "auto"))

(provide 'ai-conf)
;;; ai-conf.el ends here
