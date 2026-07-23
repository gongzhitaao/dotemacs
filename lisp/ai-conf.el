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

;;; * Shared

(defconst me-agent-buffer-font '(:family "JuliaMono" :height 120)
  "Face spec shared by every agent buffer.
Smaller than the editing default, since these buffers hold streamed
prose and diffs rather than code I am editing.")

(defun me--set-agent-buffer-font ()
  "Apply `me-agent-buffer-font' to the current buffer.
For modes with no single body face to override, so the remap has to be
buffer-local."
  (buffer-face-set me-agent-buffer-font))

;;; * Terminal backend

(use-package inheritenv
  :straight (:type git :host github :repo "purcell/inheritenv"))

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
  :hook ((eat-mode . me--set-agent-buffer-font)
         (eat-eshell-mode . me--set-agent-buffer-font))
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
  (apply #'set-face-attribute 'claude-code-repl-face nil
         me-agent-buffer-font))

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

(defun me--agent-shell-resolve-path (path)
  "Translate PATH between TRAMP form and the remote host's own form.

`acp.el' passes :file-handler to `make-process' when `default-directory'
is remote, so on a TRAMP buffer the agent runs on the far host and speaks
that host's paths: /home/me/x, never /ssh:host:/home/me/x.

Both directions need mapping, and `agent-shell-path-resolver-function' is
the single hook for both.  Outbound, the cwd sent with session/new must
lose the TRAMP prefix or the agent is handed a directory that does not
exist -- which is a hang, not an error.  Inbound is the dangerous one:
fs/read_text_file and fs/write_text_file arrive carrying the remote
host's path, and without the prefix restored Emacs would happily read and
write the identically named file on this machine.

Inert locally, where `file-remote-p' returns nil throughout."
  (if-let* ((remote (file-remote-p default-directory)))
      (if (file-remote-p path)
          (file-remote-p path 'localname)
        (concat remote path))
    path))

(defun me--acp-pty-for-remote (orig &rest args)
  "Run ORIG with ARGS, forcing a pty for the ACP process when remote.

`acp.el' spawns the agent with :connection-type \\='pipe.  Over TRAMP that
channel is write-only: the remote process starts and stays alive, but
nothing sent to its stdin ever arrives, so the agent never sees
`initialize' and agent-shell sits on \"Starting agent\" with no error.
Reproducible without any of this code -- \"sh -c \\='read l; echo GOT:$l\\='\"
run through `make-process' on a TRAMP directory blocks forever on a pipe
and answers immediately on a pty.

A pty carries stdin fine and, checked against claude-agent-acp over ssh,
neither echoes input back nor translates newlines, so the JSON-RPC
stream stays intact.

Scoped to this one call rather than advising `make-process' globally,
and left alone entirely for local sessions, where pipes work."
  (if (not (file-remote-p default-directory))
      (apply orig args)
    (let ((real (symbol-function 'make-process)))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest a)
                   (apply real (plist-put a :connection-type 'pty)))))
        (apply orig args)))))

(with-eval-after-load 'acp
  (advice-add 'acp--start-client :around #'me--acp-pty-for-remote))

(use-package agent-shell
  :bind (("C-c a" . agent-shell)
         :map agent-shell-mode-map
         ("C-c C-q" . me-agent-shell-compose)
         ;; Alias for the C-c C-c that is already there, to match the
         ;; C-c C-k that aborts the compose buffer, org-src, magit and
         ;; every other "this was a mistake" buffer.
         ("C-c C-k" . agent-shell-interrupt))
  :custom
  (agent-shell-path-resolver-function #'me--agent-shell-resolve-path)

  ;; Put the agent / model / mode / context-usage readout in the mode line
  ;; rather than a header line.  The `graphical' default draws an SVG badge
  ;; sized at (* 3 char-height), so the header is three lines tall no matter
  ;; what font it is given -- shrinking the text cannot help.  `text' would
  ;; give a one-line header instead; nil drops the header entirely and
  ;; `agent-shell--mode-line-format' takes over, which is set up from
  ;; `agent-shell--start' and so survives the dead mode hook noted below.
  (agent-shell-header-style nil)

  ;; Show what the agent actually ran.  Both are needed: the group flag
  ;; reveals the members of a run of consecutive actions, the tool-use
  ;; flag expands each member's command and diff.  Thoughts stay folded.
  (agent-shell-activity-group-expand-by-default t)
  (agent-shell-tool-use-expand-by-default t)
  ;; Permission mode, not the model: "use a model classifier to approve or
  ;; deny permission prompts" rather than stopping on each one.  The other
  ;; mode IDs claude-agent-acp reports are default, acceptEdits, plan,
  ;; dontAsk and bypassPermissions.  Change per session with C-c C-m, or
  ;; cycle with C-<tab>.
  ;;
  ;; The model is deliberately left alone: its IDs are default, opus[1m],
  ;; claude-fable-5[1m], sonnet and haiku, and "default" -- already the
  ;; value when unset -- is the one that picks for itself.  C-c C-v to
  ;; override for a session.
  (agent-shell-anthropic-default-session-mode-id "auto"))

;; agent-shell's faces are all semantic (prompt, model, error...) with no
;; body face to hang a family on, so the buffer's default gets remapped,
;; the same way the eat buffers do.
;;
;; This cannot be an `agent-shell-mode' hook, because that hook never
;; runs.  `shell-maker-define-major-mode' builds the mode with
;;
;;     (eval `(define-derived-mode ... (use-local-map ,mode-map)))
;;
;; which splices the keymap *object* into the body, so the mode ends up
;; evaluating `(keymap ...)' as a function call and signals
;; `void-function keymap'.  It fails after the keymap and syntax table
;; are installed but before `run-mode-hooks', which is why the shell is
;; usable while every mode hook is silently skipped.  `unwind-protect'
;; gets the font on either way without swallowing the error.

(defun me--agent-shell-mode-font (orig &rest args)
  "Apply `me-agent-buffer-font' around ORIG, called with ARGS."
  (unwind-protect (apply orig args)
    (me--set-agent-buffer-font)))

(with-eval-after-load 'agent-shell
  (advice-add 'agent-shell-mode :around #'me--agent-shell-mode-font))

;;; ** Composing requests in Org

;; Typing at the shell prompt while a request is in flight does nothing:
;; `shell-maker--clear-input-for-execution' wraps its body in an `unless
;; shell-maker--busy', so RET is silently dropped.  agent-shell's answer
;; is `agent-shell-queue-request', which reads from the minibuffer, and
;; its viewport compose buffer, which is derived from `text-mode'.
;;
;; Neither suits a long request.  This opens a scratch Org buffer instead,
;; so a prompt can be written with lists, source blocks and the usual
;; editing bindings.  C-c C-c sends, C-c C-k discards.  Mid-request the
;; text is queued rather than dropped, which is what agent-shell's own
;; send commands do.

(defvar me-agent-shell-compose-buffer-name "*agent-shell compose*"
  "Name of the buffer used to compose agent-shell requests.")

(defvar-local me--agent-shell-compose-target nil
  "Shell buffer that this compose buffer submits to.")

(defvar-keymap me-agent-shell-compose-mode-map
  :doc "Keymap for `me-agent-shell-compose-mode'."
  "C-c C-c" #'me-agent-shell-compose-send
  "C-c C-k" #'me-agent-shell-compose-cancel)

(define-minor-mode me-agent-shell-compose-mode
  "Minor mode for composing an agent-shell request in an Org buffer."
  :lighter " Compose")

(defun me-agent-shell-compose ()
  "Compose a request for this project's agent shell in an Org buffer.
Reuses the compose buffer if it already holds an unsent draft."
  (interactive)
  (require 'agent-shell)
  ;; `agent-shell--shell-buffer' is private, but it is the only thing that
  ;; does the full resolution -- viewport, current shell, project shell,
  ;; then prompt -- and reimplementing that would drift.
  (let ((shell (agent-shell--shell-buffer)))
    (pop-to-buffer (get-buffer-create me-agent-shell-compose-buffer-name))
    ;; `org-mode' calls `kill-all-local-variables', so the target has to be
    ;; recorded after it, not before.
    (unless (derived-mode-p 'org-mode)
      (org-mode))
    (me-agent-shell-compose-mode 1)
    (setq me--agent-shell-compose-target shell)
    (setq header-line-format
          (substitute-command-keys
           (format "%s  \\<me-agent-shell-compose-mode-map>\\[me-agent-shell-compose-send] send, \\[me-agent-shell-compose-cancel] discard"
                   (buffer-name shell))))))

(defun me-agent-shell-compose-send ()
  "Send the composed request, queueing it if the agent is busy."
  (interactive)
  (let ((text (string-trim (buffer-substring-no-properties
                            (point-min) (point-max))))
        (shell me--agent-shell-compose-target))
    (when (string-empty-p text)
      (user-error "Nothing to send"))
    (unless (buffer-live-p shell)
      (user-error "Shell buffer is gone; C-c C-k to discard"))
    (if (with-current-buffer shell (shell-maker-busy))
        (with-current-buffer shell (agent-shell-queue-request text))
      (agent-shell-insert :text text :submit t :shell-buffer shell))
    (kill-buffer)
    (pop-to-buffer shell)))

(defun me-agent-shell-compose-cancel ()
  "Discard the composed request."
  (interactive)
  (kill-buffer))

(provide 'ai-conf)
;;; ai-conf.el ends here
