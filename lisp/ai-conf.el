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

(require 'cl-lib)                        ; `cl-letf' used below

;;; * Shared

(defconst me-agent-buffer-font (list :family "JuliaMono"
                                     :height me-default-font-height)
  "Face spec shared by every agent buffer.
A distinct family from the editing default, but the same height, since
120 reads too small for streamed prose and diffs.")

(defun me--set-agent-buffer-font ()
  "Apply `me-agent-buffer-font' to the current buffer.
For modes with no single body face to override, so the remap has to be
buffer-local."
  (buffer-face-set me-agent-buffer-font))

;;; * Terminal backend

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
;; `env -u TERMINFO' is the terminal fix-up tmux needs.  eat points
;; TERMINFO at a local build directory that does not exist on the remote,
;; and while it stays set ncurses searches only there and cannot find any
;; terminal type, so tmux refuses to start.  Unsetting it lets ncurses
;; fall back to ~/.terminfo.
;;
;; When eat's own terminfo lives there, TERM=eat-truecolor -- what eat
;; already exports -- resolves remotely and tmux draws with eat's real
;; capabilities.  `me--claude-code-ensure-remote-terminfo' copies it over
;; on first use of a host, so this needs no manual per-host setup; when
;; the copy has not (yet) happened, TERM falls back to xterm-256color so
;; tmux still starts, just with the imperfect partial redraws that leave
;; the buffer stale until a window resize forces a full repaint.

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

(defun me--claude-code-ensure-remote-terminfo ()
  "Install eat's terminfo under the remote ~/.terminfo when it is absent.
Return non-nil when eat-truecolor should resolve on the remote afterward,
so the caller can choose TERM=eat-truecolor over the xterm-256color
fallback.  eat ships its terminfo entries as symlinks into a local build
tree, so the bytes are read through the link and written as plain files;
`default-directory' is remote throughout, so all the target paths are."
  ;; `claude-code.el' requires eat lazily (inside `claude-code--term-make'),
  ;; and the advice below runs this guard *before* that require fires.  On
  ;; the first remote session after an Emacs restart eat is therefore not
  ;; loaded yet, `eat-term-terminfo-directory' is unbound, and the guard
  ;; wrongly reports failure -- poisoning that one session with the
  ;; xterm-256color fallback.  Load eat here so the variable is bound; a
  ;; failed load leaves `boundp' nil and the graceful fallback intact.
  (require 'eat nil t)
  (let* ((dir (and (boundp 'eat-term-terminfo-directory)
                   eat-term-terminfo-directory
                   (file-directory-p eat-term-terminfo-directory)
                   eat-term-terminfo-directory))
         ;; Build the remote path explicitly: a bare "~/.terminfo" would
         ;; expand against the *local* home even under a remote
         ;; `default-directory'.  The TRAMP prefix forces the remote side,
         ;; and ~ is expanded there by the file ops below.
         (base (and dir (concat (file-remote-p default-directory) "~/.terminfo")))
         (probe (and base (expand-file-name "e/eat-truecolor" base))))
    (cond
     ((null dir) nil)
     ((file-exists-p probe) t)
     (t
      (condition-case err
          (progn
            (dolist (sub (directory-files dir nil "\\`[^.]"))
              (let ((local-sub (expand-file-name sub dir)))
                (when (file-directory-p local-sub)
                  (dolist (entry (directory-files local-sub nil "\\`[^.]"))
                    (let ((local-file (expand-file-name entry local-sub))
                          (remote-file (expand-file-name
                                        (format "%s/%s" sub entry) base)))
                      (make-directory (file-name-directory remote-file) t)
                      (when (file-symlink-p remote-file)
                        (delete-file remote-file))
                      (let ((coding-system-for-read 'binary)
                            (coding-system-for-write 'binary))
                        (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally local-file)
                          (write-region (point-min) (point-max)
                                        remote-file nil 'silent))))))))
            (message "Installed eat terminfo on %s"
                     (file-remote-p default-directory 'host))
            t)
        (error
         (message "eat terminfo install on %s failed (%s); using xterm-256color"
                  (file-remote-p default-directory 'host)
                  (error-message-string err))
         nil))))))

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
             ;; eat-truecolor when its terminfo is (now) on the remote,
             ;; else xterm-256color so tmux still starts.
             (term (if (me--claude-code-ensure-remote-terminfo)
                       "eat-truecolor" "xterm-256color"))
             ;; Resolve claude here rather than letting tmux look it up:
             ;; tmux runs the pane command with the *server's* environment,
             ;; which was fixed whenever that server first started and need
             ;; not have ~/.local/bin on PATH.  Tramp's own-remote-path does
             ;; know where claude lives.
             (program (or (executable-find program 'remote) program))
             (buffer (funcall orig backend buffer-name "env"
                              (append (list "-u" "TERMINFO"
                                            (concat "TERM=" term)
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

(defun me--agent-shell-dot-subdir (subdir)
  "Return a local path for SUBDIR of agent-shell's per-project data.

The default, `agent-shell--dot-subdir-in-repo', puts this under
.agent-shell/ in the project root.  On a TRAMP project that root is
remote, so the transcript -- which is appended to after every message --
is rewritten over ssh each time, and the session spends its life
shuttling the file back and forth.

Keeping the data under the local cache instead makes those writes local.
Paths are keyed by host and project so a project checked out on several
machines does not collide with itself.

This covers screenshots as well, since both go through
`agent-shell--dot-subdir', and it also stops agent-shell adding
.agent-shell/ to the repo's exclude file: that only happens when the
resolved directory is inside the project, which this never is."
  (let* ((cwd (agent-shell-cwd))
         (host (or (file-remote-p cwd 'host) "localhost"))
         (local (or (file-remote-p cwd 'localname) cwd))
         (project (file-name-nondirectory (directory-file-name local)))
         (project (if (string-empty-p project) "default" project)))
    (agent-shell-cache-dir host project subdir)))

(defun me--agent-shell-lean-remote-fs (orig &rest args)
  "Run ORIG with ARGS without the incidental work of visiting a file.

agent-shell answers fs/read_text_file and fs/write_text_file by going
through `find-file-noselect' and `basic-save-buffer', so each file the
agent touches also drags in a VC backend probe -- which runs git on the
remote host -- plus a lock file, a backup copy and auto-save setup.  Every
one of those is a separate synchronous round trip, and TRAMP has no async
file primitives to fall back on, so Emacs is wedged for the duration.

None of that work is wanted here.  The agent already has the content, the
file is not being visited for editing, and the transcript records what
changed.  Binding it all away leaves one round trip per file instead of
five or six.

Deliberately scoped to these two handlers: files opened by hand keep
their locks, backups and VC state."
  (let ((vc-handled-backends nil)
        (make-backup-files nil)
        (create-lockfiles nil)
        ;; Not `auto-save-default': that is read once when the buffer is
        ;; created, so a buffer the agent opens here would stay without
        ;; auto-save for the rest of its life, including after I start
        ;; editing it by hand.  These two are consulted per operation
        ;; instead, and only for remote files.
        (remote-file-name-inhibit-locks t)
        (remote-file-name-inhibit-auto-save t)
        (remote-file-name-inhibit-auto-save-visited t))
    (apply orig args)))

(with-eval-after-load 'agent-shell
  (dolist (fn '(agent-shell--on-fs-read-text-file-request
                agent-shell--on-fs-write-text-file-request))
    (advice-add fn :around #'me--agent-shell-lean-remote-fs)))

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

(defun me--agent-shell-viewport-soft-wrap ()
  "Soft-wrap the agent-shell compose buffer at `fill-column'.
`visual-fill-column-mode' only reshapes the margins; the actual
line wrapping is `visual-line-mode', so both are needed."
  (visual-line-mode +1)
  (visual-fill-column-mode +1))

(use-package agent-shell
  :bind (("C-c a" . agent-shell)
         :map agent-shell-mode-map
         ("C-c C-q" . agent-shell-prompt-compose)
         ;; Alias for the C-c C-c that is already there, to match the
         ;; C-c C-k that aborts the compose buffer, org-src, magit and
         ;; every other "this was a mistake" buffer.
         ("C-c C-k" . agent-shell-interrupt)
         ;; Swap the submit key.  Out of the box RET submits: comint's RET
         ;; runs `comint-send-input', which `agent-shell-mode-map' remaps to
         ;; `agent-shell-submit'.  A newline needs S-RET.  Reverse that so a
         ;; multi-line prompt is the easy path: bind RET directly to
         ;; `newline' (a direct key binding shadows the inherited remap) and
         ;; put submit on C-RET.  C-RET is only a distinct event under a GUI
         ;; Emacs; in a terminal it collapses to RET, so there S-RET (still
         ;; live) remains the newline and RET would submit again.
         ("RET" . newline)
         ("C-<return>" . agent-shell-submit))
  ;; Soft-wrap composed prompts at `fill-column'.  The viewport edit buffer
  ;; is where the prompt is typed; wrapping it looks like `auto-fill' but
  ;; inserts no hard newlines, so the submitted text stays one logical line
  ;; per paragraph.  `visual-fill-column-mode' needs `visual-line-mode' under
  ;; it (it just moves the window margins in so the visual-line wrap lands at
  ;; `fill-column' instead of the window edge; width is `fill-column' while
  ;; `visual-fill-column-width' is nil).  View mode is read-only, left alone.
  :hook (agent-shell-viewport-edit-mode . me--agent-shell-viewport-soft-wrap)
  :custom
  (agent-shell-path-resolver-function #'me--agent-shell-resolve-path)
  (agent-shell-dot-subdir-function #'me--agent-shell-dot-subdir)

  ;; Show the agent / model / mode / context-usage readout in a header
  ;; line.  The `graphical' default draws an SVG badge sized at (* 3
  ;; char-height), so the header is three lines tall no matter what font it
  ;; is given -- shrinking the text cannot help.  `text' gives a one-line,
  ;; default-font header instead, which is what we want here.  (nil drops
  ;; the header entirely and pushes the readout into the mode line via
  ;; `agent-shell--mode-line-format' -- too cramped alongside everything
  ;; else there.)
  (agent-shell-header-style 'text)

  ;; The running indicator in that header is a nyan cat flying a rainbow
  ;; rather than the stock `wide' block bar.  A function, not the frame
  ;; list itself, because the frames are built further down the file (see
  ;; "Nyan cat busy indicator") and `:custom' is expanded up here; the
  ;; function is only called at heartbeat time, by when the file is loaded.
  (agent-shell-busy-indicator-frames #'me-agent-shell-nyan-frames)

  ;; Keep tool output folded by default -- the group flag would reveal the
  ;; members of a run of consecutive actions, the tool-use flag would expand
  ;; each member's command and diff.  Both off means a compact transcript you
  ;; expand on demand; thoughts stay folded regardless.
  (agent-shell-activity-group-expand-by-default nil)
  (agent-shell-tool-use-expand-by-default nil)
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
  (agent-shell-anthropic-default-session-mode-id "auto")

  :config
  ;; Fenced code blocks are already run through the language's major-mode
  ;; font-lock by default, so ```python and ```elisp highlight out of the
  ;; box.  The catch is `agent-shell-markdown--resolve-lang-mode' just
  ;; appends "-mode" to the tag and keeps it only if that is `fboundp', so
  ;; a tag whose mode has a different name silently renders plain.  The
  ;; built-in alias table covers elisp/cpp/objc; add the shell family and a
  ;; couple of config formats, all mapping to modes that ship with Emacs
  ;; and need no tree-sitter grammar.  console is left unmapped on purpose
  ;; -- it is command output, not shell source, and plain is right for it.
  ;; yaml/rust/... would need their grammars or -mode packages installed
  ;; first, then an entry here.
  (setq agent-shell-markdown-language-mapping
        (append '(("bash" . "sh")
                  ("shell" . "sh")
                  ("sh" . "sh")
                  ("zsh" . "sh")
                  ("shellscript" . "sh")
                  ("json" . "js-json")
                  ("toml" . "conf-toml"))
                agent-shell-markdown-language-mapping)))

;; Both ride along in every shell (SUI is agent-shell-ui-mode, @/Compl is
;; agent-shell-completion-mode), so their lighters are noise, not status.
;; The third element names the defining feature so delight can defer until
;; each is loaded rather than forcing it now.
(delight '((agent-shell-ui-mode nil agent-shell-ui)
           (agent-shell-completion-mode nil agent-shell-completion)))

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

(defun me--agent-shell-pad-header ()
  "Give the text header line some breathing room on either side.
agent-shell's `text' header opens with a single leading space and no
trailing one, so it sits flush against the edges.  agent-shell has no
padding option, but an invisible `:box' -- drawn in the header-line's
own background so it reads as padding rather than a border -- insets the
text horizontally (and a hair vertically)."
  (face-remap-add-relative
   'header-line
   `(:box (:line-width (5 . 5)
           :color ,(face-attribute 'header-line :background nil 'default)))))

(defun me--agent-shell-mode-font (orig &rest args)
  "Set up ORIG's buffer font and header padding, ORIG called with ARGS."
  (unwind-protect (apply orig args)
    (me--set-agent-buffer-font)
    (me--agent-shell-pad-header)))

(with-eval-after-load 'agent-shell
  (advice-add 'agent-shell-mode :around #'me--agent-shell-mode-font))

;; C-c a's agent picker prefixes every candidate with an icon, which is
;; fun -- but `agent-shell--config-icon' hardcodes its height to
;; `frame-char-height' (50px on this HiDPI display), so it fills a whole
;; text line and reads as oversized.  There is no size knob, so shrink the
;; one `frame-char-height' call inside that function: bind it to a
;; fraction only for the duration of the icon render, leaving every other
;; caller untouched.

(defconst me-agent-shell-config-icon-scale 0.6
  "Fraction of `frame-char-height' to size the agent picker icon at.")

(defun me--agent-shell-small-config-icon (orig &rest args)
  "Render ORIG's agent picker icon smaller, ORIG called with ARGS."
  (cl-letf* ((real (symbol-function 'frame-char-height))
             ((symbol-function 'frame-char-height)
              (lambda (&rest a)
                (round (* me-agent-shell-config-icon-scale (apply real a))))))
    (apply orig args)))

(with-eval-after-load 'agent-shell
  (advice-add 'agent-shell--config-icon
              :around #'me--agent-shell-small-config-icon))

;; Droid's icon is a GitHub avatar URL with no file extension, so
;; `agent-shell--fetch-agent-icon' caches a real PNG under an
;; extensionless name -- which `agent-shell--config-icon' then discards,
;; because its `image-supported-file-p' guard judges by filename, not
;; content.  Give the cached file the extension its bytes call for so the
;; guard accepts it; agents whose icon is already well-named pass through.

(defun me--agent-shell-icon-add-extension (path)
  "Return PATH with an image extension inferred from its own bytes.
No-op when PATH is nil, missing, or already an accepted image name."
  (if (and path
           (file-exists-p path)
           (not (image-supported-file-p path)))
      (if-let* ((type (ignore-errors (image-type path nil nil)))
                (typed (concat path "." (symbol-name type))))
          (progn
            (unless (file-exists-p typed)
              (copy-file path typed))
            typed)
        path)
    path))

(with-eval-after-load 'agent-shell
  (advice-add 'agent-shell--fetch-agent-icon
              :filter-return #'me--agent-shell-icon-add-extension))

;;; ** Timestamps on chat labels

;; `agent-shell-chat-label-function' is the documented way to draw each
;; turn's badge ("Me", or the agent's name such as "Claude"), so no
;; reaching into overlays: wrap the default boxed badge and append the
;; turn's time.
;;
;; The function is called on every relabel, so stamping `current-time'
;; into it directly would paint every past turn with the time of the
;; latest relabel.  Instead freeze each turn's time the first time its
;; badge is drawn, as a text property at the turn's `:prompt-start' -- a
;; buffer position that, as the label docstring notes, travels with the
;; turn across relabels.  `:prompt-start' is nil for the live, not-yet-
;; submitted prompt, which is exactly when a badge should stay unstamped
;; (a time is "when I sent this", not "when the empty prompt was drawn").
;; User and agent share a turn's `:prompt-start', so the stamp is keyed
;; by role too, letting each keep the time its own badge first appeared.

(defface me-agent-shell-label-time
  '((t :inherit shadow :weight normal))
  "Face for the timestamp appended to a chat label."
  :group 'agent-shell)

(defcustom me-agent-shell-label-time-format "%H:%M"
  "`format-time-string' format for the time shown after a chat label."
  :type 'string
  :group 'agent-shell)

(defun me--agent-shell-turn-time (prompt-start role)
  "Return the time frozen for ROLE's badge of the turn at PROMPT-START.
Stamps `current-time' the first time (PROMPT-START, ROLE) is seen and
returns that stamp thereafter, stored as a text property at PROMPT-START
so it travels with the turn across relabels."
  (let ((prop (if (eq role 'user)
                  'me--agent-shell-ts-user
                'me--agent-shell-ts-agent)))
    (or (get-text-property prompt-start prop)
        (let ((now (current-time)))
          (with-silent-modifications
            (put-text-property prompt-start (min (point-max) (1+ prompt-start))
                               prop now))
          now))))

(defun me-agent-shell-chat-label (label)
  "Draw LABEL as the default boxed badge, with the turn's time appended.
A value for `agent-shell-chat-label-function'.  LABEL's :prompt-start is
nil for the live prompt, which stays unstamped until submitted."
  (let ((badge (agent-shell-chat-boxed-label label))
        (prompt-start (map-elt label :prompt-start)))
    (if (not prompt-start)
        badge
      (let ((time (me--agent-shell-turn-time prompt-start
                                             (map-elt label :role))))
        (concat badge
                (propertize (concat "  " (format-time-string
                                          me-agent-shell-label-time-format time))
                            'face 'me-agent-shell-label-time))))))

(with-eval-after-load 'agent-shell
  (setq agent-shell-chat-label-function #'me-agent-shell-chat-label))

;;; ** Timestamps on activity-group headers

;; The collapsible header over each run of agent activity ("Ran a command",
;; "Thought", "Thought, ran 2 commands") is rendered by
;; `agent-shell-activity-group-header-label-function', called with the shell
;; state and a stable group id.  Unlike the chat labels this is a documented
;; override point, so no reaching into internals: wrap it, freeze each group's
;; time the first time its (non-empty) header is drawn -- which is when the
;; run began -- and append it.  The group id keys a per-buffer table so the
;; frozen time survives the re-renders the header gets as its run grows.

(defvar-local me--agent-shell-group-times nil
  "Hash table mapping an activity group id to its first-seen time.")

(defun me--agent-shell-group-time (state group-id)
  "Return GROUP-ID's frozen time, stamping `current-time' on first sight.
Kept in STATE's buffer so it survives the header's re-renders."
  (with-current-buffer (or (map-elt state :buffer) (current-buffer))
    (unless me--agent-shell-group-times
      (setq me--agent-shell-group-times (make-hash-table :test 'equal)))
    (or (gethash group-id me--agent-shell-group-times)
        (puthash group-id (current-time) me--agent-shell-group-times))))

(defun me--agent-shell-group-label-with-time (orig group)
  "Append the activity group's frozen time to its header label.
ORIG is the wrapped `agent-shell-activity-group-header-label-function';
GROUP is its argument, an alist of :state and :group-id.  Returns nil
untouched (an empty group), so no time shows until the run has content."
  (when-let* ((label (funcall orig group)))
    (concat label
            (propertize (concat "  " (format-time-string
                                      me-agent-shell-label-time-format
                                      (me--agent-shell-group-time
                                       (map-elt group :state)
                                       (map-elt group :group-id))))
                        ;; The header is buffer text agent-shell faces through
                        ;; `font-lock-face', so set both: `face' alone loses to
                        ;; the heading's `font-lock-face' in this context.
                        'face 'me-agent-shell-label-time
                        'font-lock-face 'me-agent-shell-label-time))))

(with-eval-after-load 'agent-shell
  (add-function :around agent-shell-activity-group-header-label-function
                #'me--agent-shell-group-label-with-time))

;;; ** @ / completion at the prompt

;; agent-shell offers @ (project files) and / (agent commands) completion
;; as `completion-at-point-functions', triggered on the @ or / char by
;; `agent-shell-completion-mode'.  Its trigger calls the built-in
;; `completion-at-point', which corfu renders through its
;; `completion-in-region-function' -- so the prompt gets the same popup UI
;; as everywhere else with no extra wiring.
;;
;; This used to need a custom trigger: `company' is a parallel frontend,
;; not a `completion-in-region' one, so `completion-at-point' bypassed it
;; and fell back to the plain `*Completions*' window.  Switching the
;; in-buffer UI to corfu removed the need entirely.

;;; ** Rich busy indicator

;; The stock busy indicator is a bare spinner glyph.  This grows it into
;; the Claude-Code-style readout -- "Ruminating… (1m 23s · ↓ 4.2k tokens)"
;; -- by advising the one function that renders the glyph.  The heartbeat
;; already re-renders the mode line on every tick, so the clock ticks for
;; free; nothing here schedules its own timer.
;;
;; Honesty about the token count: agent-shell fills :output-tokens from
;; the end-of-turn PromptResponse, so during a long think it shows the
;; PREVIOUS turn's total, not a live climb, and is absent on the very
;; first turn.  Shown when present, dropped when zero, never faked.

(defvar me-agent-shell-busy-words
  '("Thinking" "Pondering" "Ruminating" "Cogitating" "Mulling"
    "Noodling" "Percolating" "Chewing" "Brewing" "Conjuring"
    "Befuddling" "Wrangling" "Untangling" "Divining" "Scheming")
  "Whimsical gerunds cycled through while the agent works.")

(defvar-local me--agent-shell-turn-start nil
  "`float-time' when the current turn began, or nil before its first tick.
Stamped on a turn's first busy tick and left running past the idle edge
-- unlike the spinner, which clears -- so the end-of-turn summary can
report the total.  A new `:request-count' restamps it (see
`me--agent-shell-turn-id').")

(defvar-local me--agent-shell-turn-id nil
  "`:request-count' that `me--agent-shell-turn-start' was stamped for.")

(defun me--agent-shell-format-elapsed (seconds)
  "Format SECONDS as \"1m 23s\", or \"23s\" under a minute."
  (let ((s (floor seconds)))
    (if (>= s 60)
        (format "%dm %ds" (/ s 60) (% s 60))
      (format "%ds" s))))

(defun me--agent-shell-format-tokens (n)
  "Format token count N as \"4.2k\" / \"1.3m\", matching Claude Code."
  (cond ((>= n 1000000) (format "%.1fm" (/ n 1000000.0)))
        ((>= n 1000)    (format "%.1fk" (/ n 1000.0)))
        (t              (format "%d" n))))

(defun me--agent-shell-turn-word (elapsed)
  "Whimsical gerund for a turn that has run ELAPSED seconds.
Rotates every few seconds so the live indicator feels alive without
flickering each 100ms tick; the end-of-turn summary reuses the formula so
it freezes whichever word was showing when the turn finished."
  (nth (mod (floor elapsed 3) (length me-agent-shell-busy-words))
       me-agent-shell-busy-words))

(defun me--agent-shell-busy-suffix (frame)
  "Append elapsed time and token count to the busy indicator FRAME.
Advice on `agent-shell--busy-indicator-frame', which returns the spinner
glyph while busy and nil otherwise.  The turn clock is stamped on the
first busy tick of each `:request-count' and left running past the idle
edge, so `me--agent-shell-turn-summary' can report the total."
  (if (not frame)
      frame
    (let ((rc (map-elt (agent-shell--state) :request-count)))
      (unless (and me--agent-shell-turn-start
                   (equal me--agent-shell-turn-id rc))
        (setq me--agent-shell-turn-start (float-time)
              me--agent-shell-turn-id rc)))
    (let* ((elapsed (- (float-time) me--agent-shell-turn-start))
           (word (me--agent-shell-turn-word elapsed))
           (tokens (map-nested-elt (agent-shell--state) '(:usage :output-tokens)))
           (parts (list (me--agent-shell-format-elapsed elapsed))))
      (when (and (numberp tokens) (> tokens 0))
        (push (format "↓ %s tokens" (me--agent-shell-format-tokens tokens)) parts))
      (concat frame " "
              (propertize (concat word "…") 'face 'agent-shell-secondary)
              (propertize (format " (%s)" (string-join (nreverse parts) " · "))
                          'face 'agent-shell-secondary)))))

;; Leaving a trace.  The live clock vanishes when the turn ends, so freeze
;; it into the transcript the way the Claude Code terminal does -- a muted
;; "Thinking for 9s · done 20:51" where the spinner was.  The word is the
;; same gerund the spinner last showed; the completion time reuses
;; `me-agent-shell-label-time-format', matching the chat and group-header
;; stamps.  Rendered as a one-line fragment (body only, no label), the way
;; agent-shell's own Usage and stop-reason boxes land at turn end.

(defun me--agent-shell-turn-summary (_event)
  "Freeze the turn's elapsed clock into a persistent transcript line.
A `turn-complete' subscriber: reads the clock `me--agent-shell-busy-suffix'
stamped for this `:request-count' and renders a muted one-line fragment.
No-op for a turn too quick to have stamped a clock."
  (let ((state (agent-shell--state)))
    (when (and me--agent-shell-turn-start
               (equal me--agent-shell-turn-id (map-elt state :request-count)))
      (let* ((elapsed (- (float-time) me--agent-shell-turn-start))
             (summary (format "%s for %s · done %s"
                              (me--agent-shell-turn-word elapsed)
                              (me--agent-shell-format-elapsed elapsed)
                              (format-time-string me-agent-shell-label-time-format))))
        (agent-shell--update-fragment
         :state state
         :block-id (format "%s-turn-summary" (map-elt state :request-count))
         ;; Both faces: the body is buffer text agent-shell font-locks, so
         ;; `font-lock-face' holds the muted look that `face' alone loses.
         :body (propertize summary
                           'face 'agent-shell-secondary
                           'font-lock-face 'agent-shell-secondary)
         :create-new t)))))

(defun me--agent-shell-subscribe-turn-summary ()
  "Subscribe the current shell to end-of-turn summary lines.
An `agent-shell-mode-hook' entry, run once per shell with state ready."
  (agent-shell-subscribe-to
   :shell-buffer (current-buffer)
   :event 'turn-complete
   :on-event #'me--agent-shell-turn-summary))

(with-eval-after-load 'agent-shell
  (advice-add 'agent-shell--busy-indicator-frame
              :filter-return #'me--agent-shell-busy-suffix)
  (add-hook 'agent-shell-mode-hook #'me--agent-shell-subscribe-turn-summary))

;;; ** Nyan cat busy indicator

;; The frame the header shows while the agent works: an actual pixel-art
;; nyan cat flying across a rainbow and waving its legs, not a glyph bar.
;; The cat rides a fixed-width runway (rainbow tiles behind, blank ahead),
;; gliding one cell every `me-agent-shell-nyan-slowness' ticks while its
;; legs step a sprite every tick -- so the travel is slow but the wave
;; stays lively; the runway loops back to the left.
;;
;; Two sharp edges, both about how Emacs renders these cells:
;;
;;  - `agent-shell' picks `frames[tick mod len]' each heartbeat and only
;;    redraws when the frame STRING differs from the last -- and `equal' on
;;    strings ignores text properties, `display' included.  So the pixels
;;    cannot carry the animation: the UNDERLYING characters must differ.
;;    The rainbow-tile count is unique per frame, so the strings are too.
;;
;;  - Adjacent cells whose `display' values are `eq' MERGE into one display
;;    region and render as a single tile.  So every tile is built from a
;;    fresh image / space object -- reusing one collapses the whole run.
;;
;; The art is vendored from nyan-mode (see img/nyan/README) -- the cat is
;; six 25x15 sprites with the legs in different positions, the rainbow a
;; repeatable 8x15 tile.  The rich suffix above (`me--agent-shell-busy-
;; suffix') appends the clock and token count after the frame; the runway
;; is a constant width so that suffix stays put as the cat flies.
;;
;; A terminal frame can't show images, so there a glyph cat flies instead:
;; it travels a fixed-width runway laying a colored-block rainbow behind
;; it, the character layout (and so the string) changing every phase.

(defconst me-agent-shell-nyan-image-dir
  (expand-file-name "img/nyan/" me-emacs-config-dir)
  "Directory holding the vendored nyan-cat XPM sprites.
Anchored to `me-emacs-config-dir', not `user-emacs-directory': this
config splits them, pointing the latter at ~/.cache/emacs.")

(defconst me-agent-shell-nyan-scale 2
  "Integer upscale for the nyan sprites; keep it whole so pixels stay crisp.")

(defconst me-agent-shell-nyan-flight 12
  "How many cells the pixel-art cat crosses before the flight loops.
Keep it a multiple of 6 -- the number of leg sprites -- so the legs cycle
seamlessly across the loop point.")

(defconst me-agent-shell-nyan-slowness 3
  "Heartbeat ticks the cat dwells in each cell before advancing one.
The legs still wave every tick regardless (a fresh sprite per frame);
this only slows the travel, so raise it to make the cat glide more
lazily beneath the same leg animation.  The frame list is
`flight' * this long.")

(defun me--agent-shell-nyan-image (file)
  "Return the scaled, vertically centered XPM sprite FILE as a fresh image.
Fresh each call on purpose: adjacent cells whose `display' values are
`eq' merge into one display region and render a single tile, so every
cell needs its own image object (see `me--agent-shell-nyan-pixel-frame')."
  (create-image (expand-file-name file me-agent-shell-nyan-image-dir)
                'xpm nil :scale me-agent-shell-nyan-scale :ascent 'center))

(defun me--agent-shell-nyan-blank ()
  "Return a transparent cell one rainbow-tile wide, as a fresh space spec.
Fresh for the same `eq'-merging reason as `me--agent-shell-nyan-image'."
  (propertize " " 'display
              (list 'space :width (list (* me-agent-shell-nyan-scale 8)))))

(defun me--agent-shell-nyan-pixel-frame (position leg)
  "Return the pixel-art nyan-cat frame at runway POSITION with leg sprite LEG.
The cat sits POSITION cells along the runway -- POSITION rainbow tiles
behind, blank space ahead to a constant width -- with its legs at sprite
`(1+ LEG)'.  POSITION and LEG are driven separately (see the frame list),
so the cat can glide slowly while the legs wave every tick.  Every tile
is a FRESH image or space (see `me--agent-shell-nyan-image'), or an
`eq'-merged run would collapse to a single tile.  The LEG digit is the
cat cell, so consecutive frames differ even mid-cell -- which is what
makes agent-shell redraw, since `equal' ignores the `display' images."
  (concat
   (mapconcat (lambda (_)
                (propertize "r" 'display (me--agent-shell-nyan-image "rainbow.xpm")))
              (number-sequence 1 position) "")
   (propertize (number-to-string leg) 'display
               (me--agent-shell-nyan-image
                (format "nyan-frame-%d.xpm" (1+ leg))))
   (mapconcat (lambda (_) (me--agent-shell-nyan-blank))
              (number-sequence 1 (- me-agent-shell-nyan-flight 1 position)) "")))

;; --- Glyph fallback for terminal frames (no image support) ---

(defconst me-agent-shell-nyan-colors
  ["#ff5f5f" "#ff9f43" "#ffd93d" "#4ad66d" "#4dabf7" "#9775fa"]
  "Nyan-cat rainbow stripe colors, red through violet, for the glyph cat.")

(defconst me-agent-shell-nyan-runway 6
  "How many cells the glyph cat travels before the flight loops.
One rainbow stripe is laid per cell, so this also caps the trail length;
set it to the stripe count to show the full rainbow at full extension.")

(defun me--agent-shell-nyan-glyph-frame (phase)
  "Return the glyph nyan-cat frame at animation PHASE.
The cat sits PHASE cells along the runway, trailing PHASE rainbow
stripes -- stripe I in the Ith color -- and padded with spaces ahead to
a constant width.  Both the block/space layout and so the frame string
change every phase, which is what makes `agent-shell' redraw and animate."
  (let ((n (length me-agent-shell-nyan-colors)))
    (concat
     (mapconcat
      (lambda (i)
        (propertize "█" 'face `(:foreground
                                ,(aref me-agent-shell-nyan-colors (mod i n)))))
      (number-sequence 0 (1- phase))      ; nil (empty trail) when PHASE is 0
      "")
     (propertize "[,,_,,]" 'face '(:foreground "#ffb3d9"))
     (propertize ":3" 'face '(:foreground "#9aa0a6"))
     (make-string (- me-agent-shell-nyan-runway phase) ?\s))))

(defconst me-agent-shell-nyan-pixel-frames
  (when (ignore-errors (file-exists-p
                        (expand-file-name "rainbow.xpm"
                                          me-agent-shell-nyan-image-dir)))
    ;; One frame per tick of a full loop: the cat advances a cell every
    ;; `slowness' ticks (POSITION) but steps a leg sprite every tick (LEG).
    ;; `flight' being a multiple of 6 keeps both seamless at the wrap.
    (mapcar (lambda (tick)
              (me--agent-shell-nyan-pixel-frame
               (/ tick me-agent-shell-nyan-slowness)
               (mod tick 6)))
            (number-sequence
             0 (1- (* me-agent-shell-nyan-flight me-agent-shell-nyan-slowness)))))
  "Precomputed pixel-art nyan frames: the cat gliding while its legs wave, or nil.")

(defconst me-agent-shell-nyan-glyph-frames
  (mapcar #'me--agent-shell-nyan-glyph-frame
          (number-sequence 0 me-agent-shell-nyan-runway))
  "Precomputed glyph nyan frames, the terminal fallback.")

(defun me-agent-shell-nyan-frames ()
  "Return the nyan-cat frames for `agent-shell-busy-indicator-frames'.
Pixel-art sprites on an image-capable frame, the glyph cat otherwise.
A function so the custom, expanded earlier in the file, can name it
before the frame lists are built, and so the choice is made per tick in
the shell's own frame."
  (if (and me-agent-shell-nyan-pixel-frames (display-images-p))
      me-agent-shell-nyan-pixel-frames
    me-agent-shell-nyan-glyph-frames))

(provide 'ai-conf)
;;; ai-conf.el ends here
