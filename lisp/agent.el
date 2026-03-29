;;; agent.el --- Generic AI CLI wrapper using eat -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A lightweight, backend-agnostic wrapper for AI CLI tools (Claude Code,
;; Codex, etc.) using the eat terminal emulator.
;;
;; Usage:
;;   (use-package agent
;;     :load-path "lisp"
;;     :bind-keymap ("C-c a" . agent-command-map))

;;; Code:

(require 'project)
(require 'eat)

;;; * Customization

(defgroup agent nil
  "Generic AI CLI wrapper for Emacs."
  :group 'tools
  :prefix "agent-")

(defcustom agent-program "claude"
  "The CLI program to run."
  :type 'string)

(defcustom agent-program-switches nil
  "Extra command-line switches always passed to the program."
  :type '(repeat string))

(defcustom agent-resume-switch "--resume"
  "CLI flag to resume a previous session."
  :type 'string)

(defcustom agent-slash-commands
  '("/bug" "/clear" "/compact" "/config" "/cost" "/doctor" "/help"
    "/init" "/login" "/logout" "/memory" "/model" "/permissions"
    "/review" "/status" "/terminal-setup" "/vim")
  "List of slash commands available for completion."
  :type '(repeat string))

(defcustom agent-confirm-before-kill t
  "Whether to confirm before killing an agent process."
  :type 'boolean)

(defcustom agent-large-buffer-threshold 100000
  "Character count above which sending a buffer requires confirmation."
  :type 'integer)

(defcustom agent-display-buffer-function #'agent-display-buffer-below
  "Function to display the agent buffer.  Called with one argument, the buffer."
  :type 'function)

;;; * Internal state

(defvar agent--buffers (make-hash-table :test 'equal)
  "Map from directory to list of agent buffers.")

(defvar agent--scroll-timer nil
  "Timer that keeps agent windows scrolled to bottom.")

;;; * Keymap

(defvar agent-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'agent-start)
    (define-key map (kbd "r") #'agent-resume)
    (define-key map (kbd "s") #'agent-send-region)
    (define-key map (kbd "c") #'agent-send-command)
    (define-key map (kbd "/") #'agent-slash-command)
    (define-key map (kbd "x") #'agent-send-command-with-context)
    (define-key map (kbd "t") #'agent-toggle)
    (define-key map (kbd "b") #'agent-switch-to-buffer)
    (define-key map (kbd "k") #'agent-kill)
    (define-key map (kbd "y") #'agent-send-return)
    (define-key map (kbd "n") #'agent-send-escape)
    (define-key map (kbd "1") #'agent-send-1)
    (define-key map (kbd "2") #'agent-send-2)
    (define-key map (kbd "3") #'agent-send-3)
    map)
  "Keymap for agent commands.  Bind under a prefix in your init file.")

;;; * Helpers

(defun agent--directory ()
  "Return the project root or `default-directory'."
  (if-let ((proj (project-current)))
      (project-root proj)
    default-directory))

(defun agent--buffer-name (dir &optional instance)
  "Return buffer name for DIR and optional INSTANCE name."
  (let ((short (abbreviate-file-name (file-truename dir))))
    (if instance
        (format "*agent:%s:%s*" short instance)
      (format "*agent:%s*" short))))

(defun agent--buffer-p (buffer)
  "Return non-nil if BUFFER is an agent buffer."
  (string-prefix-p "*agent:" (buffer-name buffer)))

(defun agent--live-buffers (&optional dir)
  "Return live agent buffers, optionally filtered to DIR."
  (let ((bufs (seq-filter (lambda (b)
                            (and (buffer-live-p b)
                                 (agent--buffer-p b)
                                 (get-buffer-process b)))
                          (buffer-list))))
    (if dir
        (let ((prefix (format "*agent:%s" (abbreviate-file-name (file-truename dir)))))
          (seq-filter (lambda (b) (string-prefix-p prefix (buffer-name b))) bufs))
      bufs)))

(defun agent--get-buffer ()
  "Get the agent buffer for the current directory, prompting if needed."
  (let ((bufs (agent--live-buffers (agent--directory))))
    (cond
     ((= (length bufs) 1) (car bufs))
     ((> (length bufs) 1)
      (let* ((names (mapcar #'buffer-name bufs))
             (choice (completing-read "Select agent: " names nil t)))
        (get-buffer choice)))
     ;; No buffer for this dir; try all buffers
     (t (let ((all (agent--live-buffers)))
          (cond
           ((= (length all) 1) (car all))
           ((> (length all) 1)
            (let* ((names (mapcar #'buffer-name all))
                   (choice (completing-read "Select agent: " names nil t)))
              (get-buffer choice)))
           (t nil)))))))

(defun agent--send-string (string)
  "Send STRING to the agent terminal in the current buffer."
  (when (and (bound-and-true-p eat-terminal))
    (eat-term-send-string eat-terminal string)))

(defun agent--do-send (text)
  "Send TEXT to the agent buffer, returning the buffer or nil."
  (if-let ((buf (agent--get-buffer)))
      (progn
        (with-current-buffer buf
          (agent--send-string text)
          (sit-for 0.05)
          (agent--send-string (kbd "RET")))
        (funcall agent-display-buffer-function buf)
        buf)
    (message "No running agent.  Start one with `agent-start'.")
    nil))

(defun agent-display-buffer-below (buffer)
  "Display BUFFER below the current window."
  (display-buffer buffer '((display-buffer-below-selected))))

(defun agent--scroll-all-windows ()
  "Keep all non-selected agent windows scrolled to bottom."
  (if (agent--live-buffers)
      (dolist (win (window-list nil 'no-mini))
        (when (and (agent--buffer-p (window-buffer win))
                   (not (eq win (selected-window))))
          (let* ((buf (window-buffer win))
                 (eob (with-current-buffer buf (point-max))))
            (set-window-point win eob)
            (with-current-buffer buf
              (save-excursion
                (goto-char eob)
                (forward-line (- (window-body-height win)))
                (set-window-start win (line-beginning-position)))))))
    (when agent--scroll-timer
      (cancel-timer agent--scroll-timer)
      (setq agent--scroll-timer nil))))

(defun agent--cleanup-buffer ()
  "Remove current buffer from tracking.  Added to `kill-buffer-hook'."
  (remhash (buffer-name (current-buffer)) agent--buffers))

;;; * Interactive commands

;;;###autoload
(defun agent-start (&optional arg)
  "Start an agent in the project directory.

With \\[universal-argument], switch to the buffer after creation.
With \\[universal-argument] \\[universal-argument], prompt for directory."
  (interactive "P")
  (let* ((dir (if (equal arg '(16))
                  (read-directory-name "Directory: ")
                (agent--directory)))
         (existing (agent--live-buffers dir))
         (instance (when existing
                     (let ((name (read-string
                                  (format "Instance name (existing: %s): "
                                          (mapcar #'buffer-name existing)))))
                       (when (string-empty-p name)
                         (error "Instance name cannot be empty"))
                       name)))
         (buf-name (agent--buffer-name dir instance))
         (default-directory dir)
         (switches agent-program-switches)
         (process-adaptive-read-buffering nil))

    (unless (executable-find agent-program)
      (error "Program '%s' not found in PATH" agent-program))

    (let ((buf (apply #'eat-make
                      (string-trim-right (string-trim buf-name "\\*") "\\*")
                      agent-program nil switches)))
      (unless (buffer-live-p buf)
        (error "Failed to create agent buffer"))

      (with-current-buffer buf
        (eat-semi-char-mode)
        (add-hook 'kill-buffer-hook #'agent--cleanup-buffer nil t))

      (unless agent--scroll-timer
        (setq agent--scroll-timer
              (run-with-timer 0 0.2 #'agent--scroll-all-windows)))

      (puthash buf-name buf agent--buffers)
      (funcall agent-display-buffer-function buf)

      (when (equal arg '(4))
        (pop-to-buffer buf))

      buf)))

;;;###autoload
(defun agent-resume (&optional arg)
  "Resume a previous agent session.

With \\[universal-argument], switch to buffer.
With \\[universal-argument] \\[universal-argument], prompt for directory."
  (interactive "P")
  (let ((agent-program-switches
         (append agent-program-switches (list agent-resume-switch))))
    (agent-start arg)))

;;;###autoload
(defun agent-send-region (&optional arg)
  "Send the region (or buffer) to the agent.

With \\[universal-argument], prompt for instructions to prepend.
With \\[universal-argument] \\[universal-argument], also switch to agent buffer."
  (interactive "P")
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (when (or (<= (buffer-size) agent-large-buffer-threshold)
                           (yes-or-no-p "Buffer is large.  Send anyway? "))
                   (buffer-substring-no-properties (point-min) (point-max)))))
         (prompt (when (consp arg)
                   (read-string "Instructions: ")))
         (full (cond
                ((and prompt text) (format "%s\n\n%s" prompt text))
                (text text)
                (t nil))))
    (when full
      (let ((buf (agent--do-send full)))
        (when (and (equal arg '(16)) buf)
          (pop-to-buffer buf))))))

;;;###autoload
(defun agent-send-command (&optional arg)
  "Prompt for text and send it to the agent.

With \\[universal-argument], switch to agent buffer."
  (interactive "P")
  (let* ((text (read-string "Send to agent: " nil 'agent-command-history))
         (buf (agent--do-send text)))
    (when (and arg buf)
      (pop-to-buffer buf))))

;;;###autoload
(defun agent-send-command-with-context (&optional arg)
  "Send a command with current file context.

Includes the file path and line range if region is active.
With \\[universal-argument], switch to agent buffer."
  (interactive "P")
  (let* ((file (or buffer-file-name (buffer-name)))
         (ref (if (use-region-p)
                  (format "@%s:%d-%d" file
                          (line-number-at-pos (region-beginning) t)
                          (line-number-at-pos (region-end) t))
                (format "@%s:%d" file (line-number-at-pos nil t))))
         (prompt (read-string (format "Command (context: %s): " ref)
                              nil 'agent-command-history))
         (text (format "%s %s" prompt ref))
         (buf (agent--do-send text)))
    (when (and arg buf)
      (pop-to-buffer buf))))

;;;###autoload
(defun agent-slash-command ()
  "Send a slash command to the agent, chosen via completion."
  (interactive)
  (let ((cmd (completing-read "Slash command: " agent-slash-commands nil nil "/")))
    (agent--do-send cmd)))

;;;###autoload
(defun agent-toggle ()
  "Toggle visibility of the agent window."
  (interactive)
  (if-let ((buf (agent--get-buffer)))
      (if-let ((win (get-buffer-window buf)))
          (delete-window win)
        (funcall agent-display-buffer-function buf))
    (message "No running agent.")))

;;;###autoload
(defun agent-switch-to-buffer ()
  "Switch to an agent buffer."
  (interactive)
  (if-let ((buf (agent--get-buffer)))
      (pop-to-buffer buf)
    (message "No running agent.")))

;;;###autoload
(defun agent-kill ()
  "Kill the agent process and buffer."
  (interactive)
  (when-let ((buf (agent--get-buffer)))
    (when (or (not agent-confirm-before-kill)
              (yes-or-no-p "Kill agent? "))
      (with-current-buffer buf
        (eat-kill-process)
        (kill-buffer buf))
      (message "Agent killed."))))

;;; * Quick responses

;;;###autoload
(defun agent-send-return ()
  "Send Return to the agent."
  (interactive)
  (agent--do-send ""))

;;;###autoload
(defun agent-send-escape ()
  "Send Escape to the agent."
  (interactive)
  (when-let ((buf (agent--get-buffer)))
    (with-current-buffer buf
      (agent--send-string "\e"))
    (funcall agent-display-buffer-function buf)))

;;;###autoload
(defun agent-send-1 ()
  "Send \"1\" to the agent."
  (interactive)
  (agent--do-send "1"))

;;;###autoload
(defun agent-send-2 ()
  "Send \"2\" to the agent."
  (interactive)
  (agent--do-send "2"))

;;;###autoload
(defun agent-send-3 ()
  "Send \"3\" to the agent."
  (interactive)
  (agent--do-send "3"))

(provide 'agent)
;;; agent.el ends here
