;;; cmdfreq.el --- Count interactive command usage   -*- lexical-binding: t; -*-

;;; Commentary:

;; Counts every interactive command invocation, keyed by the command *and* the
;; key sequence that invoked it.  A command called through `M-x' is recorded
;; with the binding "M-x", one called programmatically (menu item, another
;; command) with an empty binding.  So the report answers two questions at
;; once: which commands do I use, and do I reach them the cheap way?
;;
;; Counts live in a hash table and are merged into `cmdfreq-file' on a timer
;; and from `kill-emacs-hook', so concurrent Emacs sessions accumulate into one
;; history instead of clobbering each other.
;;
;; Enable with `(cmdfreq-mode)' and read it with `M-x cmdfreq-show'.

;;; Code:

(require 'cl-lib)

(defgroup cmdfreq nil
  "Count interactive command usage."
  :group 'convenience)

(defcustom cmdfreq-file (locate-user-emacs-file "cmdfreq.eld")
  "File accumulating command counts across sessions."
  :type 'file)

(defcustom cmdfreq-exclude
  '(self-insert-command
    execute-extended-command
    execute-extended-command-for-buffer)
  "Commands never counted.

`self-insert-command' would drown out everything else.  The
`execute-extended-command' family is redundant: the command it
dispatches to is already recorded with the binding \"M-x\"."
  :type '(repeat symbol))

(defcustom cmdfreq-save-interval 300
  "Seconds between automatic saves, or nil to only save at exit."
  :type '(choice (const :tag "Only at exit" nil) integer))

(defvar cmdfreq--table (make-hash-table :test #'equal)
  "Counts not yet flushed to `cmdfreq-file'.
Maps (COMMAND . BINDING) to a count.")

(defvar cmdfreq--timer nil)

;;; Recording

(defun cmdfreq--binding (function)
  "Describe the key sequence that invoked FUNCTION.

`execute-extended-command' fakes up a key sequence of \"M-x NAME RET\"
before dispatching (see `set--this-command-keys' in simple.el); since
that string is exactly reconstructible, report those calls as plain
\"M-x\" rather than as one distinct binding per command name.

Return the empty string for a call that no key sequence led to: one
command calling another, or a timer.  `this-command-keys' still holds
the *previous* command's keys during such a call, so it cannot answer
this on its own; only the command the key sequence dispatched to is
`this-command'.

Compare in vector form: `this-command-keys' returns the faked sequence as
a unibyte string, which is not `equal' to the multibyte one a \"\\M-x\"
literal produces here."
  (let ((keys (this-command-keys-vector)))
    (cond ((not (eq function this-command)) "")
          ((equal keys (vconcat [?\M-x] (symbol-name function) [?\r])) "M-x")
          ((zerop (length keys)) "")
          (t (key-description keys)))))

(defun cmdfreq--record (function &rest _)
  "Count a `call-interactively' of FUNCTION.  Remaining arguments ignored."
  (when (and (symbolp function)
             (not (memq function cmdfreq-exclude)))
    (cl-incf (gethash (cons function (cmdfreq--binding function)) cmdfreq--table 0)))
  nil)

;;; Persistence

(defun cmdfreq--read ()
  "Return the counts in `cmdfreq-file' as a hash table.
An unreadable or missing file yields an empty table."
  (let ((table (make-hash-table :test #'equal)))
    (when (file-readable-p cmdfreq-file)
      (with-demoted-errors "cmdfreq: %S"
        (dolist (entry (with-temp-buffer
                         (insert-file-contents cmdfreq-file)
                         (read (current-buffer))))
          (cl-incf (gethash (cons (nth 0 entry) (nth 1 entry)) table 0)
                   (nth 2 entry)))))
    table))

(defun cmdfreq--entries (table)
  "Return TABLE as a list of (COMMAND BINDING COUNT), most used first."
  (let (entries)
    (maphash (lambda (key count)
               (push (list (car key) (cdr key) count) entries))
             table)
    (sort entries (lambda (a b) (> (nth 2 a) (nth 2 b))))))

(defun cmdfreq-save ()
  "Merge the pending counts into `cmdfreq-file'."
  (interactive)
  (when (> (hash-table-count cmdfreq--table) 0)
    (let ((merged (cmdfreq--read)))
      (maphash (lambda (key count) (cl-incf (gethash key merged 0) count))
               cmdfreq--table)
      (with-temp-file cmdfreq-file
        (let ((print-length nil) (print-level nil))
          (insert ";;; -*- lisp-data -*-  Command counts, see cmdfreq.el\n")
          (pp (cmdfreq--entries merged) (current-buffer))))
      (clrhash cmdfreq--table))))

(defun cmdfreq--total ()
  "Return a hash table of the saved counts plus the pending ones."
  (let ((total (cmdfreq--read)))
    (maphash (lambda (key count) (cl-incf (gethash key total 0) count))
             cmdfreq--table)
    total))

;;; Reporting

(defun cmdfreq--rows ()
  "Return `tabulated-list-entries' aggregated per command."
  (let ((by-command (make-hash-table :test #'eq)))
    (pcase-dolist (`(,command ,binding ,count) (cmdfreq--entries (cmdfreq--total)))
      (push (cons binding count) (gethash command by-command)))
    (let (rows)
      (maphash
       (lambda (command bindings)
         (let ((total (cl-reduce #'+ bindings :key #'cdr))
               (via-mx (cl-reduce #'+ bindings :key
                                  (lambda (b) (if (equal (car b) "M-x") (cdr b) 0)))))
           (push (list command
                       (vector (number-to-string total)
                               (number-to-string via-mx)
                               (symbol-name command)
                               (mapconcat (lambda (b)
                                            (format "%s (%d)"
                                                    (if (equal (car b) "") "--" (car b))
                                                    (cdr b)))
                                          (sort bindings (lambda (a b) (> (cdr a) (cdr b))))
                                          ", ")))
                 rows)))
       by-command)
      rows)))

(defun cmdfreq--describe ()
  "Describe the command on the current line."
  (interactive)
  (let ((command (tabulated-list-get-id)))
    (unless command (user-error "No command on this line"))
    (describe-function command)))

;; Name this after the report mode, not after `cmdfreq-mode'.  A keymap named
;; `cmdfreq-mode-map' would be adopted by `define-minor-mode' (easy-mmode.el)
;; and, since the minor mode is global, would bind RET *everywhere*.
(defvar-keymap cmdfreq-report-mode-map
  :doc "Keymap for `cmdfreq-report-mode'."
  "RET" #'cmdfreq--describe)

(define-derived-mode cmdfreq-report-mode tabulated-list-mode "CmdFreq"
  "Major mode listing command usage counts."
  (setq tabulated-list-format
        [("Count" 7 (lambda (a b) (< (string-to-number (aref (cadr a) 0))
                                     (string-to-number (aref (cadr b) 0))))
          :right-align t)
         ("M-x" 5 (lambda (a b) (< (string-to-number (aref (cadr a) 1))
                                   (string-to-number (aref (cadr b) 1))))
          :right-align t)
         ("Command" 40 t)
         ("Keys" 0 t)])
  (setq tabulated-list-sort-key '("Count" . t))
  (tabulated-list-init-header))

;;;###autoload
(defun cmdfreq-show ()
  "Show how often each interactive command was called, and through which keys.
A high count in the \"M-x\" column means the command is worth binding."
  (interactive)
  (with-current-buffer (get-buffer-create "*cmdfreq*")
    (cmdfreq-report-mode)
    (setq tabulated-list-entries #'cmdfreq--rows)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

;;; Entry point

;;;###autoload
(define-minor-mode cmdfreq-mode
  "Count interactive command invocations."
  :global t
  (if cmdfreq-mode
      (progn
        (advice-add 'call-interactively :before #'cmdfreq--record)
        (add-hook 'kill-emacs-hook #'cmdfreq-save)
        (when cmdfreq-save-interval
          (setq cmdfreq--timer
                (run-with-timer cmdfreq-save-interval cmdfreq-save-interval
                                #'cmdfreq-save))))
    (advice-remove 'call-interactively #'cmdfreq--record)
    (remove-hook 'kill-emacs-hook #'cmdfreq-save)
    (when cmdfreq--timer
      (cancel-timer cmdfreq--timer)
      (setq cmdfreq--timer nil))
    (cmdfreq-save)))

(provide 'cmdfreq)
;;; cmdfreq.el ends here
