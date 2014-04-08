(require 'ido)
(ido-mode 'both) ;; for buffers and files
(setq ido-save-directory-list-file (expand-file-name "ido.last" my-savefile-dir)
      ;; ignore these buffers during completion
      ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
                           "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
      ;; case insensitive
      ido-case-fold t
      ;; remember last directory
      ido-enable-last-directory-history t
      ido-max-work-file-list 50
      ido-use-filename-at-point nil
      ido-use-url-at-point nil
      ido-enable-flex-matching nil
      ido-max-prospects 6
      ido-confirm-unique-completion t)

(setq confirm-nonexistent-file-or-buffer nil)

;; increase minibuffer size when ido completion is active
(add-hook 'ido-minibuffer-setup-hook
          (function
           (lambda ()
             (make-local-variable 'resize-minibuffer-window-max-height))))

(provide 'ido-conf)
