;;; keybinding.el --- Key bindings config for Emacs
(global-set-key (kbd "C-c k") 'browse-kill-ring)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-<tab>") 'other-window)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'query-replace-regexp)

(global-set-key (kbd "<f5>") 'undo-tree-visualize)
(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "<f8>") 'deft)
(global-set-key (kbd "<f9>") 'recentf-open-files)
;; f10 - menu
(global-set-key (kbd "<f11>") 'ispell)
(global-set-key (kbd "<f12>") 'ibus-mode)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c l") 'org-store-link)

(global-set-key (kbd "s-r") 'set-visited-file-name)
(global-set-key (kbd "s-/")
                (function (lambda ()
                            (interactive)
                            (my-apply-region-or-line 'comment-or-uncomment-region))))

(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)

(global-set-key (kbd "M-]") 'rainbow-delimiters-mode)
(global-set-key (kbd "s-=") 'align-regexp)
(global-set-key (kbd "s-\\")
                (function (lambda ()
                            (interactive)
                            (my-apply-region-or-line 'indent-region))))

(provide 'keybinding)
;;; keybinding.el ends here
