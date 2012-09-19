
;;; bindings.el

(require 'browse-kill-ring)
(global-set-key (kbd "C-c k") 'browse-kill-ring)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f11>") 'ispell)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "<f12>") 'calendar)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-g") 'goto-line)
(global-set-key (kbd "s-s") 'save-buffer)

(global-set-key (kbd "s-/") 'comment-or-uncomment-region)

(provide 'bindings)
