;;; keybinding.el --- Key bindings config for Emacs

(global-set-key (kbd "C-c k") 'browse-kill-ring)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-<tab>") 'other-window)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'query-replace-regexp)

(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "<f8>") 'deft)
(global-set-key (kbd "<f9>") 'recentf-open-files)
;; f10 - menu
(global-set-key (kbd "<f11>") 'ispell)
(global-set-key (kbd "<f12>") 'gnus-other-frame)

(global-set-key (kbd "C-|") 'fci-mode)
(global-set-key (kbd "C-,") 'color-identifiers-mode)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c g") 'ace-jump-mode)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c <left>") 'decrease-left-margin)
(global-set-key (kbd "C-c C-<left>") 'decrease-left-margin)
(global-set-key (kbd "C-c <right>") 'increase-left-margin)
(global-set-key (kbd "C-c C-<right>") 'increase-left-margin)

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(global-set-key (kbd "s-/")
                (function (lambda ()
                            (interactive)
                            (my-apply-region-or-line
                             'comment-or-uncomment-region))))
(global-set-key (kbd "s-=") 'align-regexp)
(global-set-key (kbd "s-\\")
                (function (lambda ()
                            (interactive)
                            (my-apply-region-or-line
                             'indent-region))))

(defun my/sp-keymap()
  "My sp-keymap"
  (define-key sp-keymap (kbd "M-s f") 'sp-forward-sexp)
  (define-key sp-keymap (kbd "M-s b") 'sp-backward-sexp)

  (define-key sp-keymap (kbd "M-s d") 'sp-down-sexp)
  (define-key sp-keymap (kbd "M-s D") 'sp-backward-down-sexp)
  (define-key sp-keymap (kbd "M-s a") 'sp-beginning-of-sexp)
  (define-key sp-keymap (kbd "M-s e") 'sp-end-of-sexp)

  (define-key sp-keymap (kbd "M-s u") 'sp-up-sexp)
  ;; (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
  (define-key sp-keymap (kbd "M-s U") 'sp-backward-up-sexp)
  (define-key sp-keymap (kbd "M-s t") 'sp-transpose-sexp)

  (define-key sp-keymap (kbd "M-s n") 'sp-next-sexp)
  (define-key sp-keymap (kbd "M-s p") 'sp-previous-sexp)

  (define-key sp-keymap (kbd "M-s k") 'sp-kill-sexp)
  (define-key sp-keymap (kbd "M-s w") 'sp-copy-sexp)

  (define-key sp-keymap (kbd "M-s s") 'sp-forward-slurp-sexp)
  (define-key sp-keymap (kbd "M-s r") 'sp-forward-barf-sexp)
  (define-key sp-keymap (kbd "M-s S") 'sp-backward-slurp-sexp)
  (define-key sp-keymap (kbd "M-s R") 'sp-backward-barf-sexp)
  (define-key sp-keymap (kbd "M-s F") 'sp-forward-symbol)
  (define-key sp-keymap (kbd "M-s B") 'sp-backward-symbol)

  (define-key sp-keymap (kbd "M-s [") 'sp-select-previous-thing)
  (define-key sp-keymap (kbd "M-s ]") 'sp-select-next-thing)

  (define-key sp-keymap (kbd "M-s M-i") 'sp-splice-sexp)
  (define-key sp-keymap (kbd "M-s <delete>") 'sp-splice-sexp-killing-forward)
  (define-key sp-keymap (kbd "M-s <backspace>") 'sp-splice-sexp-killing-backward)
  (define-key sp-keymap (kbd "M-s M-<backspace>") 'sp-splice-sexp-killing-around)

  (define-key sp-keymap (kbd "M-s M-w") 'sp-wrap)
  (define-key sp-keymap (kbd "M-s M-u") 'sp-unwrap-sexp)
  (define-key sp-keymap (kbd "M-s M-b") 'sp-backward-unwrap-sexp)

  (define-key sp-keymap (kbd "M-s M-t") 'sp-prefix-tag-object)
  (define-key sp-keymap (kbd "M-s M-p") 'sp-prefix-pair-object)
  (define-key sp-keymap (kbd "M-s M-c") 'sp-convolute-sexp)
  (define-key sp-keymap (kbd "M-s M-a") 'sp-absorb-sexp)
  (define-key sp-keymap (kbd "M-s M-e") 'sp-emit-sexp)
  (define-key sp-keymap (kbd "M-s M-p") 'sp-add-to-previous-sexp)
  (define-key sp-keymap (kbd "M-s M-n") 'sp-add-to-next-sexp)
  (define-key sp-keymap (kbd "M-s M-j") 'sp-join-sexp)
  (define-key sp-keymap (kbd "M-s M-s") 'sp-split-sexp)
  (define-key sp-keymap (kbd "M-s M-r") 'sp-raise-sexp))

(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)
(global-set-key (kbd "M-]") 'rainbow-delimiters-mode)

;; (global-set-key (kbd "C-x C-;") 'ecb-activate)
;; (global-set-key (kbd "C-x C-'") 'ecb-deactivate)
;; (global-set-key (kbd "C-;") 'ecb-toggle-ecb-windows)

;; (global-set-key (kbd "C-)") 'ecb-goto-window-edit1)
;; (global-set-key (kbd "C-!") 'ecb-goto-window-directories)
;; (global-set-key (kbd "C-@") 'ecb-goto-window-sources)
;; (global-set-key (kbd "C-#") 'ecb-goto-window-methods)

(provide 'my-keybinding)
;;; keybinding.el ends here
