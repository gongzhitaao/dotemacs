;;; evil-conf.el --- evil config                     -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'evil)
(evil-mode 1)

(setq cursor-type 'box)
(setq evil-insert-state-cursor "green"
      evil-emacs-state-cursor "wheat"
      evil-normal-state-cursor "red"
      evil-visual-state-cursor "orange"
      evil-motion-state-cursor "blue")

(require 'view)

;; vim command mode
(define-key evil-normal-state-map (kbd "<backspace>") #'View-scroll-half-page-backward)
(define-key evil-normal-state-map (kbd "<delete>") #'View-scroll-half-page-forward)
(define-key evil-normal-state-map (kbd "<escape>") #'evil-emacs-state)
(define-key evil-normal-state-map (kbd "C-e") #'move-end-of-line)
(define-key evil-normal-state-map (kbd "C-z") #'delete-other-windows)
(define-key evil-normal-state-map (kbd "a") nil)
(define-key evil-normal-state-map (kbd "b") #'helm-mini)
(define-key evil-normal-state-map (kbd "i") nil)
(define-key evil-normal-state-map (kbd "o") #'me//evil-open-below)
(define-key evil-normal-state-map (kbd "O") #'me//evil-open-above)
(define-key evil-normal-state-map (kbd "q") #'bury-buffer)
(define-key evil-normal-state-map (kbd "Q") #'kill-this-buffer)
(define-key evil-normal-state-map (kbd "s") #'helm-swoop)

;; emacs mode
(define-key evil-emacs-state-map (kbd "<escape>") #'evil-force-normal-state)
(define-key evil-emacs-state-map (kbd "C-z") #'delete-other-windows)

;; motion mode
(define-key evil-motion-state-map (kbd "<tab>") #'forward-button)
(define-key evil-motion-state-map (kbd "d") #'View-scroll-half-page-forward)
(define-key evil-motion-state-map (kbd "u") #'View-scroll-half-page-backward)

(add-to-list 'evil-emacs-state-modes 'image-mode)
(add-to-list 'evil-emacs-state-modes 'dired-mode)
(add-to-list 'evil-emacs-state-modes 'image-dired-thumbnail-mode)
(add-to-list 'evil-emacs-state-modes 'mu4e-compose-mode)

(defun me//evil-open-above (count)
  "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (unless (eq evil-want-fine-undo t)
    (evil-start-undo-step))
  (evil-insert-newline-above)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (evil-emacs-state 1)
  (when evil-auto-indent
    (indent-according-to-mode)))

(defun me//evil-open-below (count)
  "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (unless (eq evil-want-fine-undo t)
    (evil-start-undo-step))
  (push (point) buffer-undo-list)
  (evil-insert-newline-below)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (evil-emacs-state 1)
  (when evil-auto-indent
    (indent-according-to-mode)))

(telephone-line-defsegment
  me//telephone-line-airline-position-segment
  (&optional lines columns)
  "Position segment imitating vim-airline's appearance.  Optional
LINES and COLUMNS set padding on lines/columns."
  (let* ((l (number-to-string (if lines lines 4)))
         (c (number-to-string (if columns columns 3))))
    (if (eq major-mode 'paradox-menu-mode)
        (telephone-line-raw mode-line-front-space t)
      `((-3 "%p") ,(concat "  %I  %" l "l:%" c "c")))))

(telephone-line-defsegment me//telephone-line-atom-encoding-segment ()
  "Displays the encoding of the buffer the same way Atom does.
Set HIDE-UTF8 to display nothing for UTF-8, as it can be an assumed default.
Adapted from doom-modeline."
  (unless (derived-mode-p 'special-mode)
    (let ((sys (coding-system-plist buffer-file-coding-system)))
      (cond ((memq (plist-get sys :category)
                   '(coding-category-undecided coding-category-utf-8))
             nil)
            (t (upcase (symbol-name (plist-get sys :name))))))))

(telephone-line-defsegment me//telephone-line-pdf-segment ()
  (if (eq major-mode 'pdf-view-mode)
      (propertize (me//pdf-view-page-number)
                  'face '(:inherit)
                  'display '(raise 0.0)
                  'mouse-face '(:box 1)
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 (lambda ()
                                         (interactive)
                                         (pdf-view-goto-page))))))

;; The followings are telephone modeline, which is not part of the evil package.
;; However, it is better to combine these two settings.
(setq telephone-line-lhs
      '((evil . (telephone-line-evil-tag-segment))
        (accent . (telephone-line-minor-mode-segment
                   me//telephone-line-atom-encoding-segment
                   me//telephone-line-pdf-segment
                   telephone-line-vc-segment))
        (nil . (telephone-line-buffer-segment))))
(setq telephone-line-rhs
      '((nil    . (telephone-line-process-segment
                   telephone-line-misc-info-segment))
        (accent . (telephone-line-major-mode-segment))
        (evil   . (me//telephone-line-airline-position-segment))))
(telephone-line-mode t)

(provide 'evil-conf)
;;; evil-conf.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
