;; eink-theme.el -- E-Ink themes for emacs on E-ink

;; Author: Zhitao Gong <zhitaao.gong@gmail.com>
;; Maintainer: Zhitao Gong
;; Version: 0.1
;; Homepage: https://github.com/belak/base16-emacs

;;; Commentary:
;; base16-theme is a collection of themes built around the base16
;; concept (https://github.com/chriskempson/base16).  All themes are
;; generated from the official set of color schemes and the templates
;; which are included in this repo.

;;; Code:

(defun base16-transform-color-key (key colors)
  "Transform a given color `KEY' into a theme color using `COLORS'.

This function is meant for transforming symbols to valid colors.
If the value refers to a setting then return whatever is appropriate.
If not a setting but is found in the valid list of colors then
return the actual color value.  Otherwise return the value unchanged."
  (if (symbolp key)
      (cond

       ;; ((string= (symbol-name key) "base16-settings-fringe-bg")
       ;;  (if base16-distinct-fringe-background
       ;;      (plist-get colors :base01)
       ;;    (plist-get colors :base00)))

       ((string= (symbol-name key) "base16-settings-mode-line-box")
        (if (eq base16-highlight-mode-line 'box)
            (list :line-width 1 :color (plist-get colors :base04))
          nil))

       ;; ((string= (symbol-name key) "base16-settings-mode-line-fg")
       ;;  (if (eq base16-highlight-mode-line 'contrast)
       ;;      (plist-get colors :base05)
       ;;    (plist-get colors :base04)))

       (t
        (let ((maybe-color
               (plist-get colors (intern (concat ":" (symbol-name key))))))
          (if maybe-color
              maybe-color
            key))))
    key))


(defun base16-transform-spec (spec colors)
  "Transform a theme `SPEC' into a face spec using `COLORS'."
  (let ((output))
    (while spec
      (let* ((key (car spec))
             (value (base16-transform-color-key (cadr spec) colors)))

        ;; Append the transformed element
        (cond
         ((and (memq key '(:box :underline)) (listp value))
          (setq output (append output (list key (base16-transform-spec
                                                 value colors)))))
         (t
          (setq output (append output (list key value))))))

      ;; Go to the next element in the list
      (setq spec (cddr spec)))

    ;; Return the transformed spec
    output))

(defun base16-transform-face (spec colors)
  "Transform a face `SPEC' into an Emacs theme face definition using `COLORS'."
  (let* ((face             (car spec))
         (definition       (cdr spec)))
    (list face `((t ,(base16-transform-spec definition colors))))))

(defun base16-set-faces (theme-name colors faces)
  "Define the important part of `THEME-NAME' using `COLORS' to map the `FACES' to actual colors."
  (apply 'custom-theme-set-faces theme-name
         (mapcar #'(lambda (face)
                     (base16-transform-face face colors))
                 faces)))

(defface normal
  '((t :foreground "gray0" :distant-foreground "gray100")) "normal"
  :group 'eink-theme)
(defface normal+
  '((t :inherit normal :weight semi-bold)) "normal+"
  :group 'eink-theme)
(defface normal++
  '((t :inherit normal :weight bold)) "normal++"
  :group 'eink-theme)
(defface normal-
  '((t :inherit normal :weight semi-light)) "normal-"
  :group 'eink-theme)
(defface normal--
  '((t :inherit normal :weight light)) "normal--"
  :group 'eink-theme)
(defface normal---
  '((t :inherit normal :weight extra-light)) "normal---"
  :group 'eink-theme)

(setq org-todo-keyword-faces
          `(("TODO" :inherit normal+)
            ("NEXT" :inherit normal+)
            ("DONE" :inherit normal--)
            ("WAIT" :inherit normal- :slant italic)
            ("HOLD" :inherit normal- :slant italic)
            ("KILL" :inherit normal--)))

(defun base16-theme-define (theme-name theme-colors)
  "Define the faces for a base16 colorscheme, given a `THEME-NAME'and a plist of `THEME-COLORS'."
  (base16-set-faces
   theme-name
   theme-colors

   '(
;;; Built-in
     (bold                                         :weight semi-bold)
     (bold-italic                                  :weight semi-bold :slant italic)
     (italic                                       :slant italic :underline nil)
     (underline                                    :underline t)

;;;; basic colors
     (bookmark-menu-bookmark                       :inherit normal--)
     (border                                       :background gray)
     (cursor                                       :background white)
     (default                                      :foreground black :background white :distant-foreground white)
     (dired-symlink                                :underline t)
     (error                                        :inherit normal+)
     (header-line                                  :background nil :inherit mode-line)
     (highlight                                    :background white)
     (link                                         :inherit normal-- :underline t)
     (link-visited                                 :inherit normal--- :underline t)
     (minibuffer-prompt                            :inherit normal-)
     (region                                       :inverse-video t)
     (shadow                                       :foreground gray)
     (success                                      :inherit normal+)
     (trailing-whitespace                          :underline (:style wave))
     (vertical-border                              :foreground black)
     (warning                                      :inherit normal+)
     (widget-button                                :underline t)
     (widget-field                                 :background gray :box (:line-width 1 :color white))
     (window-divider                               :foreground white)

;;;; font-lock
     (font-lock-builtin-face                       :inherit normal+)
     (font-lock-comment-delimiter-face             :inherit normal--)
     (font-lock-comment-face                       :inherit normal--)
     (font-lock-constant-face                      :inherit normal+)
     (font-lock-doc-face                           :inherit normal-)
     (font-lock-doc-string-face                    :inherit normal-)
     (font-lock-function-name-face                 :foreground black :slant italic)
     (font-lock-keyword-face                       :inherit normal+)
     (font-lock-negation-char-face                 :inherit normal+)
     (font-lock-preprocessor-face                  :inherit normal+)
     (font-lock-regexp-grouping-backslash          :slant italic)
     (font-lock-regexp-grouping-construct          :slant italic)
     (font-lock-string-face                        :inherit normal--)
     (font-lock-type-face                          :inherit normal+)
     (font-lock-variable-name-face                 :inherit normal-)
     (font-lock-warning-face                       :inherit normal+)

;;;; isearch
     (match                                        :inverse-video t)
     (isearch                                      :inverse-video t)
     (lazy-highlight                               :inverse-video t)
     (isearch-lazy-highlight-face                  :inherit lazy-highlight) ;; was replaced with 'lazy-highlight in emacs 22
     (isearch-fail                                 :inverse-video t :inherit font-lock-warning-face)

;;;; line-numbers
     (line-number                                  :weight extra-light :foreground gray)
     (line-number-current-line                     :inherit bold)

;;;; mode-line
     (mode-line                                    :foreground black :background white :box t)
     (mode-line-buffer-id                          :inherit normal+)
     (mode-line-emphasis                           :foreground white :slant italic)
     (mode-line-highlight                          :foreground black :box nil :inherit bold)
     (mode-line-inactive                           :foreground gray :background white :box nil :weight light)

;;; Third-party

;;;; anzu-mode
     (anzu-mode-line                               :foreground black)

;;;; auctex
     (font-latex-bold-face                         :inherit normal+)
     (font-latex-doctex-documentation-face         :inherit font-lock-doc-face)
     (font-latex-italic-face                       :slant italic)
     (font-latex-math-face                         :inherit normal--)
     (font-latex-sectioning-0-face                 :inherit outline-0)
     (font-latex-sectioning-1-face                 :inherit outline-1)
     (font-latex-sectioning-2-face                 :inherit outline-2)
     (font-latex-sectioning-3-face                 :inherit outline-3)
     (font-latex-sectioning-4-face                 :inherit outline-4)
     (font-latex-sectioning-5-face                 :inherit outline-5)
     (font-latex-string-face                       :inherit font-lock-string-face)
     (font-latex-verbatim-face                     :inherit font-lock-string-face)
     (font-latex-warning-face                      :inherit font-lock-warning-face)

     (TeX-error-description-error                  :inherit error)
     (TeX-error-description-tex-said               :inherit font-lock-function-name-face)
     (TeX-error-description-warning                :inherit warning)

;;;; highlight-indent-guides
     (highlight-indent-guides-character-face       :inherit normal-)
     (highlight-indent-guides-top-character-face   :inherit normal+)

;; ;;;; avy
;;      (avy-lead-face-0                              :foreground white :background black)
;;      (avy-lead-face-1                              :foreground white :background black)
;;      (avy-lead-face-2                              :foreground white :background black)
;;      (avy-lead-face                                :foreground white :background black)
;;      (avy-background-face                          :foreground gray)
;;      (avy-goto-char-timer-face                     :inherit highlight)

;;;; clojure-mode
     (clojure-keyword-face                         :foreground black)

;;;; company-mode
     (company-tooltip                              :inherit tooltip)
     (company-scrollbar-bg                         :background white)
     (company-scrollbar-fg                         :background gray)
     (company-tooltip-annotation                   :foreground black)
     (company-tooltip-common                       :inherit font-lock-constant-face)
     (company-tooltip-selection                    :background white :inherit font-lock-function-name-face)
     (company-tooltip-search                       :inherit match)
     (company-tooltip-search-selection             :inherit match)
     (company-preview-common                       :inherit secondary-selection)
     (company-preview                              :foreground gray)
     (company-preview-search                       :inherit match)
     (company-echo-common                          :inherit secondary-selection)

;;;; cperl-mode
     (cperl-array-face                             :inherit bold :inherit font-lock-variable-name-face)
     (cperl-hash-face                              :inherit bold :slant italic :inherit font-lock-variable-name-face)
     (cperl-nonoverridable-face                    :inherit font-lock-builtin-face)

;;;; cscope-minor-mode
     (cscope-file-face                             :foreground black)
     (cscope-function-face                         :foreground black)
     (cscope-line-number-face                      :foreground black)
     (cscope-mouse-face                            :foreground gray :background white)
     (cscope-separator-face                        :foreground black :overline t :underline t :inherit bold)

;;;; csv-mode
     (csv-separator-face                           :foreground black)

;;;; diff-hl-mode
     (diff-hl-change                               :foreground black)
     (diff-hl-delete                               :foreground black)
     (diff-hl-insert                               :foreground black)

;;;; diff-mode
     (diff-added                                   :foreground black)
     (diff-changed                                 :foreground black)
     (diff-removed                                 :foreground black)
     (diff-header                                  :background white)
     (diff-file-header                             :background white)
     (diff-hunk-header                             :foreground black :background white)

;;;; dired+
     (diredp-compressed-file-suffix                :foreground black)
     (diredp-dir-heading                           :foreground nil :background nil :inherit heading)
     (diredp-dir-priv                              :foreground black :background nil)
     (diredp-exec-priv                             :foreground black :background nil)
     (diredp-executable-tag                        :foreground black :background nil)
     (diredp-file-name                             :foreground black)
     (diredp-file-suffix                           :foreground black)
     (diredp-flag-mark-line                        :background nil :inherit highlight)
     (diredp-ignored-file-name                     :foreground gray)
     (diredp-link-priv                             :foreground black :background nil)
     (diredp-mode-line-flagged                     :foreground black)
     (diredp-mode-line-marked                      :foreground black)
     (diredp-no-priv                               :background nil)
     (diredp-number                                :foreground black)
     (diredp-other-priv                            :foreground black :background nil)
     (diredp-rare-priv                             :foreground black :background nil)
     (diredp-read-priv                             :foreground black :background nil)
     (diredp-symlink                               :foreground black)
     (diredp-write-priv                            :foreground black :background nil)

;;;; ediff-mode
     (ediff-even-diff-A                            :foreground nil :background nil :inverse-video t)
     (ediff-even-diff-B                            :foreground nil :background nil :inverse-video t)
     (ediff-odd-diff-A                             :foreground gray :background nil :inverse-video t)
     (ediff-odd-diff-B                             :foreground gray :background nil :inverse-video t)

;;;; eldoc-mode
     (eldoc-highlight-function-argument            :inherit normal+)

;;;; eshell
     (eshell-ls-archive                            :foreground black)
     (eshell-ls-backup                             :foreground black)
     (eshell-ls-clutter                            :foreground black)
     (eshell-ls-directory                          :foreground black)
     (eshell-ls-executable                         :foreground black)
     (eshell-ls-missing                            :foreground black)
     (eshell-ls-product                            :foreground black)
     (eshell-ls-readonly                           :foreground white)
     (eshell-ls-special                            :foreground black)
     (eshell-ls-symlink                            :foreground black)
     (eshell-ls-unreadable                         :foreground gray)
     (eshell-prompt                                :foreground black)

;;;; evil-mode
     (evil-search-highlight-persist-highlight-face :background white :inverse-video t :inherit font-lock-warning-face)

;;;; flycheck-mode
     (flycheck-error                               :underline (:style wave :color black))
     (flycheck-info                                :underline (:style wave :color black))
     (flycheck-warning                             :underline (:style wave :color black))

;;;; flymake-mode
     (flymake-warnline                             :background white :underline black)
     (flymake-errline                              :background white :underline black)
     (flymake-warning                              :background white :underline black)
     (flymake-error                                :background white :underline black)

;;;; flyspell-mode
     (flyspell-duplicate                           :underline (:style wave :color black))
     (flyspell-incorrect                           :underline (:style wave :color black))

;;;; git-gutter-mode
     (git-gutter                                   :added                             :foreground black)
     (git-gutter                                   :deleted                           :foreground black)
     (git-gutter                                   :modified                          :foreground black)
     (git-gutter                                   :separator                         :foreground black)
     (git-gutter                                   :unchanged                         :foreground black :inverse-video t)

;;;; git-gutter+-mode
     (git-gutter+-added                            :foreground black)
     (git-gutter+-deleted                          :foreground black)
     (git-gutter+-modified                         :foreground black)
     (git-gutter+-unchanged                        :foreground black :inverse-video t)

;;;; git-gutter-fringe
     (git-gutter-fr                                :added                          :foreground black)
     (git-gutter-fr                                :deleted                        :foreground black)
     (git-gutter-fr                                :modified                       :foreground black)

;;;; grep
     (grep-context-face                            :foreground gray)
     (grep-error-face                              :inherit normal+ :underline t)
     (grep-hit-face                                :foreground black)
     (grep-match-face                              :foreground nil :background nil :inherit match)

;;;; helm
     (helm-action                                  :inherit normal-)
     (helm-buffer-directory                        :inherit normal-)
     (helm-buffer-file                             :inherit normal--)
     (helm-buffer-not-saved                        :inherit normal+)
     (helm-buffer-process                          :inherit normal--)
     (helm-buffer-saved-out                        :unerline t)
     (helm-candidate-number                        :inverse-video t)
     (helm-ff-backup-file                          :inherit dired-ignored)
     (helm-ff-denied                               :foreground black :background gray)
     (helm-ff-directory                            :inherit dired-directory)
     (helm-ff-dotted-directory                     :inherit dired-ignored)
     (helm-ff-dotted-symlink-directory             :underline t)
     (helm-ff-executable                           :slant italic)
     (helm-ff-file                                 :inherit normal-)
     (helm-ff-file-extension                       :inherit normal--)
     (helm-ff-invalid-symlink                      :inherit dired-warning)
     (helm-ff-symlink                              :inherit dired-symlink)
     (helm-M-x-key                                 :foreground black)
     (helm-match                                   :inverse-video t)
     (helm-moccur-buffer                           :foreground black)
     (helm-selection                               :inherit normal+)
     (helm-selection-line                          :inherit normal+)
     (helm-source-header                           :inherit normal+)
     (helm-visible-mark                            :inverse-video t)

;;;; hl-line-mode
     (hl-line                                      :inherit normal+)
     (col-highlight                                :background white)

;;;; hl-sexp-mode
     (hl-sexp-face                                 :background gray)

;;;; hydra
     (hydra-face-red                               :foreground black)
     (hydra-face-blue                              :foreground black)

;;;; ido-mode
     (ido-subdir                                   :foreground gray)
     (ido-first-match                              :inherit normal+)
     (ido-only-match                               :inherit normal+)
     (ido-indicator                                :foreground black :background white)
     (ido-virtual                                  :foreground gray)

;;;; imenu-list
     (imenu-list-entry-face-0                      :foreground black)
     (imenu-list-entry-face-1                      :foreground black)
     (imenu-list-entry-face-2                      :foreground black)
     (imenu-list-entry-face-3                      :foreground black)

;;;; js2-mode
     (js2-warning-face                             :underline black)
     (js2-error-face                               :foreground nil :underline black)
     (js2-external-variable-face                   :foreground black)
     (js2-function-param-face                      :foreground black)
     (js2-instance-member-face                     :foreground black)
     (js2-private-function-call-face               :foreground black)

;;;; js3-mode
     (js3-warning-face                             :underline black)
     (js3-error-face                               :foreground nil :underline black)
     (js3-external-variable-face                   :foreground black)
     (js3-function-param-face                      :foreground black)
     (js3-jsdoc-tag-face                           :foreground black)
     (js3-jsdoc-type-face                          :foreground black)
     (js3-jsdoc-value-face                         :foreground black)
     (js3-jsdoc-html-tag-name-face                 :foreground black)
     (js3-jsdoc-html-tag-delimiter-face            :foreground black)
     (js3-instance-member-face                     :foreground black)
     (js3-private-function-call-face               :foreground black)

;;;; linum-mode
     (linum                                        :foreground gray :background white)

;;;; lsp-ui-doc
     (lsp-ui-doc-header                            :inherit org-document-title)
     (lsp-ui-doc-background                        :background white)

;;;; magit
     (magit-blame-culprit                          :background white)
     (magit-blame-heading                          :background white :foreground black)
     (magit-branch                                 :foreground gray :inherit bold)
     (magit-branch-current                         :inherit normal+ :box t)
     (magit-branch-local                           :inherit normal+)
     (magit-branch-remote                          :inherit normal+)
     (magit-cherry-equivalent                      :foreground black)
     (magit-cherry-unmatched                       :foreground black)
     (magit-diff-context-highlight                 :background white :foreground black)
     (magit-diff-file-header                       :background white :foreground black)
     (magit-hash                                   :foreground black)
     (magit-header-line                            :background white :inherit normal+)
     (magit-hunk-heading                           :background gray)
     (magit-hunk-heading-highlight                 :background gray)
     (magit-diff-hunk-heading                      :background white)
     (magit-diff-hunk-heading-highlight            :background white)
     (magit-item-highlight                         :background white)
     (magit-log-author                             :foreground black)
     (magit-process-ng                             :foreground black :inherit magit-section-heading)
     (magit-process-ok                             :foreground black :inherit magit-section-heading)
     (magit-reflog-amend                           :foreground black)
     (magit-reflog-checkout                        :foreground black)
     (magit-reflog-cherry-pick                     :foreground black)
     (magit-reflog-commit                          :foreground black)
     (magit-reflog-merge                           :foreground black)
     (magit-reflog-other                           :foreground black)
     (magit-reflog-rebase                          :foreground black)
     (magit-reflog-remote                          :foreground black)
     (magit-reflog-reset                           :foreground black)
     (magit-section-highlight                      :background white)
     (magit-signature-bad                          :inherit normal+)
     (magit-signature-error                        :foreground black)
     (magit-signature-expired                      :foreground black)
     (magit-signature-good                         :foreground black)
     (magit-signature-revoked                      :foreground black)
     (magit-signature-untrusted                    :foreground black)
     (magit-tag                                    :foreground black)
;;;; mark-multiple
     (mm/master-face                               :foreground nil :background nil :inherit region)
     (mm/mirror-face                               :foreground nil :background nil :inherit region)

;;;; markdown-mode
     (markdown-url-face                            :inherit link)
     (markdown-link-face                           :foreground black :underline t)
     (markdown-pre-face                            :inherit normal)

;;;; message-mode
     (message-header-subject                       :inherit normal+ :inherit message-header-other)
     (message-header-to                            :inherit normal+ :inherit message-header-other)
     (message-header-cc                            :foreground nil :inherit message-header-to)

;;;; mic-paren
     (paren-face-match                             :foreground nil :background nil :inherit show-paren-match)
     (paren-face-mismatch                          :foreground nil :background nil :inherit show-paren-mismatch)
     (paren-face-no-match                          :foreground nil :background nil :inherit show-paren-mismatch)

;;;; mmm-mode
     (mmm-code-submode-face                        :background gray)
     (mmm-comment-submode-face                     :inherit font-lock-comment-face)
     (mmm-output-submode-face                      :background gray)

;;;; nxml-mode
     (nxml-name-face                               :foreground unspecified :inherit font-lock-constant-face)
     (nxml-attribute-local-name-face               :foreground unspecified :inherit font-lock-variable-name-face)
     (nxml-ref-face                                :foreground unspecified :inherit font-lock-preprocessor-face)
     (nxml-delimiter-face                          :foreground unspecified :inherit font-lock-keyword-face)
     (nxml-delimited-data-face                     :foreground unspecified :inherit font-lock-string-face)
     (rng-error-face                               :underline black)

;;; outline-mode
     (outline-1                                    :inherit normal+)
     (outline-2                                    :inherit normal+)
     (outline-3                                    :inherit normal+)
     (outline-4                                    :inherit normal)
     (outline-5                                    :inherit normal-)
     (outline-6                                    :inherit normal-)
     (outline-7                                    :inherit normal-)

;;;; org-mode
     (org-agenda-structure                         :inherit normal-)
     (org-agenda-date                              :underline nil)
     (org-agenda-done                              :inherit normal--)
     (org-agenda-dimmed-todo-face                  :inherit normal--)
     (org-agenda-clocking                          :slant italic)
     (org-block                                    :inherit normal-)
     (org-checkbox                                 :inherit normal-)
     (org-code                                     :inherit normal-)
     (org-column-title                             :inherit normal+ :underline t)
     (org-date                                     :inherit normal- :underline t)
     (org-document-info                            :inherit normal-)
     (org-document-info-keyword                    :inherit normal-)
     (org-document-title                           :inherit normal+)
     (org-done                                     :inherit normal--)
     (org-ellipsis                                 :inherit normal--)
     (org-footnote                                 :inherit normal--)
     (org-formula                                  :inherit normal-)
     (org-habit-clear-face                         :background white)
     (org-habit-clear-future-face                  :background white)
     (org-habit-overdue-face                       :background white)
     (org-headline-done                            :inherit normal--)
     (org-hide                                     :inherit normal--)
     (org-link                                     :inherit link)
     (org-priority                                 :inherit normal--)
     (org-scheduled-previously                     :inherit normal-)
     (org-scheduled-today                          :inherit normal-)
     (org-special-keyword                          :inherit normal-- :inherit bold)
     (org-tag                                      :inherit normal+)
     (org-upcoming-deadline                        :slant italic)
     (org-warning                                  :inherit normal+)

;;;; paren-face-mode
     (paren-face                                   :inherit normal--)

;;;; python-mode
     (py-builtins-face                             :inherit normal+)

;;;; rainbow-delimiters
     (rainbow-delimiters-depth-1-face              :inherit normal-)
     (rainbow-delimiters-depth-2-face              :inherit normal-)
     (rainbow-delimiters-depth-3-face              :inherit normal-)
     (rainbow-delimiters-depth-4-face              :inherit normal-)
     (rainbow-delimiters-depth-5-face              :inherit normal-)
     (rainbow-delimiters-depth-6-face              :inherit normal-)
     (rainbow-delimiters-depth-7-face              :inherit normal-)
     (rainbow-delimiters-depth-8-face              :inherit normal-)
     (rainbow-delimiters-depth-9-face              :inherit normal-)

;;;; regex-tool
     (regex-tool-matched-face                      :inherit match)

;;;; sh-mode
     (sh-heredoc                                   :inherit font-lock-string-face)
     (sh-quoted-exec                               :inherit font-lock-preprocessor-face)

;;;; show-paren-mode
     (show-paren-match                             :inherit normal++)
     (show-paren-mismatch                          :strike-through t)

;;;; undo-tree-mode
     (undo-tree-visualizer-default-face            :inherit normal-)
     (undo-tree-visualizer-current-face            :inherit normal+)

;;;; which-func-mode
     (which-func                                   :inherit normal+)

;;;; whitespace-mode
     (whitespace-hspace                            :overline t)
     (whitespace-line                              :background white :strike-through t)
     (whitespace-newline                           :background black)
     (whitespace-space-after-tab                   :background black)
     (whitespace-space-before-tab                  :background black)
     (whitespace-tab                               :strike-through t)
     (whitespace-trailing                          :underline (:style wave))
     (whitespace-empty                             :underline (:style wave))
     ;; (whitespace-indentation                       :inherit normal)
     (whitespace-space                             :background white)

     (web-mode-current-element-highlight-face      :inherit normal++)

     (me-dired-dim-0                               :inherit normal--)
     (me-dired-dim-1                               :inherit normal---)
     (me-dired-executable                          :slant italic)

     ))

  ;; Anything leftover that doesn't fall neatly into a face goes here.
  (let ((white (plist-get theme-colors :white))
        (gray (plist-get theme-colors :gray))
        (black (plist-get theme-colors :black)))

    ;; Emacs 24.3 changed ’ansi-term-color-vector’ from a vector of colors
    ;; to a vector of faces.

    (defun me//propertize-evil-tag (str inverse-video)
      "Make the evil state notifier pertty.
      Propertize STR with foreground FG and background BG color."
      (propertize str 'face
                  `((:inverse-video ,inverse-video))))

    (defun me//colorize-evil-tag (state-color-list)
      "Change evil tag color according to STATE-COLOR-LIST."
      (dolist (elm state-color-list)
        (set (intern-soft (concat "evil-" (plist-get elm :state) "-state-cursor"))
             (if (plist-get elm :edit) 'box 'hollow))
        (set (intern-soft (concat "evil-" (plist-get elm :state) "-state-tag"))
             (me//propertize-evil-tag (plist-get elm :tag) (not (plist-get elm :edit))))))

    (let ((state-color-list
           `((:state "insert" :edit t   :tag " <I> ")
             (:state "emacs"  :edit t   :tag " <E> ")
             (:state "normal" :edit nil :tag " <N> ")
             (:state "visual" :edit nil :tag " <V> ")
             (:state "motion" :edit nil :tag " <M> "))))
      (me//colorize-evil-tag state-color-list))

    (add-hook 'activate-mark-hook #'(lambda () (setq cursor-type 'hollow)))
    (add-hook 'deactivate-mark-hook #'(lambda () (setq cursor-type 'box)))

    ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(defvar eink-colors
  '(:white "gray100"
    :gray  "gray50"
    :black "gray0")
  "E-ink recognizable colors.

Actually, only black and white are recognized, `gray' is rendered
as light weight.")

(deftheme eink)

;; Add all the faces to the theme
(base16-theme-define 'eink eink-colors)

;; Mark the theme as provided
(provide-theme 'eink)

(provide 'eink-theme)

;;; eink-theme.el ends here
