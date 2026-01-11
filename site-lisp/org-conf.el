;;; org-conf.el --- Org mode config (straight.el version)  -*- lexical-binding: t; -*-

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

;; Org mode configuration using straight.el for package management.
;; Use with init2.el.

;;; Code:

;; Install org BEFORE loading other org-related packages.
;; This ensures the newer org is loaded before the built-in one.
(use-package org)

(defun me--init-org ()
  "Init orgmode."
  (turn-on-auto-fill)
  (flyspell-mode)
  (org-num-mode)

  (defun me--org-show-context-advice (&rest ignore)
    (org-show-context 'default))
  (advice-add 'ispell-highlight-spelling-error :before
              'me--org-show-context-advice)

  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))


(use-package org
  :mode ("\\.org\\'" . org-mode)
  :custom
  ( org-adapt-indentation nil)
  ( org-agenda-tags-column 0)
  ( org-auto-align-tags nil)
  ( org-directory (file-name-as-directory (file-name-concat me-emacs-data-dir "notes")))
  ( org-display-remote-inline-images 'cache)
  ( org-ellipsis "…")
  ( org-export-backends '(ascii beamer html latex md))
  ( org-fold-catch-invisible-edits 'smart)
  ( org-hide-emphasis-markers t)
  ( org-hide-macro-markers t)
  ( org-hierarchical-todo-statistics nil)
  ( org-image-actual-width nil)
  ( org-insert-heading-respect-content t)
  ( org-log-done 'time)
  ( org-modules '(ol-bbdb ol-bibtex ol-gnus org-clock org-tempo org-habit org-table))
  ( org-preview-latex-image-directory (file-name-as-directory (file-name-concat me-emacs-cache-dir "ltximg/")))
  ( org-provide-todo-statistics t)
  ( org-special-ctrl-a/e t)
  ( org-src-fontify-natively t)
  ( org-src-preserve-indentation t)
  ( org-startup-folded 'content)
  ( org-startup-with-inline-images t)
  ( org-support-shift-select t)
  ( org-tags-column 0)
  ( org-time-stamp-custom-formats '("<%m/%d/%y %a>" . "<%Y-%m-%d %a %R %z>"))
  ( org-todo-keywords
   '((sequence "TODO(t!)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "KILL(k@)")))
  ( org-treat-S-cursor-todo-selection-as-state-change nil)
  ( org-treat-insert-todo-heading-as-state-change t)
  ( org-use-fast-tag-selection 'auto)
  ( org-use-fast-todo-selection t)

  :config
  (bind-keys :map org-mode-map
             ("C-J" . me/join-next-line))

  (add-hook 'org-mode-hook #'me--init-org)
  (define-key org-mode-map [remap fill-paragraph] #'org-fill-paragraph)
  (setopt org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
  (define-key org-mode-map (kbd "C-c [") nil)
  (setf (alist-get "+" org-emphasis-alist) '((:strike-through "red"))))

(use-package org-capture
  :custom
  ( org-capture-templates
   `(
     ("l" "Log daily" plain
      (file+olp+datetree
       ,(file-name-concat me-emacs-data-dir
                          (format-time-string "time-machine/%Y.org")))
      "%?"
      :empty-lines 1
      :jump-to-captured t
      :tree-type week)

     ("t" "TODO" entry
      (file+headline
       ,(file-name-concat (format-time-string "%Y") "inbox") "Refile Me")
      (file "capture/todo.org")
      :empty-lines 1
      :jump-to-captured t)
     )))

(use-package org-appear
  :custom
  ( org-appear-autolinks t)
  :hook (org-mode . org-appear-mode))

(use-package org-refile
  :custom
  ( org-refile-targets '((org-agenda-files :maxlevel . 4)))
  ( org-refile-use-outline-path 'file)
  ( org-outline-path-complete-in-steps nil))

(defun me--org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun me--org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(use-package org-modern
  :config
  (global-org-modern-mode))

;;; ** Time related

(use-package org-habit
  :custom
  ( org-habit-following-days 1)
  ( org-habit-graph-column   100)
  ( org-habit-preceding-days 28))

(use-package org-clock
  :custom
  (org-clock-history-length 32)
  (org-clock-idle-time 10)
  (org-clock-in-resume t)
  (org-clock-into-drawer t)
  (org-clock-mode-line-total 'today)
  (org-clock-persist t)
  (org-clock-persist-file
   (file-name-concat me-emacs-cache-dir "org-clock-save.el"))
  (org-clock-sound "~/.local/share/sounds/casio-hour-chime.wav")
  (org-log-into-drawer t)

  :config
  (org-clock-persistence-insinuate))

(use-package org-timer
  :custom
  (org-timer-default-timer "0:25")
  (org-timer-display 'both))

(use-package appt
  :config
  (appt-activate 1)
  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt))

;;; ** Agenda

(use-package org-agenda
  :custom
  (org-agenda-clockreport-parameter-plist
   '(:link t :maxlevel 2 :compact t :formula % :fileskip0 t ::stepskip0 t))
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-compact-blocks nil)
  (org-agenda-dim-blocked-tasks t)
  (org-agenda-files (file-name-concat org-directory
                                      (format-time-string "%Y") "agenda"))
  (org-agenda-include-diary nil)
  (org-agenda-prefix-format
   '((agenda   . " %-16:c%-13t%-20 s")
     (timeline . "  % s")
     (todo     . " %-16:c")
     (tags     . " %-16:c")
     (search   . " %-16:c")))
  (org-agenda-remove-tags t)
  (org-agenda-show-all-dates t)
  (org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)
  (org-agenda-sorting-strategy
   '(( agenda habit-down time-up priority-down user-defined-down
       category-keep)
     ((todo priority-down category-keep))
     ((tags priority-down category-keep))
     (search category-keep)))
  (org-agenda-span 'day)
  (org-agenda-start-with-log-mode nil)
  (org-agenda-tags-column -130)

  :config
  (defun me--init-org-agenda ()
    (set (make-local-variable 'writeroom-width) 150)
    (hl-line-mode))
  (add-hook 'org-agenda-mode-hook #'me--init-org-agenda)

  (setcar org-agenda-time-grid '(daily today))

  (advice-add 'org-agenda-goto :after
              (lambda (&rest args)
                (org-narrow-to-subtree)))

  (defun me--org-agenda-cmp-user-defined (a b)
    "Compare the todo states of strings A and B."
    (let* ((todo-priority '(("NEXT" . 6)
                            ("TODO" . 5)
                            ("WAIT" . 4)
                            ("HOLD" . 3)
                            ("DONE" . 2)
                            ("KILL" . 1)))
           (ma (or (get-text-property 1 'org-marker a)
                   (get-text-property 1 'org-hd-marker a)))
           (mb (or (get-text-property 1 'org-marker b)
                   (get-text-property 1 'org-hd-marker b)))
           (fa (and ma (marker-buffer ma)))
           (fb (and mb (marker-buffer mb)))
           (ta (or (get-text-property 1 'todo-state a) "KILL"))
           (tb (or (get-text-property 1 'todo-state b) "KILL"))
           (la (cdr (assoc ta todo-priority)))
           (lb (cdr (assoc tb todo-priority))))
      (cond ((< la lb) -1)
            ((< lb la) +1))))

  (setopt org-agenda-cmp-user-defined #'me--org-agenda-cmp-user-defined)
  (setopt org-agenda-custom-commands
        '(("d" "Daily agenda and all TODOs"
           ((agenda "" ((org-agenda-span 'day)))
            (tags "+PRIORITY=\"A\""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header
                    "High-priority unfinished tasks")))
            (alltodo ""
                     ((org-agenda-skip-function
                       '(or (me--org-skip-subtree-if-habit)
                            (me--org-skip-subtree-if-priority ?A)
                            (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header
                       "ALL normal priority tasks:"))))
           ((org-agenda-compact-blocks nil))))))


;;; ** Presentation

(defun me--org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))

(defun me--org-present-start ()
  ;; Need this to remove the face remap.
  (setq-local me--org-present-cookies '())

  (dolist (face '((default . 1.75)
                  (header-line . 4.0)
                  (org-block . 1.25)
                  (org-block-begin-line . 0.75)
                  (org-code . 1.75)
                  (org-document-title . 3)
                  (org-level-1 . 2.75)
                  (org-level-2 . 2.5)
                  (org-level-3 . 2.25)
                  (org-verbatim . 1.75)))
    (add-to-list 'me--org-present-cookies
                 (face-remap-add-relative (car face) :height (cdr face))))

  (writeroom-mode -1)

  ;; Set a blank header line string to create blank space at the top
  (setq-local header-line-format " ")

  ;; Display inline images automatically
  (org-display-inline-images)

  (setq visual-fill-column-width 90
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (variable-pitch-mode)
  (org-appear-mode -1))

(defun me--org-present-end ()
  ;; Reset font customizations
  (when (boundp 'me--org-present-cookies)
      (dolist (cookie me--org-present-cookies)
        (face-remap-remove-relative cookie))
      (makunbound 'me--org-present-cookies))

  ;; Clear the header line string so that it isn't displayed
  (setq-local header-line-format nil)
  (visual-fill-column-mode -1)
  (variable-pitch-mode -1)
  (org-remove-inline-images)
  (org-appear-mode 1)
  (writeroom-mode 1))

;;; ** Export

(use-package ox
  :custom
  (org-export-global-macros
   '(("tex" . "@@latex:$1@@")
     ("html" . "@@html:$1@@"))))

(use-package org-contrib)

(use-package ox-extra
  :after org-contrib
  :config (ox-extras-activate '(ignore-headlines)))

;;; *** HTML

(use-package ox-html
  :custom
  (org-html-allow-name-attribute-in-anchors t)
  (org-html-doctype "html5")
  (org-html-head-include-default-style nil)
  (org-html-head-include-scripts nil)
  (org-html-html5-fancy t)
  (org-html-htmlize-output-type 'css)
  (org-html-postamble t)
  (org-html-postamble-format
   '(("en" "<a class=\"author\"
           href=\"http://gongzhitaao.org\">%a</a> / <span
           class=\"date\">%T</span><span class=\"creator\">%c</span>"))))

;;; *** LaTeX

(use-package ox-bibtex
  :after org-contrib)

(use-package ox-latex
  :custom
  (org-latex-caption-above nil)
  (org-latex-hyperref-template "\\hypersetup{
      pdfauthor={%a},
      pdftitle={%t},
      pdfkeywords={%k},
      pdfsubject={%d},
      pdfcreator={%c},
      pdflang={%L},
      bookmarks=true,
      unicode=true,
      pdftoolbar=true,
      pdfmenubar=true,
      pdffitwindow=false,
      pdfstartview={FitH},
      pdfnewwindow=true,
      colorlinks=true,
      linkcolor=Maroon,
      citecolor=ForestGreen,
      filecolor=Mulberry,
      urlcolor=MidnightBlue}\n")
  (org-latex-pdf-process
   `(,(concat "PDFLATEX=%latex texi2dvi"
              " --shell-escape --pdf --tidy --verbose --batch %f")))
  (org-latex-prefer-user-labels t)
  (org-latex-src-block-backend 'minted)

  :config
  (add-to-list 'org-latex-packages-alist '("dvipsnames,svgnames,x11names,hyperref" "xcolor"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "geometry"))

  (add-to-list 'org-latex-classes
               '("ctexart"
                 "\\documentclass[11pt]{ctexart} [NO-DEFAULT-PACKAGES] [NO-PACKAGES] [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("IEEEtran"
                 "\\documentclass{IEEEtran} [NO-PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("authesis"
                 "\\documentclass{report}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(use-package ox-beamer
  :config
  (add-to-list 'org-beamer-environments-extra '("only" "o" "\\only%a{" "}"))
  (add-to-list 'org-beamer-environments-extra '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))
  (add-to-list 'org-beamer-environments-extra '("action" "A" "\\action%a{" "}")))

;; https://www.reddit.com/r/orgmode/comments/7u2n0h/tip_for_defining_latex_macros_for_use_in_both/
(use-package org-src
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)))

  (add-to-list 'org-src-lang-modes '("latex-macro" . LaTeX))

  (defvar org-babel-default-header-args:latex-macro
    '((:results . "raw")
      (:exports . "results")))

  (defun me--prefix-all-lines (pre body)
    (with-temp-buffer
      (insert body)
      (string-insert-rectangle (point-min) (point-max) pre)
      (buffer-string)))

  (defun org-babel-execute:latex-macro (body _params)
    (concat
     (me--prefix-all-lines "#+LATEX_HEADER: " body)
     "\n#+HTML_HEAD_EXTRA: <div style=\"display: none\"> \\(\n"
     (me--prefix-all-lines "#+HTML_HEAD_EXTRA: " body)
     "\n#+HTML_HEAD_EXTRA: \\)</div>\n")))

;; Helper functions
;; -----------------------------------------------------------------------------

(use-package org-id)

(defun me--clean-up-heading (heading)
  "Clean up HEADING text."
  (replace-regexp-in-string "[^[:alpha:][:digit:][:space:]_-]" ""
                            (downcase heading)))

(defun me--preprocess-heading (heading sep)
  "Preprocessing on HEADING with SEP as separator."
  (let* ((words (split-string (me--clean-up-heading heading)))
         (words2 (subseq words 0 (min 2 (length words)))))
    (replace-regexp-in-string "\\s-+" sep (mapconcat 'identity words2 " "))))

(defun me--org-id-from-heading (heading &optional level sep uniq)
  "Format HEADING to use as custom ID and return it.

LEVEL is provided, the level of heading is prefixed.  This
function normalize HEADING by replacing spaces with SEP.  SEP is
the separator used to glue different parts.  if UNIQ, append a 5
digit hash ID."
  (let ((sep (or sep "-")))
    (concat (when org-id-prefix (format "%s%s" org-id-prefix sep))
            (when level (format "h%d%s" level sep))
            (me--preprocess-heading heading sep)
            (when uniq (concat sep (substring (org-id-new) 0 5))))))

(defun me--org-id-hash (s &optional length)
  "Generate a unique string hash for S, truncated at LENGTH."
  (let ((length (or length 5))
        (hash (secure-hash 'sha1 s)))
    (substring-no-properties hash nil length)))

(defun me/org-custom-id-get (&optional pom create uniq)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.

If POM is nil, refer to the entry at point.  If the entry does
not have an CUSTOM_ID, the function returns nil.  However, when
CREATE is non nil, create a CUSTOM_ID if none is present already.
PREFIX will be passed through to `org-id-new'.  Append a 5-digit
hash if UNIQ is non nil.  In any case, the CUSTOM_ID of the entry
is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (if create nil
                (org-entry-get nil "CUSTOM_ID"))))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (t
        (setq id (me--org-id-from-heading (org-get-heading t t t t)
                                          (org-current-level)
                                          "-"
                                          uniq))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

(defun me/org-custom-id-get-create (&optional force)
  "Create an ID w/o a suffix for the current entry and return it.

If the entry already has an ID, just return it.  With optional
argument FORCE, force the creation of a new ID."
  (interactive "P")
  (when (derived-mode-p 'org-mode)
    (me/org-custom-id-get (point) force)))

(defun me/org-custom-id-get-create-hash (&optional force)
  "Create an ID w/o a suffix for the current entry and return it.

If the entry already has an ID, just return it.  With optional
argument FORCE, force the creation of a new ID."
  (interactive "P")
  (when (derived-mode-p 'org-mode)
    (me/org-custom-id-get (point) force 'uniq)))

(defun me/org-custom-id-get-create-all (&optional force)
  "Create custom ID for every heading.  Overwrite current if FORCE."
  (interactive "P")
  (when (derived-mode-p 'org-mode)
    (when force
      (org-entry-put (point) "CUSTOM_ID" nil))
    (let ((me--org-custom-id-get-wrapper
           (if force
               (lambda ()
                 (org-entry-put (point) "CUSTOM_ID" nil)
                 (me/org-custom-id-get (point) 'create))
             (lambda () (me/org-custom-id-get (point) 'create)))))
      (org-map-entries me--org-custom-id-get-wrapper))))

(defun me/org-custom-id-get-create-hash-all (&optional force)
  "Create custom ID for every heading.  Overwrite current if FORCE."
  (interactive "P")
  (when (derived-mode-p 'org-mode)
    (when force
      (org-entry-put (point) "CUSTOM_ID" nil))
    (let ((me--org-custom-id-get-wrapper
           (lambda () (me/org-custom-id-get (point) force 'uniq))))
      (org-map-entries me--org-custom-id-get-wrapper))))

(provide 'org-conf)
;;; org-conf.el ends here
