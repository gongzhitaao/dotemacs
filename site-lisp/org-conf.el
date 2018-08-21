;;; org-conf.el --- Orgmode configuration
;; Time-stamp: <2018-08-21 15:25:02 gongzhitaao>

;;; Commentary:
;;

;;; Code:
;; -------------------------------------------------------------------
;; Orgmode
;; -------------------------------------------------------------------

(use-package ox-beamer)
(use-package ox-bibtex)
(use-package ox-extra)
(use-package ox-gfm)

(use-package org
  :init
  (setq org-modules '(org-bbdb org-bibtex org-clock org-gnus
                               org-habit org-table))
  (setq org-export-backends '(ascii beamer html latex md))

  :config
  (setq org-directory me-emacs-data)

  (defun me//init-org ()
    (turn-on-auto-fill)
    (flyspell-mode)

    (defun me--org-show-context-advice (&rest ignore)
      (org-show-context 'default))
    (advice-add 'ispell-highlight-spelling-error :before
                'me--org-show-context-advice)

    (make-local-variable 'ispell-skip-region-alist)
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

  (defun me//init-org-agenda ()
    (define-key org-agenda-mode-map (kbd "F") #'org-gcal-fetch))

  (add-hook 'org-mode-hook #'me//init-org)
  (add-hook 'org-agenda-mode-hook #'me//init-org-agenda)

  (setq org-adapt-indentation nil)
  (setq org-list-description-max-indent 0)
  (setq org-support-shift-select t)

  (add-to-list 'org-structure-template-alist '("A" "#+AUTHOR: ?" ""))
  (add-to-list 'org-structure-template-alist '("a" "#+BEGIN_abstract\n?\n#+END_abstract" ""))
  (add-to-list 'org-structure-template-alist '("B" "#+BIBLIOGRAPHY: ?" ""))
  (add-to-list 'org-structure-template-alist '("C" "#+CAPTION: ?" ""))
  (add-to-list 'org-structure-template-alist '("D" "#+DESCRIPTION: ?" ""))
  (add-to-list 'org-structure-template-alist '("K" "#+KEYWORDS: ?" ""))
  (add-to-list 'org-structure-template-alist '("M" "#+MACRO: ?" ""))
  (add-to-list 'org-structure-template-alist '("N" "#+NAME: ?" ""))
  (add-to-list 'org-structure-template-alist '("O" "#+OPTIONS: ?" ""))
  (add-to-list 'org-structure-template-alist '("p" "#+LATEX_HEADER: ?" ""))
  (add-to-list 'org-structure-template-alist '("R" "#+ATTR_BEAMER: ?" ""))
  (add-to-list 'org-structure-template-alist '("S" "#+SETUPFILE: ?" ""))
  (add-to-list 'org-structure-template-alist '("T" "#+TITLE: ?" ""))
  (add-to-list 'org-structure-template-alist '("X" "#+ATTR_LaTeX: ?" ""))

  (define-key org-mode-map [remap fill-paragraph] #'org-fill-paragraph)
  (define-key org-mode-map (kbd "C-c [") nil)

  ;; Recursive update todo statistics
  (setq org-provide-todo-statistics      t
        org-hierarchical-todo-statistics nil)

  ;; Show events from diary
  (setq org-agenda-include-diary t)

  (setq org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)

  ;; Fontify src blocks
  (setq org-src-fontify-natively t
        org-src-preserve-indentation t)

  (setq org-catch-invisible-edits 'smart)

  ;; Use prefix key as tag selection
  (setq org-use-fast-todo-selection t)

  ;; Bypassing logging if change state with Shift key
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  (setq org-todo-keywords
        '((sequence
           "TODO(t)" "NEXT(n)" "|"
           "DONE(d!)")
          (sequence
           "WAIT(w@/!)" "HOLD(h@/!)" "|"
           "KILL(k@)")))

  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red"          :weight bold)
          ("NEXT" :foreground "cyan"         :weight bold)
          ("DONE" :foreground "green"        :weight bold)
          ("WAIT" :foreground "yellow"       :weight bold)
          ("HOLD" :foreground "magenta"      :weight bold)
          ("KILL" :foreground "forest green" :weight bold)))

  (set-face-foreground 'org-level-1 (me//colir-blend "LightSalmon" "gray90" 0.3))
  (set-face-foreground 'org-level-2 (me//colir-blend "yellow" "gray90" 0.2))
  (set-face-foreground 'org-level-3 (me//colir-blend "LightGreen" "gray90" 0.3))
  (set-face-foreground 'org-level-4 (me//colir-blend "LightSkyBlue" "gray90" 0.3))
  (set-face-foreground 'org-level-5 (me//colir-blend "LightSkyBlue" "gray90" 0.1))
  (set-face-foreground 'org-level-6 (me//colir-blend "LightGreen" "gray90" 0.1))
  (set-face-foreground 'org-level-7 (me//colir-blend "yellow" "gray90" 0.1))
  (set-face-foreground 'org-level-8 (me//colir-blend "LightSalmon" "gray90" 0.1))

  (set-face-foreground 'org-special-keyword (me//colir-blend "SteelBlue" "gray60" 0.2))
  (set-face-foreground 'org-date (me//colir-blend "goldenrod" "gray70" 0.3))
  (set-face-foreground 'org-link (me//colir-blend "SteelBlue" "gray90" 0.2))
  (set-face-foreground 'org-footnote (me//colir-blend "SteelBlue" "gray90" 0.5))

  ;; Files to be included in Agenda view.
  (setq org-agenda-files
        (expand-file-name "orgfile" org-directory))

  (setq org-agenda-dim-blocked-tasks            t
        org-agenda-compact-blocks               nil
        org-agenda-repeating-timestamp-show-all t
        org-agenda-show-all-dates               t)

  (setq org-time-stamp-custom-formats
        '("<%m/%d/%y %a>" . "<%Y-%m-%d %a %R %z>"))

  (setq org-agenda-prefix-format
        '((agenda   . " %i %-12:c%?-12t% s")
          (timeline . "  % s")
          (todo     . " %i %-12:T")
          (tags     . " %i %-12:T")
          (search   . " %i %-12:T")))

  (defun me//org-skip-subtree-if-habit ()
    "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (string= (org-entry-get nil "STYLE") "habit")
          subtree-end
        nil)))

  (defun me//org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))

(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header
                  "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-ndays 1)))
          (alltodo ""
                   ((org-agenda-skip-function
                     '(or (me//org-skip-subtree-if-habit)
                          (me//org-skip-subtree-if-priority ?A)
                          (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header
                     "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks nil)))))

  (setq org-agenda-tags-column         -100
        org-agenda-start-with-log-mode t)
  (setq org-habit-graph-column         50
        org-habit-preceding-days       28
        org-habit-following-days       1)

  (setq org-clock-history-length 32
        org-clock-in-resume t)
  (setq org-log-into-drawer   t
        org-clock-into-drawer t)

  (setq org-clock-persist t)
  (org-clock-persistence-insinuate)

  (setq org-use-fast-tag-selection 'auto)

  (setq org-capture-templates
        `(("m" "Save mail link" entry
           (file "todo.org")
           (file "capture/mail.org")
           :empty-lines 1
           :jump-to-captured t)

          ("r" "Insert related work reference")
          ("ra" "Adversarial attack/defense" checkitem
           (file+headline "notes/bibliography/related/adversarial.org" "Recent Work"))
          ("rc" "Compression and Quantization" checkitem
           (file+headline "notes/bibliography/related/compression.org" "Recent Work"))
          ("re" "Evolution" checkitem
           (file+headline "notes/bibliography/related/evolution.org" "Recent Work"))
          ("rl" "Reinforcement Learning" checkitem
           (file+headline "notes/bibliography/related/rl.org" "Recent Work"))
          ("rn" "Natural Language Processing" checkitem
           (file+headline "notes/bibliography/related/nlp.org" "Recent Work"))
          ("rr" "Randomness" checkitem
           (file+headline "notes/bibliography/related/random.org" "Recent Work"))
          ("rt" "Theory" checkitem
           (file+headline "notes/bibliography/related/theory.org" "Recent Work"))
          ("ru" "Miscellaneous" checkitem
           (file+headline "notes/bibliography/related/misc.org" "Recent Work"))

          ("s" "Saved for later" item
           (file+headline "readlist.org" "Reading List"))

          ("t" "New TODO" entry
           (file "todo.org")
           (file "capture/todo.org")
           :empty-lines 1
           :jump-to-captured t)

          ("l" "New daily/weekly plan/log" plain
           (file+olp+datetree "time-machine.org")
           "%?"
           :empty-lines 1
           :jump-to-captured t
           :tree-type week)))

  (setq org-latex-prefer-user-labels t)

  (setq org-latex-pdf-process
        (quote ("PDFLATEX=%latex texi2dvi --shell-escape --pdf --clean --verbose --batch %f")))

  (setq org-latex-listings 'minted)
  ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
  ;; (add-to-list 'org-latex-packages-alist '("activate={true,nocompatibility},final,tracking=true,kerning=true,spacing=basictext,factor=1100,stretch=10,shrink=10" "microtype"))
  ;; (add-to-list 'org-latex-packages-alist '("" "geometry"))

  (add-to-list 'org-latex-classes
               '("ctexart"
                 "\\documentclass[11pt]{ctexart} [NO-DEFAULT-PACKAGES] [NO-PACKAGES] [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (ox-extras-activate '(ignore-headlines))
  (add-to-list 'org-latex-classes
               '("IEEEtran"
                 "\\documentclass{IEEEtran} [NO-PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (setq org-latex-hyperref-template "\\hypersetup{
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

  (add-to-list 'org-beamer-environments-extra
               '("only" "O" "\\only%a{" "}"))
  (add-to-list 'org-beamer-environments-extra
               '("action" "A" "\\action%a{" "}"))

  (setq org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil)

  (setq org-html-allow-name-attribute-in-anchors t)

  (setq org-html-htmlize-output-type 'css)

  ;; Postamble.
  (setq org-html-postamble t
        org-html-postamble-format
        '(("en" "<a class=\"author\"
           href=\"http://gongzhitaao.org\">%a</a> / <span
           class=\"date\">%T</span><span class=\"creator\">%c</span>")))

  (load-file (expand-file-name "my-org-misc.el" org-directory)))

;; -----------------------------------------------------------------------------
;; Helper functions
;; -----------------------------------------------------------------------------

(require 'org-id)

(defun me//clean-up-heading (heading)
  "Clean up heading text."
  (replace-regexp-in-string "[^[:alpha:][:digit:][:space:]_-]" ""
                            (downcase heading)))

(defun me//org-id-from-heading (heading &optional level sep)
  "Format HEADING to use as custom ID and return it.

This function normalize HEADING by replacing spaces with SEP.  if
LEVEL is provided, the level of heading is prefixed.  SEP is the
separator used to glue different parts."
  (let ((sep (or sep "-")))
    (concat (when org-id-prefix (format "%s%s" org-id-prefix sep))
            (when level (format "h%d%s" level sep))
            (replace-regexp-in-string "\\s-+" sep
                                      (me//clean-up-heading heading))
            (concat sep (me//org-id-hash (concat (buffer-name) heading))))))

(defun me//org-id-hash (s &optional length)
  "Generate a unique string hash."
  (let ((length (or length 5))
        (hash (secure-hash 'sha1 s)))
    (substring-no-properties hash nil length)))

(defun me/org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.

If POM is nil, refer to the entry at point.  If the entry does
not have an CUSTOM_ID, the function returns nil.  However, when
CREATE is non nil, create a CUSTOM_ID if none is present already.
PREFIX will be passed through to `org-id-new'.  In any case, the
CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (me//org-id-from-heading (org-get-heading t t t t)
                                          (org-current-level)
                                          "-"))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

(defun me/org-custom-id-get-create (&optional force)
  "Create an ID w/o a suffix for the current entry and return it.

If the entry already has an ID, just return it.  With optional
argument FORCE, force the creation of a new ID."
  (interactive "P")
  (when (derived-mode-p 'org-mode)
    (when force
      (org-entry-put (point) "CUSTOM_ID" nil))
    (me/org-custom-id-get (point) 'create)))

(defun me/org-custom-id-get-create-all (&optional force)
  "Create custom ID for every heading."
  (interactive "P")
  (when (derived-mode-p 'org-mode)
    (when force
      (org-entry-put (point) "CUSTOM_ID" nil))
    (let ((me//org-custom-id-get-wrapper
           (if force
               (lambda ()
                 (org-entry-put (point) "CUSTOM_ID" nil)
                 (me/org-custom-id-get (point) 'create))
             (lambda () (me/org-custom-id-get (point) 'create)))))
      (org-map-entries me//org-custom-id-get-wrapper))))

(provide 'org-conf)

;;; org-conf.el ends here
