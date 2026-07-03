;;; bib-conf.el --- Bibliography and PDF reading config  -*- lexical-binding: t; -*-

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

;; Bibliography and PDF reading workflow: bibtex, bibtex-completion,
;; citar (+ embark, org-roam, org-cite), reftex and pdf-tools, plus the
;; helpers that jump between a bib entry, its PDF and its note.

;;; Code:

;; ** Bibliography

(defvar me-bib (file-name-as-directory
                (file-name-concat me-emacs-data-dir "bibliography"))
  "My bibliography collection path.")
(defvar me-bib-files (list (file-name-concat me-bib "refdb.bib"))
  "My bibliography files.")
(defvar me-bib-pdfs (list (file-name-as-directory (file-name-concat me-bib "pdf")))
  "Paths containing my PDFs of the bibliography.")
(defvar me-bib-notes (file-name-as-directory org-directory)
  "Path to store my notes on each papers.")

(defun me/bibtex-find-text-begin ()
  "Go to the beginning of a field entry."
  (interactive)
  (bibtex-find-text t))

(defun me--bibtex-set-field (field value &optional nodelim)
  "Set FIELD to VALUE in the current BibTeX entry, creating it if absent.
Optional NODELIM is passed to `bibtex-make-field'.  Ported from
org-ref's `bibtex-set-field', which is no longer loaded."
  (bibtex-beginning-of-entry)
  (let ((found (bibtex-search-forward-field field t)))
    (if found
        (progn
          (goto-char (cadr found))
          (when value
            (bibtex-kill-field)
            (bibtex-make-field field nil nil nodelim)
            (backward-char)
            (insert value)))
      (bibtex-beginning-of-entry)
      (forward-line)
      (beginning-of-line)
      (bibtex-next-field nil)
      (forward-char)
      (bibtex-make-field field nil nil nodelim)
      (backward-char)
      (insert value))))

(defun me--bibtex-add-timestamp ()
  "Add an ISO 8601 timestamp field to the current bibtex entry."
  (interactive)
  (me--bibtex-set-field "timestamp" (format-time-string "%FT%T%z")))

(defvar me-bibtex-lowercase-words
  '("a" "an" "on" "and" "for" "the" "of" "in")
  "Words kept lowercase when title-casing a BibTeX title.")

(defvar me-bibtex-title-case-types '(("article" . ("title")))
  "Alist of (ENTRY-TYPE . FIELDS) whose FIELDS get title-cased.")

(defun me--bibtex-title-case ()
  "Title-case configured fields of the current BibTeX entry.
Ported from `org-ref-title-case', driven by
`me-bibtex-title-case-types' and `me-bibtex-lowercase-words'."
  (save-restriction
    (bibtex-narrow-to-entry)
    (bibtex-beginning-of-entry)
    (let* ((entry-type (downcase (cdr (assoc "=type=" (bibtex-parse-entry)))))
           (fields (cdr (assoc entry-type me-bibtex-title-case-types)))
           title words start)
      (dolist (field fields)
        (when (bibtex-autokey-get-field field)
          (setq title (bibtex-autokey-get-field field)
                words (split-string title)
                start 0)
          (setq words
                (mapcar
                 (lambda (word)
                   (cond
                    ;; Abbreviations (more than one dot): leave as-is.
                    ((> (seq-count (lambda (c) (eq c ?.)) word) 1) word)
                    ;; LaTeX or otherwise protected words: leave as-is.
                    ((string-match "\\$\\|{\\|}\\|(\\|)\\|\\\\" word) word)
                    ;; Minor words stay lowercase.
                    ((member (downcase word) me-bibtex-lowercase-words)
                     (downcase word))
                    ;; Quoted words.
                    ((string-prefix-p "\"" word)
                     (concat "\"" (capitalize (substring word 1))))
                    (t (capitalize word))))
                 words))
          ;; The first word is always capitalized.
          (when (member (car words) me-bibtex-lowercase-words)
            (setcar words (capitalize (car words))))
          (setq title (mapconcat #'identity words " "))
          ;; Capitalize the letter following a dash.
          (while (string-match "[a-zA-Z]-\\([a-z]\\)" title start)
            (setq title (concat (substring title 0 (match-beginning 1))
                                (upcase (match-string 1 title))
                                (substring title (match-end 1)))
                  start (match-end 1)))
          (me--bibtex-set-field field title))))))

(defun me/bibtex-clean-entry (&optional _new-key)
  "Timestamp, title-case, then clean and re-key the current entry.
Replacement for `org-ref-clean-bibtex-entry' built on the stock
`bibtex-clean-entry'.  The key is always regenerated with
`bibtex-generate-autokey' from the `bibtex-autokey-*' settings;
field formatting, case unification, page dashes and field sorting
come from `bibtex-entry-format' (set to t)."
  (interactive "P")
  (save-excursion
    (me--bibtex-add-timestamp)
    (me--bibtex-title-case)
    (bibtex-clean-entry t)))

(use-package bibtex
  :bind (:map bibtex-mode-map
              ([remap fill-paragraph]     . bibtex-fill-entry)
              ([remap bibtex-clean-entry] . me/bibtex-clean-entry)
              ("C-c C-v"                  . bibtex-validate)
              ("<backtab>"                . me/bibtex-find-text-begin)
              ("M-<down>"                 . bibtex-end-of-entry)
              ("M-<up>"                   . bibtex-beginning-of-entry))

  :custom
  ( bibtex-align-at-equal-sign t)
  ( bibtex-autokey-name-year-separator "")
  ( bibtex-autokey-titleword-length nil)
  ( bibtex-autokey-titleword-separator "_")
  ( bibtex-autokey-titlewords 1)
  ( bibtex-autokey-titlewords-stretch 0)
  ( bibtex-autokey-year-length 4)
  ( bibtex-autokey-year-title-separator "-")
  ( bibtex-dialect 'biblatex)
  ( bibtex-entry-format t)
  ( bibtex-maintain-sorted-entries t)
  ( bibtex-text-indentation 20)

  :config
  (defun me--init-bibtex ()
    "Init bibtex mode."
    (setq-local fill-column 140)
    (setq-local writeroom-width 150)
    (setq bibtex-maintain-sorted-entries
          '(me--bibtex-entry-index me--bibtex-lessp)))
  (add-hook 'bibtex-mode-hook #'me--init-bibtex))

(use-package bibtex-completion
  :custom
  ( bibtex-completion-bibliography me-bib-files)
  ( bibtex-completion-library-path me-bib-pdfs)
  ( bibtex-completion-notes-extension ".org")
  ( bibtex-completion-notes-path me-bib-notes)
  ( bibtex-completion-notes-symbol "N")
  ( bibtex-completion-pdf-symbol "P")
  ( bibtex-completion-additional-search-fields '(keywords journal booktitle))
  ( bibtex-completion-display-formats
    `((article       . "${author:20}  ${title:*}  ${year:4} ${keywords:40} ${journal:15} ${=has-pdf=:1} ${=has-note=:1}")
      (inbook        . "${author:20}  ${title:*}  ${year:4} ${keywords:40} ${chapter:15} ${=has-pdf=:1} ${=has-note=:1}")
      (incollection  . "${author:20}  ${title:*}  ${year:4} ${keywords:40} ${booktitle:15} ${=has-pdf=:1} ${=has-note=:1}")
      (inproceedings . "${author:20}  ${title:*}  ${year:4} ${keywords:40} ${booktitle:15} ${=has-pdf=:1} ${=has-note=:1}")
      (t             . ,(format "${author:20}  ${title:*}  ${year:4} ${keywords:40}  %s  ${=has-pdf=:1} ${=has-note=:1}" (make-string 13 ? ))))))

;; Use citar instead of helm-bibtex

(use-package citar
  :bind ("C-c b" . citar-open)
  :custom
  ( citar-bibliography me-bib-files)
  ( citar-library-paths me-bib-pdfs)

  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar-embark
  :after (citar embark)
  :delight
  :config
  (citar-embark-mode))

(use-package oc                         ;org-cite
  :custom
  ( org-cite-global-bibliography me-bib-files)
  ( org-cite-insert-processor 'citar)
  ( org-cite-follow-processor 'citar)
  ( org-cite-activate-processor 'basic))

(use-package citar-org-roam
  :delight
  :after (citar org-roam)

  :custom
  ( citar-org-roam-note-title-template "${year}:${title}")

  :config (citar-org-roam-mode))

(eval-when-compile
  (defun me/cleanup-bibtex-file (arg)
    "Cleanup entries, start from the begnning if ARG."
    (interactive "P")
    (bibtex-progress-message "Cleanup bibtex buffer...")
    (save-excursion
      (if arg
          (bibtex-beginning-first-field)
        (bibtex-beginning-of-entry))
      (save-restriction
        (narrow-to-region (point) (point-max))
        (bibtex-map-entries (lambda (_key _start _end)
                              (bibtex-progress-message)
                              (me/bibtex-clean-entry)))))
    (bibtex-progress-message 'done)))

(defun me--random-time ()
  "Generate random timestamp from epoch and now."
  (random (time-convert (current-time) 'integer)))

(defun me--bibtex-entry-index ()
  "Return index of BibTeX entry head at or past position of point.

The index is a list (KEY TIMESTAMP) that is used for sorting the
entries of the BibTeX buffer.  Return nil if no entry found.
Move point to the end of the head of the entry found."
  (list (bibtex-key-in-head)
        (let ((ts (bibtex-autokey-get-field "timestamp")))
          (if (string-empty-p ts)
              (me--random-time)
            (time-convert (date-to-time ts) 'integer)))))

(defun me--bibtex-lessp (index1 index2)
  "Predicate for sorting BibTeX entries with indices INDEX1 and INDEX2.

Each index is a list (KEY TIMESTAMP)."
  (let ((t1 (nth 1 index1))
        (t2 (nth 1 index2)))
    (or (and (= t1 t2)
             (string< (car index1) (car index2)))
        (< t1 t2))))

;;; *** reftex

(use-package reftex
  :delight
  :custom
  ( reftex-plug-into-AUCTeX t)
  ( reftex-ref-style-default-list '("Cleveref" "Hyperref" "Fancyref"))
  ( reftex-default-bibliography me-bib-files)
  :hook (tex-mode . reftex-mode))

;;; *** PDF

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)

  ;; pdf-tools 1.3.0 calls `pdf-roll-page-overlay' unconditionally in
  ;; `pdf-links-read-link-action' (bound to F), but never loads pdf-roll.
  ;; Require it so the symbol isn't void even without continuous scroll.
  (require 'pdf-roll)

  (defun me/pdf-set-last-viewed-bookmark ()
    (interactive)
    (when (eq major-mode 'pdf-view-mode)
      (let ((bmk (me--pdf-get-bookmark-name)))
        (bookmark-set (me--pdf-get-bookmark-name))
        (bookmark-bmenu-save)
        (message "Bookmark %s set" bmk))))

  (defun me--pdf-jump-last-viewed-bookmark ()
    (let ((bmk (me--pdf-get-bookmark-name)))
      (when (me--pdf-has-last-viewed-bookmark bmk)
        (bookmark-jump bmk))))

  (defun me--pdf-has-last-viewed-bookmark (bmk)
    (assoc bmk bookmark-alist))

  (defun me--pdf-get-bookmark-name ()
    (concat " " (file-name-sans-extension
                  (file-name-nondirectory (buffer-file-name)))))

  (add-hook 'pdf-view-mode-hook 'me--pdf-jump-last-viewed-bookmark))

;; Helper functions
;; -----------------------------------------------------------------------------

(defun me/pdf-view-next-few-lines ()
  "Scroll down few lines."
  (interactive)
  (pdf-view-next-line-or-next-page 10))

(defun me/pdf-view-prev-few-lines ()
  "Score up few lines."
  (interactive)
  (pdf-view-previous-line-or-previous-page 10))

(defun me/image-forward-hscroll-few-lines ()
  "Scroll to left a few lines."
  (interactive)
  (image-forward-hscroll 7))

(defun me/image-backward-hscroll-few-lines ()
  "Scroll to left a few lines."
  (interactive)
  (image-backward-hscroll 7))

;; copied directly from view-window-size
(defun me--window-size ()
  "Return the height of the current window, excluding the mode line.
Using `window-line-height' accounts for variable-height fonts."
  (let ((h (window-line-height -1)))
    (if h
        (1+ (nth 1 h))
      ;; This should not happen, just in case `window-line-height' returns
      ;; nil, fall back on `window-height'.
      (1- (window-height)))))

(defun me--get-cite-key ()
  "Get citation key if possible."
  (cond
   ((derived-mode-p 'org-mode)
    ;; Key under cursor in an org-cite citation.  Otherwise the file name
    ;; (a note file is named after its key by default).
    (or (citar-key-at-point)
        (file-name-base (buffer-file-name))))
   ((derived-mode-p 'bibtex-mode)
    (bibtex-completion-key-at-point))
   ((derived-mode-p 'pdf-view-mode)
    (file-name-base (buffer-file-name)))
   (t nil)))

(defun me/org-ref-open-entry ()
  "Open the bibtex entry for the key at point / of the current file."
  (interactive)
  (let ((key (me--get-cite-key)))
    (if key
        (citar-open-entry key)
      (message "No citation key found"))))

(defun me/org-ref-open-note ()
  "Open the note associated with the key at point / current file."
  (interactive)
  (let ((key (me--get-cite-key)))
    (if key
        (citar-open-notes (list key))
      (message "No citation key found"))))

(defun me/org-ref-open-pdf (&optional arg)
  "Open the PDF associated with the key at point / current file.
If ARG, open with external program.  Otherwise open in Emacs."
  (interactive "P")
  (let* ((key (me--get-cite-key))
         (pdf-file (car (and key (gethash key (citar-get-files key))))))
    (cond
     ((and pdf-file (file-exists-p pdf-file))
      (org-open-file pdf-file (not arg)))
     ((derived-mode-p 'pdf-view-mode)
      (message "Already opened"))
     (t (message "No PDF found for %s" key)))))

(use-package pdf-view
  :bind (:map pdf-view-mode-map
              ("<delete>"   . pdf-view-scroll-up-or-next-page)
              ("<down>"     . me/pdf-view-next-few-lines)
              ("<up>"       . me/pdf-view-prev-few-lines)
              ("<left>"     . me/image-forward-hscroll-few-lines)
              ("<right>"    . me/image-backward-hscroll-few-lines)
              ("<PageUp>"   . pdf-view-scroll-down-or-previous-page)
              ("<PageDown>" . pdf-view-scroll-up-or-next-page)
              ("b"          . me/pdf-set-last-viewed-bookmark)
              ("c"          . me/org-ref-open-entry)
              ;; ("d"       . pdf-view-scroll-up-or-next-page)
              ;; ("e"       . pdf-view-scroll-down-or-previous-page)
              ("d"          . me/pdf-view-next-few-lines)
              ("e"          . me/pdf-view-prev-few-lines)
              ("g"          . pdf-view-goto-page)
              ;; ("j"       . pdf-view-scroll-up-or-next-page)
              ;; ("k"       . pdf-view-scroll-down-or-previous-page)
              ("j"          . me/pdf-view-next-few-lines)
              ("k"          . me/pdf-view-prev-few-lines)
              ("n"          . me/org-ref-open-note)
              ("Q"          . kill-current-buffer)
              ("z"          . delete-other-windows)
              ("C-<left>"   . pdf-view-previous-page-command)
              ("C-<right>"  . pdf-view-next-page-command)))

;; helper functions
;; -----------------------------------------------------------------------------

(defun me--getkey-orgref ()
  "Get the year part of an org-cite citation on the current line.

My bib key is (lastname)(YYYY)-(title), where title is the first
non-trivial word in title, This function will
return (YYYY)(lastname)(title).  Note the parenthesis is only for
readability, no parenthesis actually exist."
  (save-excursion
    (if (re-search-forward "@\\([[:alnum:]_:.-]+\\)" (line-end-position) t)
        (let ((bibkey (match-string 1))
              (YYYY-re "\\([0-9]\\{4\\}\\)"))
          (if (string-match YYYY-re bibkey)
              (concat (match-string 0 bibkey)
                      (replace-regexp-in-string YYYY-re "" bibkey))
            bibkey))
      "")))

(defun me/org-sort-orgref-citation-list-by-year
    (&optional with-case sorting-type)
  "Sort the list of citations by year.

Case sensitive if WITH-CASE.  SORTING-TYPE is not used here.

The list looks like:
- [X] [cite:@someone2017] dummy
- [ ] [cite:@others2013] dummy
- [ ] [cite:@hello2018] dummy

I want to sort the list first by year (newest first), then
alphabetically (in ascending or descending order)."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (cond ((org-at-item-p)
           (org-sort-list with-case ?F #'me--getkey-orgref))
          ((org-at-heading-p)
           (org-sort-entries with-case ?F #'me--getkey-orgref))
          (t nil))))

;; The following three functions jump among PDF, bibtex entry and note.  For
;; instance, me/org-ref-open-pdf opens the PDF file when your cursor is at the
;; bibtex entry or the note that is associated with this bibtex entry.

;; (define-pdf-cache-function pagelabels)
(defun me--pdf-view-page-number ()
  "For telephone-mode line."
  (interactive)
  (if (called-interactively-p 'interactive)
      (message "[pg %s/%s/%s]"
               (nth (1- (pdf-view-current-page))
                    (pdf-cache-pagelabels))
               (number-to-string (pdf-view-current-page))
               (number-to-string (pdf-cache-number-of-pages)))
    (format "[%s/%s]"
            (number-to-string (pdf-view-current-page))
            (number-to-string (pdf-cache-number-of-pages)))))


(provide 'bib-conf)
;;; bib-conf.el ends here
