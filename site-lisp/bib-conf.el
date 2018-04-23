;;; bib-conf.el --- Bibliography manager
;; Time-stamp: <2018-04-22 15:59:18 gongzhitaao>

;;; Commentary:
;; A full-fledged bibliography manager, depends upon pdftools, org-ref,
;; helm-bibtex, bibtex-completion, orgmode.

;;; Code:

;; -------------------------------------------------------------------
;; BibTeX
;; -------------------------------------------------------------------

(defvar me-bib (expand-file-name ".local/data/bibliography" me-home)
  "My bibliography collection path.")
(defvar me-bib-files
  `(,(expand-file-name "nn.bib" me-bib))
  "My bibliography files.")
(defvar me-bib-pdfs
  `(,(expand-file-name "nn-pdf" me-bib))
  "Paths containing my PDFs of the bibliography.")
(defvar me-bib-notes
  (expand-file-name "notes" me-bib)
  "Path to store my notes on each papers.")

(use-package helm-bibtex
  :bind ("C-c b" . helm-bibtex))

(use-package bibtex
  :config
  (defun me//init-bibtex ()
    (local-set-key [remap fill-paragraph] #'bibtex-fill-entry)
    (local-set-key [remap bibtex-clean-entry] #'org-ref-clean-bibtex-entry)
    (local-set-key (kbd "C-c C-v") #'bibtex-validate)
    (setq fill-column 140))
  (add-hook 'bibtex-mode-hook #'me//init-bibtex)

  (setq bibtex-dialect 'biblatex)
  (setq bibtex-align-at-equal-sign t)
  (setq bibtex-text-indentation 20)

  (add-to-list 'bibtex-entry-format 'unify-case)
  (add-to-list 'bibtex-entry-format 'sort-fields)
  (add-to-list 'bibtex-entry-format 'whitespace)
  (add-to-list 'bibtex-entry-format 'last-comma)

  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "_"
        bibtex-autokey-titlewords 1
        bibtex-autokey-titlewords-stretch 0
        bibtex-autokey-titleword-length nil)
  (setq bibtex-maintain-sorted-entries t)

  (setq bibtex-completion-bibliography me-bib-files
        bibtex-completion-library-path me-bib-pdfs
        bibtex-completion-notes-path me-bib-notes)
  (setq bibtex-completion-notes-extension ".org")
  (setq bibtex-completion-pdf-symbol "⚐"
        bibtex-completion-notes-symbol "✔"))

;; -------------------------------------------------------------------
;; reftex
;; -------------------------------------------------------------------

(use-package reftex
  :diminish reftex-mode
  :config
  (add-hook 'tex-mode-hook #'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t
        reftex-ref-style-default-list '("Cleveref"
                                        "Hyperref"
                                        "Fancyref")
        reftex-default-bibliography me-bib-files))

;; -------------------------------------------------------------------
;; org-ref
;; -------------------------------------------------------------------

(use-package org-ref
  :init
  (setq org-ref-default-bibliography me-bib-files
        org-ref-pdf-directory me-bib-pdfs
        org-ref-notes-directory me-bib-notes)

  (setq org-ref-ref-color "goldenrod")
  (setq org-ref-cite-color "dark sea green")
  (setq org-ref-show-citation-on-enter nil)

  :after (bibtex)

  :config
  (defun me//org-ref-notes-function (thekey)
    (bibtex-completion-edit-notes
     (list (car (org-ref-get-bibtex-key-and-file thekey)))))
  (setq org-ref-notes-function #'me//org-ref-notes-function)
  (add-hook 'org-ref-clean-bibtex-entry-hook #'org-ref-downcase-bibtex-entry)

  (define-key org-ref-cite-keymap (kbd "M-<right>") #'org-ref-next-key)
  (define-key org-ref-cite-keymap (kbd "M-<left>") #'org-ref-previous-key)
  (define-key org-ref-cite-keymap (kbd "C-<left>") nil)
  (define-key org-ref-cite-keymap (kbd "C-<right>") nil)

  (setq bibtex-completion-display-formats
        '((article       . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${journal:20}  ${keywords:40}")
          (inbook        . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${chapter:20}  ${keywords:40}")
          (incollection  . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${booktitle:20}  ${keywords:40}")
          (inproceedings . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${booktitle:20}  ${keywords:40}")
          (t             . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${keywords:62}")))
  (setq bibtex-completion-additional-search-fields '(keywords journal booktitle)))

;; -----------------------------------------------------------------------------
;; helper functions
;; -----------------------------------------------------------------------------

(defun me//getkey-orgref ()
  "Get the year part of orgref citation.

My bib key is (lastname)(YYYY)-(title), where title is the first
non-trivial word in title, This function will
return (YYYY)(lastname)(title).  Note the parenthesis is only for
readability, no parenthesis actually exist."
  (save-excursion
    (if (re-search-forward org-ref-cite-re nil t)
        (let* ((bibkey (match-string 0))
               (YYYY-re "\\([0-9]\\{4\\}\\)"))
          (string-match YYYY-re bibkey)
          (concat (match-string 0 bibkey)
                  (replace-regexp-in-string YYYY-re "" bibkey)))
      "")))

(defun me/org-sort-orgref-citation-list-by-year
    (&optional with-case sorting-type)
  "Sort the list of citations by year.

The list looks like:
- [X] cite:someone2017 dummy
- [ ] cite:others2013 dummy
- [ ] cite:hello2018 dummy

I want to sort the list first by year (newest first), then
alphabetically (in ascending or descending order)."
  (interactive)
  (when (and (derived-mode-p 'org-mode) (org-at-item-p))
    (org-sort-list with-case ?F #'me//getkey-orgref)))

;; The following three functions jump among PDF, bibtex entry and note.  For
;; instance, me/org-ref-open-pdf opens the PDF file when your cursor is at the
;; bibtex entry or the note that is associated with this bibtex entry.

(defun me/org-ref-open-pdf ()
  "Open the associated PDF."
  (interactive)
  (let* ((key (cond
               ((derived-mode-p 'org-mode)
                (condition-case nil
                    (save-excursion
                      (org-ref-get-bibtex-key-under-cursor))
                  (error (file-name-base (buffer-file-name)))))
               ((derived-mode-p 'bibtex-mode)
                (save-excursion
                  (bibtex-beginning-of-entry)
                  (reftex-get-bib-field "=key=" (bibtex-parse-entry t))))
               (t nil)))
         (pdf-file (funcall org-ref-get-pdf-filename-function key)))
    (if (file-exists-p pdf-file)
        (org-open-file pdf-file)
      (message "No PDF found with name %s" pdf-file))))

(defun me/org-ref-open-entry ()
  "Open bibtex file to key with which the note associated."
  (interactive)
  (let* ((key (cond
               ((derived-mode-p 'org-mode)
                (condition-case nil
                    (save-excursion
                      (org-ref-get-bibtex-key-under-cursor))
                  (error (file-name-base (buffer-file-name)))))
               ((derived-mode-p 'pdf-view-mode)
                (file-name-base (buffer-file-name)))
               (t nil)))
         (bibfile ""))
    (if key
        (progn
          (setq bibfile (cdr (org-ref-get-bibtex-key-and-file key)))
          (find-file bibfile)
          (bibtex-search-entry key))
      (message "Non existing key %s" key))))

(defun me/org-ref-open-note ()
  "Open the associated note file."
  (interactive)
  (let* ((key (cond
               ((derived-mode-p 'org-mode)
                (ignore-errors (org-ref-get-bibtex-key-under-cursor)))
               ((derived-mode-p 'bibtex-mode)
                (save-excursion
                  (bibtex-beginning-of-entry)
                  (reftex-get-bib-field "=key=" (bibtex-parse-entry t))))
               ((derived-mode-p 'pdf-view-mode)
                (file-name-base (buffer-file-name)))
               (t nil)))
         (pdf-file (funcall org-ref-get-pdf-filename-function key)))
    (if (file-exists-p pdf-file)
        (org-ref-open-notes-at-point key)
      (message "Not open note for non-existing PDF %s" key))))

(provide 'bib-conf)

;;; bib-conf.el ends here