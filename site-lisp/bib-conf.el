;;; bib-conf.el --- Bibliography manager
;; Time-stamp: <2018-08-25 15:51:42 gongzhitaao>

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

(defun me/bibtex-find-text-begin ()
  "Go to the beginning of a field entry."
  (interactive)
  (bibtex-find-text t))

(use-package bibtex
  :config
  (defun me//init-bibtex ()
    (setq fill-column 140))
  (add-hook 'bibtex-mode-hook #'me//init-bibtex)

  (define-key bibtex-mode-map [remap fill-paragraph] #'bibtex-fill-entry)
  (define-key bibtex-mode-map [remap bibtex-clean-entry] #'org-ref-clean-bibtex-entry)
  (define-key bibtex-mode-map (kbd "C-c C-v") #'bibtex-validate)
  (define-key bibtex-mode-map (kbd "<backtab>") #'me/bibtex-find-text-begin)
  (define-key bibtex-mode-map (kbd "M-<down>") #'bibtex-end-of-entry)
  (define-key bibtex-mode-map (kbd "M-<up>") #'bibtex-beginning-of-entry)

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

  (setq org-ref-ref-color (me//colir-blend "goldenrod" "grey90" 0.4))
  (setq org-ref-cite-color (me//colir-blend "dark sea green" "grey90" 0.4))
  (setq org-ref-show-citation-on-enter nil)

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
        `((article       . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${journal:20}  ${keywords:40}")
          (inbook        . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${chapter:20}  ${keywords:40}")
          (incollection  . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${booktitle:20}  ${keywords:40}")
          (inproceedings . "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${booktitle:20}  ${keywords:40}")
          (t             . ,(format "${author:36}  ${title:*}  ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:3}  %s  ${keywords:40}" (make-string 20 ? )))))
  (setq bibtex-completion-additional-search-fields '(keywords journal booktitle)))

;; -----------------------------------------------------------------------------
;; PDF tools
;; -----------------------------------------------------------------------------

(pdf-tools-install)

(defun me/pdf-view-next-few-lines ()
  "Scroll down few lines."
  (interactive)
  (pdf-view-next-line-or-next-page 10))

(defun me/pdf-view-prev-few-lines ()
  "Score up few lines."
  (interactive)
  (pdf-view-previous-line-or-previous-page 10))

;; copied directly from view-window-size
(defun me//window-size ()
   "Return the height of the current window, excluding the mode line.
Using `window-line-height' accounts for variable-height fonts."
  (let ((h (window-line-height -1)))
    (if h
        (1+ (nth 1 h))
      ;; This should not happen, just in case `window-line-height' returns
      ;; nil, fall back on `window-height'.
      (1- (window-height)))))

(defun me/pdf-view-scroll-half-forward ()
  "Score down half page."
  (interactive)
  (pdf-view-next-line-or-next-page (/ (me//window-size) 2)))

(defun me/pdf-view-scroll-half-backward ()
  "Score up half page."
  (interactive)
  (pdf-view-previous-line-or-previous-page (/ (me//window-size) 2)))

(bind-key (kbd "<delete>") #'pdf-view-scroll-up-or-next-page pdf-view-mode-map)
(bind-key (kbd "<down>") #'me/pdf-view-next-few-lines pdf-view-mode-map)
(bind-key (kbd "<up>") #'me/pdf-view-prev-few-lines pdf-view-mode-map)
(bind-key (kbd "b") #'helm-mini pdf-view-mode-map)
(bind-key (kbd "c") #'me/org-ref-open-entry pdf-view-mode-map)
(bind-key (kbd "d") #'me/pdf-view-scroll-half-forward pdf-view-mode-map)
(bind-key (kbd "e") #'me/pdf-view-scroll-half-backward pdf-view-mode-map)
(bind-key (kbd "j") #'me/pdf-view-scroll-half-forward pdf-view-mode-map)
(bind-key (kbd "n") #'me/org-ref-open-note pdf-view-mode-map)
(bind-key (kbd "g") #'pdf-view-goto-page pdf-view-mode-map)
(bind-key (kbd "k") #'me/pdf-view-scroll-half-backward pdf-view-mode-map)
(bind-key (kbd "z") #'delete-other-windows pdf-view-mode-map)

(bind-key (kbd "<right>") #'pdf-view-next-page-command pdf-view-mode-map)
(bind-key (kbd "<left>") #'pdf-view-previous-page-command pdf-view-mode-map)

(setq pdf-view-midnight-colors '("#e5e5e5" . "#333333"))

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

(defun me/org-ref-open-pdf (&optional arg)
  "Open the associated PDF."
  (interactive "P")
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
               (arg
                (file-name-base (buffer-name)))
               (t nil)))
         (pdf-file (funcall org-ref-get-pdf-filename-function key)))
    (if (file-exists-p pdf-file)
        (if arg
            (start-process "xdg-open" nil "setsid" "xdg-open" pdf-file)
          (org-open-file pdf-file))
      (if (derived-mode-p 'pdf-view-mode)
          (message "Already opened")
        (message "No PDF found with name %s" pdf-file)))))

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
