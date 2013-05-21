;; misc.conf.el
;; Time-stamp: <2013-05-20 17:19:15 CDT gongzhitaao>

;; -------------------------------------------------------------------
;; highlight-changes-mode
;; -------------------------------------------------------------------
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil)

;; already defined in `bindings.el'
;;(global-set-key (kbd "<f6>") 'highlight-changes-visible-mode)
;;(global-set-key (kbd "M-n") 'highlight-changes-next-change)
;;(global-set-key (kbd "M-p") 'highlight-changes-previous-change)

(global-highlight-changes-mode)
(set-face-foreground 'highlight-changes nil)
(set-face-background 'highlight-changes "gray13")
(set-face-foreground 'highlight-changes-delete nil)
(set-face-background 'highlight-changes-delete "gray18")

;; -------------------------------------------------------------------
;; fci-mode
;; -------------------------------------------------------------------
(require 'fill-column-indicator)
(add-hook 'find-file-hook
          (lambda ()
            (setq fci-rule-color "darkgreen")
            (fci-mode t)))

;; -------------------------------------------------------------------
;; rainbow-delimiters-mode
;; -------------------------------------------------------------------
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; -------------------------------------------------------------------
;; bibtex-mode
;; -------------------------------------------------------------------
(add-hook 'bibtex-mode-hook
          '(lambda ()
             (local-set-key (kbd "s-\\") 'bibtex-fill-entry)
             (set-fill-column 120)))

(setq bibtex-text-indentation 20)
(setq bibtex-align-at-equal-sign t)
(bibtex-set-dialect 'biblatex)

;; -------------------------------------------------------------------
;; calendar
;; -------------------------------------------------------------------
(require 'appt)

(add-hook 'calendar-load-hook
          (lambda ()
            (calendar-mark-holidays)
            (diary-mark-entries)))

(add-hook 'calendar-today-visible-hook
          (lambda ()
            (calendar-mark-today)))

(add-hook 'diary-list-entries-hook
          (lambda ()
            (diary-sort-entries)))

(setq diary-file "~/Documents/dotemacs/diary")

(calendar-set-date-style 'iso)


;; -------------------------------------------------------------------
;; encrypt
;; -------------------------------------------------------------------
(require 'epa-file)

(defadvice epg--start (around advice-epg-disable-agent disable)
  "Make epg--start not able to find a gpg-agent"
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (setenv "GPG_AGENT_INFO" agent)))

(defun epg-disable-agent ()
  "Make EasyPG bypass any gpg-agent"
  (interactive)
  (ad-enable-advice 'epg--start 'around 'advice-epg-disable-agent)
  (ad-activate 'epg--start)
  (message "EasyPG gpg-agent bypassed"))

(defun epg-enable-agent ()
  "Make EasyPG use a gpg-agent after having been disabled with
epg-disable-agent"
  (interactive)
  (ad-disable-advice 'epg--start 'around 'advice-epg-disable-agent)
  (ad-activate 'epg--start)
  (message "EasyPG gpg-agent re-enabled"))

(epg-disable-agent)
