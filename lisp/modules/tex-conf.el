;;; tex-conf.el --- Bibtex conf
;;; Time-stamp: <2014-04-26 11:05:49 CDT gongzhitaao>

(setq bibtex-dialect 'biblatex)
(setq bibtex-align-at-equal-sign t)
(setq bibtex-text-indentation 20)

(add-hook 'bibtex-mode-hook
          (lambda ()
            (local-set-key (kbd "s-\\") 'bibtex-fill-entry)
            (setq fill-column 140)
            (hs-minor-mode 1)
            (hl-line-mode 1)))

(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-auctex t)

(provide 'tex-conf)
;;; tex-conf.el ends here
