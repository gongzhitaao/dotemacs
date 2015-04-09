;;; tex-conf.el --- Bibtex conf
;;; Time-stamp: <2015-04-08 16:53:06 CDT gongzhitaao>

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

(setq reftex-default-bibliography
      '("/home/gongzhitaao/Documents/bib/nn.bib"
        "/home/gongzhitaao/Documents/bib/sp.bib"))

(provide 'tex-conf)
;;; tex-conf.el ends here
