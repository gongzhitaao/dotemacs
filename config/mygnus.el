
(require 'gnus)

(setq user-full-name "Zhitao Gong")
(setq user-mail-address "zhitaao.gong@gmail.com")

(add-to-list 'mm-attachment-override-types "image/.*")

(setq gnus-select-method '(nntp "127.0.0.1")
      gnus-secondary-select-methods
      '((nnimap "sina"
                (nnimap-address "sina.com")
                (nnimap-server-port 143)
                (nnimap-stream network)
                (nnimap-authinfo-file "./authinfo")
                (nnimap-expunge-on-close always)
                (gnus-check-new-newsgroups nil)
)))
