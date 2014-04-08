
(require 'bbdb)
(require 'timezone)
(require 'bbdb-site)

(setq bbdb-file (expand-file-name "personal/contacts.bbdb" user-emacs-directory)
      bbdb-north-american-phone-numbers-p nil)
