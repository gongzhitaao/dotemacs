(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (latex . t)
                               (python . t)
                               (C . t)
                               (js . t)))

(setq org-babel-js-cmd "nodejs")

;; (defun gzt/org-publish-rename (file newname)
;;   "Rename FILE to NEWNAME if FILE exists."
;;   (let* ((d (plist-get project-plist :publishing-directory))
;;          (source (expand-file-name file d))
;;          (target (expand-file-name newname d)))
;;     (if (file-exists-p source)
;;         (rename-file source target t))))

;; (defun gzt/org-publish-flatten ()
;;   "Flatten publishing directories."
;;   (let ((d (plist-get project-plist :publishing-directory)))
;;     ))

;; (buffer-file-name)

;; (add-to-list
;;  'org-publish-project-alist
;;  ("emacsdotd"
;;   :base-directory "~/Dropbox/dotfiles/emacs.d"
;;   :base-extension "org"
;;   :publishing-directory "~/Documents/emacsdotd"
;;   :publishing-function org-html-publish-to-html
;;   :htmlized-source t
;;   :completion-function (lambda () (gzt/org-publish-rename "README.html" "index.html"))))

;; (add-to-list
;;  'org-publish-project-alist
;;  ("comp3220-slide"
;;     :base-directory "~/Documents/comp3220/master/slide"
;;     :publishing-directory "~/Documents/comp3220/gh-pages/slide"
;;     :publishing-function org-beamer-publish-to-pdf
;;     :htmlized-source t
;;     :recursive t
;;     :completion-function gzt/org-publish-flatten)
;;  ("comp3220-homework"
;;   :base-directory "~/Documents/comp3220/master/assignment"
;;   :publishing-directory "~/Documents/comp3220/master/build/assignment"
;;   :publishing-function org-latex-publish-to-pdf
;;   :htmlized-source t)
;;  ("comp3220-exam"
;;   :base-directory "~/Documents/comp3220/master/exam"
;;   :publishing-directory "~/Documents/comp3220/build/exam"
;;   :publishing-function org-latex-publish-to-pdf
;;   :htmlized-source t)
;;  ("comp3220-homepage"
;;   :base-directory "~/Documents/comp3220/master/homepage"
;;   :publishing-directory "~/Documents/comp3220/gh-pages/"
;;   :publishing-function org-html-publish-to-html
;;   :htmlized-source t)
;;  ("comp3220" :components ("comp3220-homepage"
;;                           "comp3220-slide"
;;                           "comp3220-homework"
;;                           "comp3220-exam")))

;; (setq
;;  org-publish-project-alist
;;  '(

;;    ("malware-detection-doc"
;;     :base-directory "~/Documents/malware-detection/master"
;;     :publishing-directory "~/Documents/malware-detection/gh-pages"
;;     :publishing-function org-html-publish-to-html
;;     :htmlized-source t
;;     :completion-function
;;     (lambda ()
;;       (let ((source (expand-file-name
;;                      "README.html"
;;                      "~/Documents/malware-detection/gh-pages"))
;;             (target (expand-file-name
;;                      "index.html"
;;                      "~/Documents/malware-detection/gh-pages")))
;;         (if (file-exists-p source)
;;             (progn
;;               (if (file-exists-p target)
;;                   (delete-file target))
;;               (rename-file source target))))))
;;    ("malware-detection-static"
;;     :base-directory "~/Documents/malware-detection/master"
;;     :publishing-directory "~/Documents/malware-detection/gh-pages"
;;     :publishing-function org-publish-attachment
;;     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|svg"
;;     :recursive t)
;;    ("malware-detection" :components ("malware-detection-doc"
;;                                      "malware-detection-static"))
;;    ("abide"
;;     :base-directory "~/Documents/abide/master"
;;     :publishing-directory "~/Documents/abide/gh-pages"
;;     :publishing-function org-html-publish-to-html
;;     :htmlized-source t
;;     :completion-function
;;     (lambda ()
;;       (let ((source (expand-file-name
;;                      "README.html"
;;                      "~/Documents/abide/gh-pages"))
;;             (target (expand-file-name
;;                      "index.html"
;;                      "~/Documents/abide/gh-pages")))
;;         (if (file-exists-p source)
;;             (progn
;;               (if (file-exists-p target)
;;                   (delete-file target))
;;               (rename-file source target))))))

;;    ("rbm"
;;     :base-directory "~/Documents/rbm/master"
;;     :publishing-directory "~/Documents/rbm/gh-pages"
;;     :publishing-function org-html-publish-to-html
;;     :htmlized-source t
;;     :completion-function
;;     (lambda ()
;;       (let ((source (expand-file-name
;;                      "README.html"
;;                      "~/Documents/rbm/gh-pages"))
;;             (target (expand-file-name
;;                      "index.html"
;;                      "~/Documents/rbm/gh-pages")))
;;         (if (file-exists-p source)
;;             (progn
;;               (if (file-exists-p target)
;;                   (delete-file target))
;;               (rename-file source target))))))

;;    ("orgcss"
;;     :base-directory "~/Documents/orgcss/master/app"
;;     :publishing-directory "~/Documents/orgcss/master/dist"
;;     :publishing-function org-html-publish-to-html
;;     :htmlized-source t
;;     :completion-function
;;     (lambda ()
;;       (let ((source (expand-file-name
;;                      "README.html"
;;                      "~/Documents/orgcss/master/app"))
;;             (target (expand-file-name
;;                      "index.html"
;;                      "~/Documents/orgcss/master/dist")))
;;         (if (file-exists-p source)
;;             (progn
;;               (if (file-exists-p target)
;;                   (delete-file target))
;;               (rename-file source target))))))))
