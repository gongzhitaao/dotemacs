;;; python-conf.el --- Python intepreter
;;; Time-stamp: <2014-09-27 19:28:54 CDT gongzhitaao>

(require 'python-mode)
(when (featurep 'python) (unload-feature 'python t))

(setq py-shell-name "ipython"
      py-shell-prompt-regexp "In \\[[0-9]+\\]: "
      py-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")

(add-hook
 'python-mode-hook
 (lambda ()
   (setq py-python-command-args '("--colors=linux"))

   (local-unset-key (kbd "C-c C-c")) ;py-execute-buffer
   (local-set-key (kbd "C-c C-c") 'py-execute-line-ipython)
   (local-set-key (kbd "C-c C-x b") 'py-execute-buffer-ipython)
   (local-set-key (kbd "C-c C-x C-b") 'py-execute-buffer-ipython)
   (local-set-key (kbd "C-c C-x d") 'py-execute-def-ipython)
   (local-set-key (kbd "C-c C-x C-d") 'py-execute-def-ipython)
   (local-set-key (kbd "C-c C-x c") 'py-execute-class-ipython)
   (local-set-key (kbd "C-c C-x C-c") 'py-execute-class-ipython)
   (local-set-key (kbd "C-c C-x r") 'py-execute-region-ipython)
   (local-set-key (kbd "C-c C-x C-r") 'py-execute-region-ipython)
   (local-set-key (kbd "C-c C-x f") 'py-execute-file-ipython)
   (local-set-key (kbd "C-c C-x C-f") 'py-execute-file-ipython)))

(provide 'python-conf)
;;; python-conf.el ends here
