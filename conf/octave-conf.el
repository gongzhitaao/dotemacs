;;; octave-conf.el --- Octave mode config
;;; Time-stamp: <2015-05-05 21:10:05 CDT gongzhitaao>

(setq octave-continuation-string "...")

(add-hook
 'octave-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c C-v") 'octave-help)))

;;; octave-conf.el ends here
