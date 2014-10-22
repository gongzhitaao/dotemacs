;;; octave-conf.el --- Octave mode config
;;; Time-stamp: <2014-10-21 15:27:04 CDT gongzhitaao>

(setq octave-continuation-string "...")

(add-hook
 'octave-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c C-v") 'octave-help)))


(provide 'octave-conf)
;;; octave-conf.el ends here
