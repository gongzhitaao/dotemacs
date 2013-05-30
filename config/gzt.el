;;; gzt.el
;;; Time-stamp: <2013-05-29 20:54:35 CDT gongzhitaao>
;;;
;;; Convinient custome functions

(defun gzt/add-hooks (func hooks)
  (mapc (lambda (hook)
	  (add-hook hook func))
	hooks))

(provide 'gzt)
