;;; font.conf.el
;;; Time-stamp: <2013-08-24 09:29:26 CDT gongzhitaao>

;; -------------------------------------------------------------------
;; encoding
;; -------------------------------------------------------------------
(let ((my-prefer-coding-system
       '(cp950 gb2312 cp936 gb18030 utf-16-unix utf-8-unix)))
  (dolist (c my-prefer-coding-system)
    (prefer-coding-system c)))

(when (display-graphic-p)
  (progn
    ;; en
    (set-face-attribute 'default nil :font "Dejavu Sans Mono:pixelsize=14")

    ;; zh
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font
       (frame-parameter nil 'font)
       charset (font-spec :family "WenQuanYi Zen Hei Mono"
                          :size 16)))))

(provide 'font.conf)
