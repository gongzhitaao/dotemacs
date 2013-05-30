;; hs.conf.el
;; Time-stamp: <2013-05-29 23:10:05 CDT gongzhitaao>

(defface collapsed-face '((t (:background "#e0cf9f" :foreground "#5f5f5f"))) "Collapsed Overlay")
(defvar collapsed-face 'collapsed-face)

(define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])
(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((marker-string "*fringe-dummy*")
           (marker-length (length marker-string))
           (display-string
            (format " (%d)... "
                    (count-lines (overlay-start ov) (overlay-end ov)))))
      (overlay-put ov 'help-echo "C-` to toggle")
      (put-text-property 0 marker-length 'display
                         (list 'left-fringe 'hs-marker 'fringe-face)
                         marker-string)
      (overlay-put ov 'before-string marker-string)
      (put-text-property 1 (1- (length display-string))
                         'face 'collapsed-face display-string)
      (overlay-put ov 'display display-string))))
(setq hs-set-up-overlay 'display-code-line-counts)
