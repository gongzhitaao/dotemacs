;;; transform-region.el --- Transform region of texts             -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Zhitao Gong

;; Author: Zhitao Gong <gongzhitaao@gongzhitaao>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar isort-executable "isort" "Executable for isort.")

(defun transform-region/isort (start end)
  "Sort python imports in START and END with isort.
isort: <https://github.com/timothycrosley/isort>"
  (interactive "r")
  (deactivate-mark)
  (let ((temp-file-name (make-temp-file "isort"))
        (exit-code 0))
    (write-region start end temp-file-name)
    (delete-region start end)
    (setq exit-code
          (call-process isort-executable nil t t "--stdout" temp-file-name))
    (if (not (= exit-code 0))
        (undo))
    (delete-file temp-file-name)))

(defvar yapf-executable "yapf" "Executable for yapf.")

(defun transform-region/yapfify (start end)
  "Format Python code in START and END with yapf.
yapf: <https://github.com/google/yapf>"
  (interactive "r")
  (deactivate-mark)
  (let ((temp-file-name (make-temp-file "yapf"))
        (exit-code 0))
    (write-region start end temp-file-name)
    (delete-region start end)
    (setq exit-code
          (call-process yapf-executable nil t t temp-file-name))
    (if (not (= exit-code 0))
        (undo))
    (delete-file temp-file-name)))

(defun transform-region (start end transformer)

  (let* ((result (transformer start end))
         (transformed-text (nth 0 result))
         (buffer-name (nth 1 result)))
    (save-mark-and-excursion
      (delete-region start end)
      (if transformed-text
          (insert transformed-text)
        (insert-buffer-substring buffer-name)))))

(provide 'transform-region)
;;; transform-region.el ends here
