;;; yank.el --- Custom yank function with image support -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides advice for the yank function to detect and save images
;; from the clipboard, automatically creating an assets directory and inserting
;; org-mode or markdown style image links.

;;; Code:

(defun me--get-clipboard-image-data ()
  "Check if clipboard contains image data and return the image type.
Returns a cons cell (TYPE . DATA) where TYPE is the image type (e.g., 'png, 'jpeg)
and DATA is the image data, or nil if clipboard doesn't contain an image."
  (let ((types '(png jpeg jpg gif bmp)))
    (catch 'found
      (dolist (type types)
        (condition-case nil
            (let ((data (gui-get-selection 'CLIPBOARD (intern (format "image/%s" type)))))
              (when data
                (throw 'found (cons type data))))
          (error nil)))
      nil)))

(defun me--next-available-image-number (directory)
  "Find the next available image number in DIRECTORY.
Returns the first N where imgN.* does not exist."
  (let ((n 0))
    (while (directory-files directory nil (format "^img%d\\." n))
      (setq n (1+ n)))
    n))

(defun me--image-extension-for-type (type)
  "Return the appropriate file extension for image TYPE."
  (pcase type
    ('png "png")
    ('jpeg "jpg")
    ('jpg "jpg")
    ('gif "gif")
    ('bmp "bmp")
    (_ "png")))

(defun me--yank-image-advice (orig-fun &rest args)
  "Advice for yank to handle images in text-mode.
ORIG-FUN is the original yank function, ARGS are its arguments."
  ;; Fast path: check if clipboard has text first
  (if (condition-case nil
          (gui-get-selection 'CLIPBOARD 'STRING)
        (error nil))
      ;; Clipboard has text, use normal yank
      (apply orig-fun args)
    ;; No text in clipboard, check for image
    (let ((image-data (me--get-clipboard-image-data)))
      (if (and image-data
               (derived-mode-p 'text-mode))
        ;; Handle image paste in text-mode
        (let* ((type (car image-data))
               (data (cdr image-data))
               (buffer-file (buffer-file-name))
               (default-directory (if buffer-file
                                     (file-name-directory buffer-file)
                                   default-directory)))
          (unless buffer-file
            (user-error "Buffer is not visiting a file. Please save the buffer first"))

          ;; Create assets directory
          (let* ((base-name (file-name-sans-extension (file-name-nondirectory buffer-file)))
                 (assets-dir (expand-file-name base-name default-directory))
                 (next-num (progn
                             (unless (file-exists-p assets-dir)
                               (make-directory assets-dir t))
                             (me--next-available-image-number assets-dir)))
                 (ext (me--image-extension-for-type type))
                 (default-name (format "img%d.%s" next-num ext))
                 (image-name (read-string (format "Image name (default: %s): " default-name)
                                         nil nil default-name))
                 (image-path (expand-file-name image-name assets-dir)))

            ;; Save image data to file
            (with-temp-file image-path
              (set-buffer-multibyte nil)
              (insert data))

            ;; Insert appropriate link format based on mode
            (let ((link (if (derived-mode-p 'markdown-mode)
                           (format "![%s](%s)" (file-name-sans-extension image-name) image-path)
                         (format "[[file:%s]]" image-path))))
              (insert link)
              (message "Image saved to %s" image-path))))

        ;; Handle normal text paste - call original function
        (apply orig-fun args)))))

;; Add advice to yank function
(advice-add 'yank :around #'me--yank-image-advice)

(provide 'yank)
;;; yank.el ends here
