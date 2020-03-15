;;; solarized-lightme-theme.el ---                  -*- lexical-binding: t; -*-

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

(require 'solarized)
(eval-when-compile
  (require 'solarized-palettes))

(deftheme solarized-lightme "The light variant of the Solarized colour theme")

(solarized-with-color-variables 'light 'solarized-lightme
  solarized-light-color-palette-alist
  '(
    (custom-theme-set-faces
     'solarized-lightme
     ;; +--- Base ---+
     `(bold ((,class (:weight bold))))
     `(bold-italic ((,class (:weight bold :slant italic))))
     `(italic ((,class (:slant italic :underline nil))))
     `(underline ((,class (:underline t))))

     `(mode-line
       ((,class (:inverse-video unspecified
                 :overline ,s-mode-line-bg
                 :underline unspecified
                 :foreground ,s-mode-line-fg
                 :background ,s-mode-line-bg
                 :box (:line-width 1 :color ,s-mode-line-bg
                       :style unspecified)))))
     `(line-number-current-line ((,class (:foreground ,base3
                                          :background ,green-l))))

     `(window-divider ((,class (:foreground ,s-mode-line-bg))))
     `(window-divider-first-pixel ((,class (:foreground ,s-mode-line-bg))))
     `(window-divider-last-pixel ((,class (:foreground ,s-mode-line-bg))))

     `(me-dired-dim-0 ((,class (:foreground "gray60"))))
     `(me-dired-dim-1 ((,class (:foreground "gray80"))))
     `(me-dired-executable ((,class (:foreground ,green))))
     )

    (setq org-todo-keyword-faces
        `(("TODO" :foreground ,red-l :weight bold)
          ("NEXT" :foreground ,blue-l :weight bold)
          ("DONE" :foreground ,green-l :weight bold)
          ("WAIT" :foreground ,yellow-l :weight bold)
          ("HOLD" :foreground ,magenta-l :weight bold)
          ("KILL" :foreground ,green :weight bold)))
    ))

(setq solarized-use-variable-pitch nil
      solarized-scale-outline-headlines nil
      solarized-scale-org-headlines nil)

(provide-theme 'solarized-lightme)

(provide 'solarized-lightme-theme)

;;; solarized-lightme-theme.el ends here
