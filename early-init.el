;;; early-init.el --- Early init.                    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Zhitao Gong

;; Author: Zhitao Gong <zhitaao.gong@gmail.com>
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

(setopt package-enable-at-startup nil)
(set-language-environment "UTF-8")

(require 'xdg)
(startup-redirect-eln-cache
 (expand-file-name "emacs/eln-cache" (xdg-cache-home)))

(provide 'early-init)
;;; early-init.el ends here
