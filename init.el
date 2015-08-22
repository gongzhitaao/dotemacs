;;; init.el -- Entry to all my configuration
;;; Commentary:
;;; Time-stamp: <2015-08-05 10:49:38 gongzhitaao>
;;; Code:

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
(require 'org)
(org-babel-load-file "~/.emacs.d/README.org")
