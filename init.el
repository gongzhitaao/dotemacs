;;; init.el -- Entry to all my configuration
;;; Commentary:
;;; Time-stamp: <2015-12-02 17:02:10 gongzhitaao>
;;; Code:

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
(require 'org)
(org-babel-load-file "~/.emacs.d/README.org")
