;;; -*- lexical-binding: t -*-

(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package imenu-anywhere
  :bind (("C-c g I" . ivy-imenu-anywhere)
         ("M-g I" . ivy-imenu-anywhere)))
