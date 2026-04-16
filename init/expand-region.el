;;; -*- lexical-binding: t -*-

(use-package expreg
  :bind (("M-]" . expreg-expand)
         ("M-[" . expreg-contract))
  :config
  (progn
    (defvar-keymap expreg-repeat-map
      :repeat t
      "]" #'expreg-expand
      "[" #'expreg-contract)))
