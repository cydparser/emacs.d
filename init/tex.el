;;; -*- lexical-binding: t -*-

(use-package auctex)

(use-package tex
  :ensure nil
  :custom
  (TeX-auto-save t)
  (TeX-newline-function 'newline-and-indent)
  (TeX-parse-self t))
