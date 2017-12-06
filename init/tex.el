;;; -*- lexical-binding: t -*-

(use-package auctex)

(use-package tex
  :ensure nil
  :init
  (progn
    (setq TeX-auto-save t
          TeX-parse-self t)))
