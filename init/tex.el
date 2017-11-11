;;; -*- lexical-binding: t -*-

(use-package auctex
  :defer t)

(use-package tex
  :defer t
  :ensure nil
  :init
  (progn
    (setq TeX-auto-save t
          TeX-parse-self t)))
