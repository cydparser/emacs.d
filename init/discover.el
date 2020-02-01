;;; -*- lexical-binding: t -*-

(use-package info-colors
  :hook (Info-selection-hook . info-colors-fontify-node))

(use-package which-key
  :demand
  :diminish ""
  :pin melpa
  :hook (after-init-hook . which-key-mode))
