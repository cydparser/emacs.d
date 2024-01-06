;;; -*- lexical-binding: t -*-

(use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 2)
  :config
  (progn
    (unbind-key "C-c C-f" sh-mode-map)))
