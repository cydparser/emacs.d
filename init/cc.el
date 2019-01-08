;;; -*- lexical-binding: t -*-

(use-package cc-mode
  :ensure nil
  :hook (c-mode-common-hook . init-cc)
  :init
  (progn
    (require 'google-c-style)

    (defun init-cc ()
      (c-add-style "Google" google-c-style t))))

(use-package cmake-mode)

(use-package cquery
  :hook (c++-mode-hook . lsp-cquery-enable))
