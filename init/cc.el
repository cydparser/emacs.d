;;; -*- lexical-binding: t -*-

(use-package cc-mode
  :ensure nil
  :hook ((c-mode-common-hook . init-cc)
         (java-mode-hook . init-java))
  :init
  (progn
    (require 'google-c-style)

    (defun init-cc ()
      (c-add-style "Google" google-c-style t))

    (defun init-java ()
      (c-set-offset 'arglist-cont 0)
      (c-set-offset 'arglist-cont-nonempty '++)
      (c-set-offset 'arglist-intro '++)
      (c-set-offset 'arglist-close 0))))

(use-package cmake-mode)

(use-package cquery
  :hook (c++-mode-hook . lsp-cquery-enable))
