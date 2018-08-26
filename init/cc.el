;;; -*- lexical-binding: t -*-

(use-package cc-mode
  :ensure nil
  :hook (java-mode-hook . init-java)
  :init
  (progn
    (defun init-java ()
      (setq c-basic-offset 4
            tab-stop-list (number-sequence 4 fill-column 4)
            tab-width 4)
      (c-set-offset 'arglist-close 0)
      (c-set-offset 'arglist-intro '+)
      (c-set-offset 'case-label '+))))
(use-package cmake-mode)

(use-package cquery
  :hook (c++-mode-hook . lsp-cquery-enable))
