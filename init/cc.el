;;; -*- lexical-binding: t -*-

(use-package cc-mode
  :ensure nil
  :hook (java-mode-hook . init-java)
  :init
  (progn
    (defun init-java ()
      (setq c-basic-offset 4
            tab-stop-list (number-sequence 4 fill-column 4)
            tab-width 4))))
