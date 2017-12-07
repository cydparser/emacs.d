;;; -*- lexical-binding: t -*-

(use-package nxml-mode
  :ensure nil
  :hook (web-mode-hook . init-nxml)
  :init
  (progn
    (defun init-nxml ()
      (add-to-list (make-local-variable 'company-backends) #'company-nxml))))
