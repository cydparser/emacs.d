;;; -*- lexical-binding: t -*-

(use-package dired-x
  :ensure nil
  :hook (dired-load-hook . init-dired-load)
  :config
  (progn
    (defun init-dired-load ()
      (require 'dired-x))))

