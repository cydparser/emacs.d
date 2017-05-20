;;; -*- lexical-binding: t -*-

(use-package dired-x
  :defer t
  :ensure nil
  :init
  (progn
    (defun init-dired-load ()
      (require 'dired-x))

    (add-hook 'dired-load-hook #'init-dired-load)))
