;;; -*- lexical-binding: t -*-

(use-package dired-x
  :ensure nil
  :hook (dired-load-hook . init-dired-load)
  :config
  (progn
    (defun init-dired-load ()
      (require 'dired-x))))

(use-package treemacs)

(use-package treemacs-projectile
  :after projectile
  :bind (("C-c g t" . treemacs-projectile-toggle)))
