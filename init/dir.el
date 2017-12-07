;;; -*- lexical-binding: t -*-

(use-package dired-x
  :ensure nil
  :hook (dired-load-hook . init-dired-load)
  :init
  (progn
    (defun init-dired-load ()
      (load "dired-x"))))

(use-package treemacs)

(use-package treemacs-projectile
  :after projectile
  :bind (("C-c g t" . treemacs-projectile-toggle)))
