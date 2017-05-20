;;; -*- lexical-binding: t -*-

(use-package sh-script
  :defer t
  :init
  (progn
    (defun init-sh ()
      (unless (file-exists-p buffer-file-name)
        (sh-set-shell "/usr/bin/env bash" t t)))

    (setq sh-basic-offset 2
          sh-indentation 2)
    (add-hook 'sh-mode-hook #'init-sh)))
