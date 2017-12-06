;;; -*- lexical-binding: t -*-

(use-package sh-script
  :hook (sh-mode-hook . init-sh)
  :init (setq sh-basic-offset 2
              sh-indentation 2)
  :config
  (progn
    (defun init-sh ()
      (unless (file-exists-p buffer-file-name)
        (sh-set-shell "/usr/bin/env bash" t t)))))
