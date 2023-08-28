;;; -*- lexical-binding: t -*-

(use-package sh-script
  :ensure nil
  :hook (sh-mode-hook . init-sh)
  :custom
  (sh-basic-offset 2)
  :init
  (progn
    (defun init-sh ()
      (unless (file-exists-p buffer-file-name)
        (sh-set-shell "/usr/bin/env bash" t t))))
  :config
  (progn
    (unbind-key "C-c C-f" sh-mode-map)))
