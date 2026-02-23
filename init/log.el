;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package logview
  :hook (logview-mode-hook . init-logview)
  :custom
  (logview-cache-filename (expand-file-name "logview-cache.extmap" init-var-directory))
  (logview-views-file (expand-file-name "logview.views" init-config-directory))
  :config
  (progn
    (require 'init-font)

    (defun init-logview ()
      (init-font-for-buffer "IosevkaTerm Nerd Font" "IosevkaTerm NF"))))
