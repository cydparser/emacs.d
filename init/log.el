;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package logview
  :custom
  (logview-cache-filename (expand-file-name "logview-cache.extmap" init-var-directory))
  (logview-views-file (expand-file-name "logview.views" init-config-directory))
  :config
  (progn
    (when (member "Inconsolata" (font-family-list))
      (add-hook 'logview-mode-hook
                (lambda () (buffer-face-set :family "Inconsolata"))))))
