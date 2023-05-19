;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package logview
  :custom
  (logview-additional-submodes
   '(("TripShot" . ((format . "TIMESTAMP LEVEL (ThreadId THREAD) NAME :")
                    (levels . "SLF4J")))))
  (logview-additional-timestamp-formats
   '(("ISO 8601 datetime + pico + time zone" (java-pattern . "yyyy-MM-dd'T'HH:mm:ss.SSSSSSSSSSSS+0000"))))
  (logview-cache-filename (expand-file-name "logview-cache.extmap" init-var-directory))
  (logview-views-file (expand-file-name "logview.views" init-config-directory))
  :config
  (progn
    (when (member "Inconsolata" (font-family-list))
      (add-hook 'logview-mode-hook
                (lambda () (buffer-face-set :family "Inconsolata"))))))
