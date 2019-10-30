;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package logview
  :init
  (progn
    (setq logview-auto-revert-mode 'auto-revert-mode
          logview-cache-filename (expand-file-name "logview-cache.extmap" init-var-directory)
          logview-views-file (expand-file-name "logview.views" init-config-directory))

    (setq logview-additional-timestamp-formats
          '(("ISO 8601 datetime + pico + time zone" (java-pattern . "yyyy-MM-dd'T'HH:mm:ss.SSSSSSSSSSSS+0000"))))

    (setq logview-additional-submodes
          '(("Tripshot" . ((format . "TIMESTAMP LEVEL (ThreadId THREAD) NAME :")
                           (levels . "SLF4J")))))))
