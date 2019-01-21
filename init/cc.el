;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package cc-mode
  :ensure nil
  :hook ((c-mode-common-hook . init-cc)
         (java-mode-hook . init-java))
  :init
  (progn
    (require 'google-c-style)

    (defun init-cc ()
      (c-add-style "Google" google-c-style t))

    (defun init-java ()
      (c-set-offset 'arglist-cont 0)
      (c-set-offset 'arglist-cont-nonempty '++)
      (c-set-offset 'arglist-intro '++)
      (c-set-offset 'arglist-close 0))))

(use-package cmake-mode)

(use-package cquery
  :hook (c++-mode-hook . lsp-cquery-enable))

(use-package lsp-java
  :hook (java-mode-hook . lsp)
  :init
  (progn
    (let ((jtd-dir (getenv "JDT_LSP"))
          (workspace (expand-file-name "lsp-java/" init-var-directory)))
      (setq lsp-java-format-settings-profile "Google"
            lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
            lsp-java-import-gradle-enabled nil
            lsp-java-import-order '("com" "java" "javax" "org")
            lsp-java-java-path (expand-file-name "jre/bin/java" jtd-dir)
            lsp-java-server-install-dir jtd-dir
            lsp-java-workspace-cache-dir (expand-file-name "cache/" workspace)
            lsp-java-workspace-dir workspace))

    (with-eval-after-load "lsp-mode"
      (require 'lsp-java)))
  :config
  (progn
    (defun init-lsp-java--locate-server-config (f)
      "Use a writable location for the config."
      (let* ((ro-dir (funcall f))
             (dir (expand-file-name (file-name-base ro-dir) lsp-java-workspace-dir))
             (ini (expand-file-name "config.ini" dir)))
        (unless (file-exists-p ini)
          (make-directory dir t)
          (copy-file (expand-file-name "config.ini" ro-dir) ini)
          (set-file-modes ini #o640))
        dir))

    (advice-add 'lsp-java--locate-server-config :around
                #'init-lsp-java--locate-server-config)

    (defun lsp-java--ensure-server ()
      (message "lsp-java--ensure-server is disabled"))))
