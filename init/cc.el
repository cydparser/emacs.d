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

(use-package lsp-java
  :after lsp-mode
  :init
  (progn
    (let ((workspace (expand-file-name "lsp-java/" init-var-directory)))
      (setq lsp-java-format-settings-profile "Google"
            lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
            lsp-java-import-gradle-enabled nil
            lsp-java-completion-import-order '("com" "java" "javax" "org")
            lsp-java-workspace-cache-dir (expand-file-name "cache/" workspace)
            lsp-java-workspace-dir workspace)))

  (with-eval-after-load "lsp-mode"
    (require 'lsp-java)))
