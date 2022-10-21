;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package cc-mode
  :ensure nil
  :hook ((c-mode-common-hook . google-set-c-style)
         (java-mode-hook . init-java))
  :init
  (progn
    (require 'google-c-style)

    (defun init-java ()
      (c-set-offset 'arglist-cont 0)
      (c-set-offset 'arglist-cont-nonempty '++)
      (c-set-offset 'arglist-intro '++)
      (c-set-offset 'arglist-close 0)))
  :config
  (progn
    (unbind-key ":" c-mode-base-map)))

(use-package cmake-mode)
