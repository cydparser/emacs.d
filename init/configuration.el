;;; -*- lexical-binding: t -*-

(use-package dhall-mode
  :if (executable-find "dhall"))

;; TODO Use 'dhall lint'

(use-package json-mode)

(use-package just-mode)

(use-package nickel-mode)

(use-package yaml-ts-mode
  :ensure nil
  :mode ("\\.ya?ml\\'"
         "\\.yamlfmt\\'"))
