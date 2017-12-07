;;; -*- lexical-binding: t -*-

(use-package multi-term)

(use-package shell
  :ensure nil
  :bind (:map shell-mode-map
              ("SPC" . comint-magic-space)))
