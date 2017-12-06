;;; -*- lexical-binding: t -*-

(use-package shell
  :ensure nil
  :bind (:mode shell-mode-map
               ("SPC" . comint-magic-space)))
