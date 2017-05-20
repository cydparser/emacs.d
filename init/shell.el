;;; -*- lexical-binding: t -*-

(use-package shell
  :defer t
  :ensure nil
  :bind (:mode shell-mode-map
               ("SPC" . comint-magic-space)))
