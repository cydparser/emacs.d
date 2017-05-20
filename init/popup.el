;;; -*- lexical-binding: t -*-

(use-package popup
  :defer t
  :ensure nil
  :bind (:map popup-menu-keymap
              ("M-n" . popup-next)
              ("M-p" . popup-previous)))
