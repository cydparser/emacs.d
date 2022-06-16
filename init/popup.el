;;; -*- lexical-binding: t -*-

(use-package popup
  :ensure nil
  :bind (:map popup-menu-keymap
              ("M-n" . popup-next)
              ("M-p" . popup-previous))
  :custom-face
  (popup-tip-face ((t (:background "#000000")))))
