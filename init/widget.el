;;; -*- lexical-binding: t -*-

(use-package widget-edit
  :ensure nil
  :bind (:map widget-keymap
              ("SPC" . widget-button-press)
              ("n" . next-line)
              ("p" . previous-line)
              ("q" . init-kill-buffer-current)))
