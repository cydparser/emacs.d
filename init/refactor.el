;;; -*- lexical-binding: t -*-

(use-package iedit
  :demand
  :diminish "🙾"
  :bind (:map iedit-mode-keymap
              ("<return>" . iedit-quit)
              ("C-o" . iedit-show/hide-unmatched-lines)))
