;;; -*- lexical-binding: t -*-

(use-package iedit
  :demand
  :diminish "🙾"
  :bind (:map iedit-mode-keymap
              ("C-o" . iedit-show/hide-unmatched-lines)))
