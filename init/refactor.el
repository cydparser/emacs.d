;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package iedit
  :demand
  :diminish "🙾"
  :bind (:map iedit-mode-keymap
              ("<return>" . iedit--quit)))

(use-package multiple-cursors
  :bind (("<M-S-down>" . mc/mark-next-lines)
         ("<M-S-up>" . mc/mark-previous-lines)
         ("<M-down-mouse-1>" . mc/toggle-cursor-on-click))
  :custom
  (mc/list-file (expand-file-name "multiple-cursors.el" init-config-directory)))
