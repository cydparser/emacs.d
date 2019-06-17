;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package iedit
  :demand
  :diminish "🙾"
  :bind (:map iedit-mode-keymap
              ("<return>" . iedit-quit)
              ("C-o" . iedit-show/hide-unmatched-lines)))

(use-package multiple-cursors
  :init
  (progn
    (setq mc/list-file (expand-file-name "multiple-cursors.el" init-config-directory)))
  :config
  (progn
    (defun init-refactor-mark-dwim ()
      (interactive)
      (if (region-active-p)
          (mc/edit-lines)
        (mc/mark-all-like-this-dwim nil)))))
