;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package iedit
  :demand
  :diminish "🙾"
  :bind (:map iedit-mode-keymap
              ("<return>" . iedit--quit)))

(use-package multiple-cursors
  :bind (("C-:" . init-multiple-cursors-edit)
         ("<M-S-down>" . mc/mark-next-lines)
         ("<M-S-up>" . mc/mark-previous-lines)
         ("<M-down-mouse-1>" . mc/toggle-cursor-on-click))
  :init
  (progn
    (setq mc/list-file (expand-file-name "multiple-cursors.el" init-config-directory)))
  :config
  (progn
    (defun init-multiple-cursors-edit ()
      (interactive)
      (if (region-active-p)
          (mc/edit-lines)
        (mc/mark-all-like-this-dwim nil)))))
