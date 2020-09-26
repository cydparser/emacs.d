;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package iedit
  :demand
  :ensure nil
  :diminish "🙾"
  :bind (:map iedit-mode-keymap
              ("<return>" . iedit-quit)))

(use-package multiple-cursors
  :bind (("C-:" . init-multiple-cursors-edit))
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
