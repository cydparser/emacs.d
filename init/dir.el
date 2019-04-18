;;; -*- lexical-binding: t -*-

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              ;; ("k" . dired-subtree-remove)
              ("C-d" . dired-subtree-remove)
              ;; ("TAB" . dired-subtree-toggle)
              ("TAB" . dired-subtree-cycle)
              ;; dired-subtree-revert
              ;; dired-subtree-narrow
              ("C-M-u" . dired-subtree-up)
              ("C-M-d" . dired-subtree-down)
              ;; dired-subtree-next-sibling
              ;; dired-subtree-previous-sibling
              ;; dired-subtree-beginning
              ;; dired-subtree-end
              ;; dired-subtree-unmark-subtree
              ;; dired-subtree-only-this-directory
              ))

(use-package dired-x
  :ensure nil
  :hook (dired-load-hook . init-dired-load)
  :init
  (progn
    (defun init-dired-load ()
      (load "dired-x"))))

;; Type C-x C-q in a dired buffer.
(use-package wdired
  :ensure nil
  :init (setq wdired-allow-to-change-permissions t))
