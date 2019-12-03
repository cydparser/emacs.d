;;; -*- lexical-binding: t -*-

(use-package dired
  :ensure nil
  :init (setq dired-listing-switches "-alht"))

(use-package dired-subtree
  :demand
  :after dired
  :bind (:map dired-mode-map
              ("e" . dired-subtree-toggle)
              ("<tab>" . dired-subtree-cycle)
              ("TAB" . dired-subtree-cycle)
              ("C-M-u" . dired-subtree-up)
              ("C-M-d" . dired-subtree-down)))

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
