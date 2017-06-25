;;; -*- lexical-binding: t -*-

(use-package dumb-jump
  :defer t
  :bind (("C-c j j" . dumb-jump-go)
         ("C-c j p" . dumb-jump-back)
         ("C-c j l" . dumb-jump-quick-look))
  :init (setq dumb-jump-selector 'ivy)
  :config
  (progn
    (unbind-key "C-M-g" dumb-jump-mode-map)
    (unbind-key "C-M-p" dumb-jump-mode-map)
    (unbind-key "C-M-q" dumb-jump-mode-map)))
