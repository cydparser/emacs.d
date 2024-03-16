;;; -*- lexical-binding: t -*-

(use-package navi-mode)

(use-package outshine
  :diminish ((outline-minor-mode . "٭")
             (outshine-mode . " "))
  :bind (:map outshine-mode-map
              ("C-c C-n" . outline-next-visible-heading)
              ("C-c C-p" . outline-previous-visible-heading))
  :config
  (progn
    (unbind-key "<M-S-down>" outshine-mode-map)
    (unbind-key "<M-down>" outshine-mode-map)
    (unbind-key "<M-up>" outshine-mode-map)
    (unbind-key "@" outshine-mode-map)))

(use-package poporg)
