;;; -*- lexical-binding: t -*-

(use-package navi-mode)

(use-package outshine
  :diminish ((outline-minor-mode . "٭")
             (outshine-mode . " "))
  :hook ((outline-minor-mode-hook . outshine-mode)
         (prog-mode-hook . outline-minor-mode))
  :bind (:map outshine-mode-map
              ("C-c C-n" . outline-next-visible-heading)
              ("C-c C-p" . outline-previous-visible-heading)))

(use-package poporg)
