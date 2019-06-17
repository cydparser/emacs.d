;;; -*- lexical-binding: t -*-

(use-package navi-mode)

(use-package outshine
  :diminish ((outline-minor-mode . "٭")
             (outshine-mode . " "))
  :hook ((outline-minor-mode-hook . outshine-mode)
         (prog-mode-hook . outline-minor-mode)))

(use-package poporg)
