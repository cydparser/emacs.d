
(use-package navi-mode)

(use-package outshine
  :diminish (outline-minor-mode . " ✦")
  :hook ((outline-minor-mode-hook . outshine-hook-function)
         (prog-mode-hook . outline-minor-mode)))

(use-package poporg)
