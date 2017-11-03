
(use-package navi-mode
  :defer t)

(use-package outshine
  :defer t
  :diminish (outline-minor-mode . " ✦")
  :init
  (progn
    (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
    (add-hook 'prog-mode-hook 'outline-minor-mode)))

(use-package poporg
  :defer t)
