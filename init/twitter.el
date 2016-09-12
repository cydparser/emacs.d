(use-package twittering-mode
  :defer t
  :init
  (progn
    (setq twittering-use-master-password t)
    (add-hook 'twittering-mode-hook 'init-whitespace-disable)))
