(use-package tex-mode
  :defer t
  :init
  (progn
    (add-hook 'tex-mode-hook #'prettify-symbols-mode)))
