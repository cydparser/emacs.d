(when (executable-find "nixos-option")
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-nixos-options))
  (add-hook 'nix-mode-hook 'company-mode))
