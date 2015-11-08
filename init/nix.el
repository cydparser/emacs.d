(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-nixos-options))

(defun init-nix-mode ()
  (company-mode))

(add-hook 'nix-mode-hook 'init-nix-mode)
