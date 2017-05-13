(defconst init-nixos (file-exists-p "/run/current-system/sw")
  "Indicates if on NixOS.")

(use-package company-nixos-options
  :defer t
  :if init-nixos
  :init
  (progn
    (defun init-company-nixos-options ()
      (add-to-list (make-local-variable 'company-backends) #'company-nixos-options))

    (add-hook 'nix-mode-hook #'init-company-nixos-options)))

(use-package nix-mode
  :defer t)
