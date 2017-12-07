;;; -*- lexical-binding: t -*-

(defconst init-nixos (file-exists-p "/run/nix")
  "Indicates if on NixOS.")

(use-package company-nixos-options
  :if init-nixos
  :hook (nix-mode-hook . init-company-nixos-options)
  :init
  (progn
    (defun init-company-nixos-options ()
      (add-to-list (make-local-variable 'company-backends)
                   #'company-nixos-options))))

(use-package nix-mode)

(use-package nix-sandbox)
