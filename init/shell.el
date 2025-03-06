;;; -*- lexical-binding: t -*-

(require 'init-utils)

(use-package comint
  :ensure nil
  :hook (comint-output-filter-functions . comint-osc-process-output)
  :init (setq comint-input-ring-separator "\n\\(: \\([0-9]+\\):\\([0-9]+\\);\\)?"))

(use-package envrc
  :demand
  :if (executable-find "direnv")
  :hook (after-init-hook . envrc-global-mode))

(use-package nushell-ts-mode
  :init
  (progn
    (add-to-list 'interpreter-mode-alist '("nu" . nushell-ts-mode))))

(use-package shell
  :ensure nil
  :hook (shell-mode-hook . compilation-shell-minor-mode)
  :bind (:map shell-mode-map
              ("SPC" . comint-magic-space))
  :custom
  (shell-highlight-undef-enable t)
  :init
  (progn
    (let ((hist (init-xdg-data "zsh/history")))
      (when (and (file-exists-p hist)
                 (not (getenv "HISTFILE")))
        (setenv "HISTFILE" hist)
        (setenv "HISTSIZE" "4096")))))

(use-package vterm
  :ensure nil
  :custom
  (vterm-shell "nu"))
